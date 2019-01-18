#lang racket/base

(require racket/class
         racket/draw
         racket/draw/arrow
         racket/string
         "config.rkt"
         "struct.rkt")

(provide draw-all render-tline)

(define (connect dc source-bounds self? is-target-inner? target-bounds
                 [right-to-left? #f][hilite? #f])
  (send dc set-pen (if hilite? hilite-pen line-pen))

  (let ([source-x (display-bound-x source-bounds)]
        [source-y (display-bound-y source-bounds)]
        [source-w (display-bound-w source-bounds)]
        [source-h (display-bound-h source-bounds)]

        [target-x (display-bound-x target-bounds)]
        [target-y (display-bound-y target-bounds)]
        [target-w (display-bound-w target-bounds)]
        [target-h (display-bound-h target-bounds)])
    (if (not self?)
        (let ([start-x (if right-to-left? source-x (+ source-x source-w -20))]
              [start-y (+ source-y source-h -20)]
              [end-x (if right-to-left? (+ target-x target-w) target-x)]
              [end-y (+ target-y (/ target-h 2))])
          (draw-arrow dc
                      start-x start-y
                      end-x end-y
                      1 1))
        ;; self
        (let* ([start-x (+ source-x source-w (- TGAP))]
               [start-y (+ source-y source-h (- TGAP))]

               [end-x (if is-target-inner?
                          (- (+ target-x target-w) TGAP)
                          (+ target-x target-w))]
               [end-y (if is-target-inner? (+ target-y (/ target-h 2)) target-y)]

               [mid-x (+ end-x SPLINE-SHIFT)]
               [mid-y (/ (+ start-y end-y) 2)])

          (send dc draw-ellipse
                (- start-x 4) (- start-y 4)
                8 8)
          (send dc draw-spline
                start-x start-y
                mid-x
                mid-y
                end-x end-y)
          ;; draw an arrow head
          (send dc draw-polygon
                (list (make-object point% (- end-x 4) end-y)
                      (make-object point% (+ end-x 4) (- end-y 4))
                      (make-object point% (+ end-x 4) (+ end-y 5))))))))

(define (draw-trace dc t bounds hilites pinned-trace)
  (send dc set-pen box-pen)
  (define label (trace-label t))
  (define hilite? (for/or ([h (in-list hilites)])
                    (or (equal? h label)
                        (and (trace-inner-loop t)
                             (equal? h
                                     (trace-label
                                      (trace-inner-loop t)))))))
  (if (or hilite? (eq? t pinned-trace))
      (send dc set-brush tracebox-highlight-brush)
      (send dc set-brush tracebox-brush))
  (define current-x (display-bound-x bounds))
  (define current-y (display-bound-y bounds))
  (define current-w (display-bound-w bounds))
  (define current-h (display-bound-h bounds))
  (send dc draw-rounded-rectangle
        current-x
        current-y
        current-w
        current-h)

  #;(define-values (w h d a) (send dc get-text-extent s trace-title-font #t))
  (send dc draw-text label (+ current-x TGAP) (+ current-y TGAP) #t)

  (when (trace-inner-loop t)
    (send dc set-brush trace-innerbox-brush)
    (send dc draw-rounded-rectangle
          (+ current-x GAP) (+ current-y (/ t-height 3))
          (- t-width TGAP) (- (* t-height 2/3) GAP))
    (send dc draw-text (trace-label (trace-inner-loop t))
          (+ current-x TGAP)
          (+ current-y (/ t-height 3) TGAP))
    )
  )

(define (draw-bridge dc b bounds hilites pinned-trace)
  (send dc set-pen box-pen)
  (define hilite? (for/or ([h (in-list hilites)])
                    (equal? (bridge-guard-id b) h)))

  (define current-x (display-bound-x bounds))
  (define current-y (display-bound-y bounds))

  (if (or hilite? (eq? b pinned-trace))
      (send dc set-brush bridgebox-highlight-brush)
      (send dc set-brush bridgebox-brush))

  (send dc draw-rounded-rectangle
        current-x current-y
        (display-bound-w bounds)
        (display-bound-h bounds))

  (send dc draw-text "bridge for" (+ current-x TGAP) (+ current-y TGAP) #t)
  (send dc draw-text (bridge-guard-id b) (+ current-x TGAP) (+ current-y TGAP TGAP GAP) #t))

(define (draw-connections dc trace-jumps bridges guard-exits inner-loop-of labeled-bounds-ht hilites)

  (define (get-display-bound label)
    (or (hash-ref labeled-bounds-ht label #f)
        (hash-ref labeled-bounds-ht
                  (hash-ref inner-loop-of label))))

  (define (is-hilite? . labels)
    (for/or ([l (in-list labels)])
      (for/or ([h (in-list hilites)])
        (equal? h l))))

  ;; Draw trace to trace jumps
  (for ([(t target-label) (in-hash trace-jumps)])
    (define self-display-bounds (get-display-bound (trace-label t)))
    (define target-display-bounds (get-display-bound target-label))

    (connect dc
             self-display-bounds
             (equal? self-display-bounds target-display-bounds)
             (hash-has-key? inner-loop-of target-label)
             target-display-bounds
             #f (is-hilite? (trace-label t) target-label)))

  ;; Draw trace to bridge jumps (guard exits)
  (for ([(t-b bridges) (in-hash guard-exits)])
    (unless (null? bridges)
      (for ([b (in-list bridges)])
        (define t-b-label (if (trace? t-b)
                              (trace-label t-b)
                              (bridge-guard-id t-b)))
        ;; connect t with b
        (define self-display-bounds (get-display-bound
                                     t-b-label))
        (define bridge-display-bounds (get-display-bound b))

        (connect dc self-display-bounds (bridge? t-b)
                 #f bridge-display-bounds
                 #f (is-hilite? t-b-label b)))))

  ;; Draw bridge to trace jumps
  (for ([b (in-list bridges)])
    (define bridge-bounds (get-display-bound (bridge-guard-id b)))
    (define target-bounds (get-display-bound (bridge-jump-target b)))

    (connect dc
             bridge-bounds
             #f
             (hash-has-key? inner-loop-of target-bounds)
             target-bounds
             #t
             (is-hilite? (bridge-guard-id b) (bridge-jump-target b)))))

(define (render-tline dc tline y jump-target)
  (send dc set-font t-font)
  (send dc set-text-foreground "black")
  (cond
    [(info-tline? tline)
     (let ([s (info-tline-line-str tline)])
       (send dc set-font secondary-t-font)
       (define-values (w h d a) (send dc get-text-extent s))
       (send dc draw-text s 0 y #t)
       (values (+ y h GAP) w))]
    [(param-tline? tline)
     (define-values (w h d a) (send dc get-text-extent "["))
     (send dc draw-text "[" INDENT y #t)
     (define wp
       (for/fold ([w (+ INDENT w GAP)])
                 ([p (in-list (param-tline-params tline))])
         (define-values (wp hp dp ap) (send dc get-text-extent p))
         (send dc draw-text p w y #t)
         (+ w wp 10)))
     (send dc draw-text "]" wp y #t)
     (values (+ y h GAP) (+ wp 20))]
    [(debug-merge-point? tline)
     (let ([s (debug-merge-point-code tline)])
       (send dc set-font secondary-t-font)
       (define-values (w h d a) (send dc get-text-extent s))
       (send dc draw-text s 0 y #t)
       (values (+ y h GAP) w))]
    [(guard? tline)
     ;; FIXME : this part could use a good refactoring
     (let ([s (format "~a ( ~a )" (guard-type tline)
                      (string-join (guard-args tline) " "))])
       (send dc set-text-foreground "red")
       (define-values (w-guard h d a) (send dc get-text-extent s))
       (send dc draw-text s INDENT y #t)
       (define w-extra INDENT)
       (when (guard-bridge? tline)
         (let ([s "show bridge"])
           (send dc set-text-foreground "blue")
           (define-values (w-bridge h d a) (send dc get-text-extent s))
           (set! w-extra (+ w-extra w-bridge))
           (send dc draw-text s (+ w-guard INDENT GAP) y #t)

           (let ([s "(run N/A times, ~N/A%)"])
             (send dc set-font secondary-t-font)
             (send dc set-text-foreground "black")
             (define-values (w-times h d a) (send dc get-text-extent s))
             (set! w-extra (+ w-extra w-times))
             (send dc draw-text s (+ w-guard GAP INDENT w-bridge GAP) y #t))))

       (values (+ y h GAP) (+ w-guard w-extra)))]
    [(assignment-tline? tline)
     (let ([s "4 = 2 + 2"])
       (define-values (w h d a) (send dc get-text-extent s))
       (send dc draw-text s 0 y #t)
       (values (+ y h GAP) w))]
    [(operation-tline? tline)
     (let ([s "operation cwal"])
       (define-values (w h d a) (send dc get-text-extent s))
       (send dc draw-text s 0 y #t)
       (values (+ y h GAP) w))]
    [else
     (error 'render-tline (format "this is not a tline : ~a\n" tline))]))


;;;; MAIN DRAW

(define (draw-all dc
                  #:traces traces
                  #:bridges bridges
                  #:inner-loop-of inner-loop-of
                  #:labeled-bounds-ht labeled-bounds-ht
                  #:display-bounds-ht display-bounds-ht
                  #:view-scale view-scale
                  #:hilites hilites
                  #:pinned-trace pinned-trace
                  #:trace-jumps trace-jumps
                  #:guard-exits guard-exits)
  (send dc set-font font)
  (send dc set-text-foreground "black")
  (send dc set-smoothing 'aligned)
  (send dc set-scale view-scale view-scale)

  (for ([(t-b bounds) (in-hash display-bounds-ht)])
    (if (trace? t-b)
        (draw-trace dc t-b bounds hilites pinned-trace)
        (draw-bridge dc t-b bounds hilites pinned-trace)))

  (draw-connections dc trace-jumps bridges  guard-exits inner-loop-of labeled-bounds-ht hilites)

  )

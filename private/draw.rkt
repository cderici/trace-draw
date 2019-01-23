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
                  (hash-ref inner-loop-of label #f) #f)))

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

    (when (and bridge-bounds target-bounds)
      (connect dc
               bridge-bounds
               #f
               (hash-has-key? inner-loop-of target-bounds)
               target-bounds
               #t
               (is-hilite? (bridge-guard-id b) (bridge-jump-target b))))))

(define (render-regular dc str x y [text-color tline-color])
  (send dc set-font t-font)
  (send dc set-text-foreground text-color)
  #;(define-values (w _ __ ___) (send dc get-text-extent str))
  (send dc draw-text str x y #t))

(define (render-params dc x l-paren r-paren y params pinned-param hilite-param hbounds
                       [text-color tline-color])
  (define param-bounds hbounds)
  (define-values (paren-w _ __ ___) (send dc get-text-extent l-paren))
  (define-values (comma-ws _2 __2 ___2) (send dc get-text-extent ", "))
  (unless param-bounds
    ;; FIXME : merge this with drawing below
    (define-values (bounds last-x)
      (for/fold ([hbounds (hash)]
                 [current-x (+ paren-w x)])
                ([p (in-list params)])
        (define-values (w h d a) (send dc get-text-extent p))
        (let ([p-end-x (+ current-x w)])
          (values (hash-set hbounds p (cons current-x p-end-x))
                  (+ p-end-x comma-ws)))))
    (set! param-bounds bounds))
  (send dc set-text-foreground text-color)
  (define first? #t)
  (define start-x x)
  (render-regular dc l-paren start-x y text-color)
  (set! start-x (+ start-x paren-w))
  (for ([p-str (in-list params)])
    (define-values (p-w _3 __3 ___3) (send dc get-text-extent p-str))
    (if first? (set! first? #f)
        (let ()
          (send dc draw-text ", " start-x y #t)
          (set! start-x (+ start-x comma-ws))))
    (when (or (equal? pinned-param p-str)
              (equal? hilite-param p-str))
      (define-values (x-l x-r)
        (let ([b (hash-ref param-bounds p-str)]) (values (car b) (cdr b))))
      (send dc set-brush tline-highlight-brush)
      (send dc draw-rounded-rectangle
            (- start-x GAP) y (+ p-w TGAP) (- TLINE-H GAP)))
    (send dc draw-text p-str start-x y #t)
    (set! start-x (+ start-x p-w)))
  (render-regular dc r-paren start-x y text-color)
  (values (+ start-x paren-w) param-bounds))

(define (render-tline dc tline tline-# hover-tline hilite-param pinned-param lbl->counts)
  (send dc set-font t-font)
  (send dc set-text-foreground tline-color)
  (define hover? (eq? tline hover-tline))
  (define y (* tline-# TLINE-H))

  (cond
    [(info-tline? tline)
     (let ([s (info-tline-line-str tline)])
       (send dc set-font secondary-t-font)
       (define-values (w h d a) (send dc get-text-extent s))
       (send dc draw-text s 0 y #t)
       w)]
    [(param-tline? tline)
     (define-values (next-x param-bounds)
       (render-params dc INDENT "[" "]" y
                      (param-tline-params tline) pinned-param hilite-param
                      (param-tline-hbounds tline)))
     (unless (param-tline-hbounds tline)
       (set-param-tline-hbounds! tline param-bounds))
     next-x]
    [(debug-merge-point? tline)
     (let ([s (string-append "> " (debug-merge-point-code tline))])
       (send dc set-font secondary-t-font)
       (define-values (w h d a) (send dc get-text-extent s))
       (send dc draw-text s 0 y #t)
       w)]
    [(guard? tline)
     (define s (guard-type tline))
     (send dc set-text-foreground "red")
     (send dc draw-text s INDENT y #t)
     (define-values (w h d a) (send dc get-text-extent s))
     (define-values (next-x param-bounds)
       (render-params dc (+ w INDENT) "(" ")" y
                      (guard-args tline) pinned-param hilite-param
                      (guard-hbounds tline) "red"))
     (define init? #f)
     (unless (guard-hbounds tline)
       (set! init? #t)
       (set-guard-hbounds! tline param-bounds))
     (define start-x next-x)
     (when (guard-bridge? tline)
       (let ([s "show bridge"])
         (send dc set-font tb-font)
         (send dc set-text-foreground "blue")
         (define-values (w-bridge h d a) (send dc get-text-extent s))
         (set! start-x (+ start-x TGAP))
         (define s-id (string-append s (guard-id tline)))
         (when (or (equal? pinned-param s-id)
                   (equal? hilite-param s-id))
           (send dc set-font show-bridge-underline-font))
         (send dc draw-text s start-x y #t)
         (when init?
           (set-guard-hbounds!
            tline (hash-set (guard-hbounds tline)
                            s-id (cons start-x (+ start-x w-bridge)))))
         (set! start-x (+ start-x w-bridge))
         (let* ([cnt (hash-ref lbl->counts (guard-id tline) #f)]
                [% (and cnt
                        (let* ([guard-owner-lbl (guard-belongs-to tline)]
                               [surrounding-run-count (hash-ref lbl->counts guard-owner-lbl #f)])
                          (and surrounding-run-count
                               (floor (inexact->exact (/ (* cnt 100) surrounding-run-count))))))]
                [s (format "(run ~a times, ~~~a%)" (or cnt "N/A") (or % "N/A"))])
           (send dc set-font secondary-t-font)
           (send dc set-text-foreground "black")
           (define-values (w-times h d a) (send dc get-text-extent s))
           (set! start-x (+ start-x TGAP))
           (send dc draw-text s start-x y #t)
           (set! start-x (+ start-x w-times)))))
     start-x]
    [(assignment-tline? tline)
     (let ([lhs (assignment-tline-lhs tline)]
           [op (assignment-tline-op tline)]
           [args (assignment-tline-args tline)]
           [hbounds (assignment-tline-hbounds tline)]
           [start-x INDENT])
       (define lhs-str (string-append lhs " = "))
       (define-values (lhs-w _ __ ___) (send dc get-text-extent lhs))
       (define-values (lhs-str-w _2 __2 ___2) (send dc get-text-extent lhs-str))
       (define lhs-start-x start-x)
       (when (or (equal? pinned-param lhs)
                 (equal? hilite-param lhs))
         (send dc set-brush tline-highlight-brush)
         (send dc draw-rounded-rectangle
               (- start-x GAP) y (+ lhs-w TGAP) (- TLINE-H GAP)))
       (send dc draw-text lhs-str start-x y #t)
       (set! start-x (+ start-x lhs-str-w))
       (define-values (op-w op-h op-d op-a) (send dc get-text-extent op))
       (send dc draw-text op start-x y #t)
       (set! start-x (+ start-x op-w))
       (define-values (next-x param-bounds)
         (render-params dc start-x "(" ")" y
                        args pinned-param hilite-param hbounds))
       (unless hbounds
         (set-assignment-tline-hbounds!
          tline
          (hash-set param-bounds lhs (cons lhs-start-x (+ lhs-start-x lhs-w)))))
       next-x)]
    [(operation-tline? tline)
     (let* ([op (operation-tline-op tline)]
            [args (operation-tline-args tline)]
            [label? (equal? op "label")]
            [jump? (equal? op "jump")]
            [hbounds (operation-tline-hbounds tline)]
            [start-x INDENT])
       (define color tline-color)
       (when (or label? jump?)
         (set! color "blue"))
       (send dc set-text-foreground color)
       (define-values (op-w op-h op-d op-a) (send dc get-text-extent op))
       (send dc draw-text op start-x y #t)
       (set! start-x (+ start-x op-w))
       (define-values (next-x param-bounds)
         (render-params dc start-x "(" ")" y
                        args pinned-param hilite-param hbounds color))
       (unless hbounds
         (set-operation-tline-hbounds! tline param-bounds))
       next-x)]
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

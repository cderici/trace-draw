#lang racket/base

(require racket/class
         racket/draw
         racket/draw/arrow
         racket/string
         "config.rkt"
         "struct.rkt"
         "util.rkt")

(provide draw-all
         compute-tline-positions-and-dimensions
         render-tlines
         render-hilites)

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

    (unless (or (not self-display-bounds)
                (not target-display-bounds))
      ;; These happen because of selecting only a few mostly used
      ;; traces/bridges, i.e. a target jump of a selected trace/bridge
      ;; may not be among the selected ones.
      (connect dc
               self-display-bounds
               (equal? self-display-bounds target-display-bounds)
               (hash-has-key? inner-loop-of target-label)
               target-display-bounds
               #f (is-hilite? (trace-label t) target-label))))

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

    (unless (or (not self-display-bounds)
                (not bridge-display-bounds))
      (connect dc self-display-bounds (bridge? t-b)
               #f bridge-display-bounds
               #f (is-hilite? t-b-label b))))))

  ;; Draw bridge to trace jumps
  (for ([b (in-list bridges)])
    (define bridge-bounds (get-display-bound (bridge-guard-id b)))
    (define target-bounds (get-display-bound (bridge-jump-target b)))

    (unless (or (not bridge-bounds)
                (not target-bounds))
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

;; returns (or void (hilitable-param-positions-hash current-x))
;; if positions not #f, then it renders, otherwise just computes the
;; positions
(define (compute/render-params dc params x y l-paren r-paren [positions #f] [text-color tline-color])
    (if positions
        (let ([number-of-params (length params)])
          (render-regular dc l-paren (hash-ref positions l-paren) (hash-ref positions "y") text-color)

          (for/fold ([p-count 1])
                    ([p (in-list params)])
            (let ([param-display-bounds (hash-ref positions p)])
              (let ([x (display-bound-x param-display-bounds)]
                    [y (display-bound-y param-display-bounds)]
                    [w (display-bound-w param-display-bounds)]
                    [h (display-bound-h param-display-bounds)])
                (send dc draw-text p x y #t)
                (when (< p-count number-of-params)
                  (send dc draw-text ", " (+ x w) y #t))
                (add1 p-count))))
          (render-regular dc r-paren (hash-ref positions r-paren) (hash-ref positions "y") text-color))
        (let-values ([(l-paren-w lp_ lp__ lp___) (send dc get-text-extent l-paren)]
                     [(r-paren-w rp_ rp__ rp___) (send dc get-text-extent r-paren)]
                     [(comma-ws cw_ cw__ cw___) (send dc get-text-extent ", ")]
                     [(number-of-params) (length params)]
                     [(paren-gap) 2])
          (let-values ([(hoverable-positions all-positions hilite-rectangle-positions final-x param-h _)
                        (for/fold ([hoverable-positions null]
                                   [all-positions (hash "y" y l-paren x)]
                                   [param-rectangle-positions (hash)]
                                   [current-x (+ x (+ l-paren-w paren-gap))]
                                   [param-h 0]
                                   [p-count 1])
                                  ([p (in-list params)])
                          (define-values (w h d a) (send dc get-text-extent p))
                          (values
                           (cons (vector p current-x (+ current-x w)) hoverable-positions)
                           (hash-set all-positions p (display-bound current-x y w h))
                           (hash-set param-rectangle-positions p (display-bound current-x y w h))
                           (if (< p-count number-of-params) (+ current-x w comma-ws) (+ current-x w paren-gap)) ; the next x is (current-x + param-width + ", ")
                           h
                           (add1 p-count)))])
            (values hoverable-positions
                    (hash-set
                     (hash-set all-positions r-paren final-x) "current-y" (+ y param-h))
                    hilite-rectangle-positions
                    (+ final-x r-paren-w))))))

(define (compute-tline-positions-and-dimensions dc tlines hide-debug-merge-points? lbl->counts)
  ;; hoverable-positions : will be used to detect what the mouse is hovering over
  ;; hilite-rectangle-positions : will be used to draw the hilite rectangles for hilited name
  ;; tline-positions : will be used to draw the tlines
  (for/fold ([hoverable-positions (hash)] ;; -> (hash tline (listof (vector string number number)))
             [hilite-rectangle-positions (hash)] ;; -> (hash "str" (listof display-bounds))
             [tline-positions (hash)] ;; -> (hash tline <everything needed to draw the tline>)
             [max-w 0]
             [current-h 0])
            ([tline (in-list tlines)])
    (cond
      [(info-tline? tline)
       (let ([s (info-tline-line-str tline)])
         #;(send dc set-font secondary-t-font)
         (define-values (w h d a) (send dc get-text-extent s))
         (values hoverable-positions
                 hilite-rectangle-positions
                 (hash-set tline-positions tline (display-bound 0 current-h w h))
                 (max max-w w)
                 (+ current-h h LINE-GAP)))]
      [(param-tline? tline)
       (define-values (param-hilitable-xs all-positions param-rectangle-positions current-x)
         (compute/render-params dc
                                (param-tline-params tline)
                                INDENT
                                current-h "[" "]"))
       (values (hash-set hoverable-positions tline param-hilitable-xs)
               (append-hash-table param-rectangle-positions hilite-rectangle-positions)
               (hash-set tline-positions tline all-positions)
               current-x
               (+ (hash-ref all-positions "current-y") LINE-GAP))]
      [(debug-merge-point? tline)
       (if hide-debug-merge-points?
           ;; we're just ignoring debug-merge-points if we're hiding
           ;; them, so all the positions of the rest of tlines is
           ;; gonna be computed naturally, but this means when we want
           ;; to "Hide Interpreted Codes" we need to recompute the
           ;; positions all over again

           ;; another idea would be, instead of doing that, to record
           ;; how many debug-merge-points are there above each tline,
           ;; so to hide them we only need to subtract from each tline
           ;; -> (* line-height #-of-debug-points-above) ... note that
           ;; we also need to shift the hilites
           ;; note that this might also confuse the mouse hover computation
           (values hoverable-positions
                   hilite-rectangle-positions
                   tline-positions
                   max-w current-h)
           (let ([s (string-append "> " (debug-merge-point-code tline))])
             #;(send dc set-font secondary-t-font)
             (define-values (w h d a) (send dc get-text-extent s))
             (values hoverable-positions
                     hilite-rectangle-positions
                     (hash-set tline-positions
                               tline
                               (cons s (display-bound 0 current-h w h)))
                     (max max-w w)
                     (+ current-h h LINE-GAP))))]
      [(guard? tline)
       (define guard-name (guard-type tline))
       (define-values (guard-name-w h d a) (send dc get-text-extent guard-name))
       ; computing params first (adding guard-name is easy afterwards)
       ; note that start-x is (INDENT + guard-name-width)
       (define-values (gp-hoverable-positions all-positions params-rectangle-positions current-x-after-param)
         (compute/render-params dc
                                (guard-args tline)
                                (+ INDENT guard-name-w)
                                current-h "(" ")"))
       (define positions-with-guard-name
         (hash-set all-positions "guard-name" (display-bound INDENT current-h
                                                             guard-name-w h)))
       (define has-a-bridge? (guard-bridge? tline))
       (define-values (hoverables-with-show-bridge show-bridge-position rectangles-with-show-bridge current-x-after-show-bridge)
         (if has-a-bridge?
             (let-values ([(sb-w sb_ sb__ sb___)
                           (send dc get-text-extent "show bridge")])
               (let ([sb-position
                      (display-bound (+ current-x-after-param TGAP)
                                     current-h sb-w sb_)])
                 (values (cons (vector "show-bridge-hover" (+ current-x-after-param TGAP) (+ current-x-after-param TGAP sb-w))
                               gp-hoverable-positions)
                         sb-position
                         (append-hash-table
                          (hash-set params-rectangle-positions
                                    ;; there's always only one
                                    ;; show-bridge highlighted
                                    ;; (underlined) at any moment
                                    "show-bridge-hover"
                                    sb-position)
                          hilite-rectangle-positions)
                         (+ current-x-after-param TGAP sb-w))))
             (values gp-hoverable-positions
                     #f
                     (append-hash-table params-rectangle-positions hilite-rectangle-positions)
                     current-x-after-param)))

       (define positions-with-show-bridge
         (if has-a-bridge?
             (hash-set positions-with-guard-name "show-bridge" show-bridge-position)
             positions-with-guard-name))

       (define-values (positions-with-run-text current-x-after-run-times)
         (if has-a-bridge?
             (let* ([cnt (hash-ref lbl->counts (guard-id tline) #f)]
                    [% (and cnt
                            (let* ([guard-owner-lbl (guard-belongs-to tline)]
                                   [surrounding-run-count (hash-ref lbl->counts guard-owner-lbl #f)])
                              (and surrounding-run-count
                                   (floor (inexact->exact (/ (* cnt 100) surrounding-run-count))))))]
                    [s (format "(run ~a times, ~~~a%)" (or cnt "N/A") (or % "N/A"))])
               (define-values (w-times h d a) (send dc get-text-extent s))
               (values
                (hash-set
                 (hash-set positions-with-show-bridge
                           "run-text" s)
                 "run-text-position"
                 (display-bound (+ current-x-after-show-bridge TGAP)
                                current-h w-times h))
                (+ current-x-after-show-bridge TGAP w-times)))
             (values positions-with-show-bridge
                     current-x-after-show-bridge)))

       (values (hash-set hoverable-positions tline hoverables-with-show-bridge)
               rectangles-with-show-bridge
               (hash-set tline-positions tline positions-with-run-text)
               (max max-w current-x-after-run-times TGAP)
               (+ current-h h LINE-GAP))]
      [(assignment-tline? tline)
       (let ([lhs (assignment-tline-lhs tline)]
             [op (assignment-tline-op tline)]
             [args (assignment-tline-args tline)])
         (define-values (lhs-w lhs-h __2 ___2) (send dc get-text-extent lhs))
         (define lhs-display-bound (display-bound INDENT current-h lhs-w lhs-h))

         (define hoverables-with-lhs
           (list (vector lhs INDENT (+ INDENT lhs-w))))
         (define rectangles-with-lhs
           (cons-hash-table lhs lhs-display-bound hilite-rectangle-positions))

         (define-values (=-w _ __ ___) (send dc get-text-extent " = "))
         (define current-x-after-= (+ INDENT lhs-w =-w))

         (define-values (op-w op_ op__ op___) (send dc get-text-extent op))
         (define current-x-after-op (+ current-x-after-= op-w))
         (define op-display-bound
           (display-bound current-x-after-= current-h op-w op_))

         (define hoverables-with-op
           (cons (vector op current-x-after-= current-x-after-op) hoverables-with-lhs))
         (define rectangles-with-op ;; let's make the ops highligted too
           (cons-hash-table op op-display-bound rectangles-with-lhs))

         (define-values (ass-hoverable-params param-positions param-rectangles current-x-after-params)
           (compute/render-params dc args current-x-after-op current-h "(" ")"))

         (values (hash-set hoverable-positions tline
                           (append hoverables-with-op ass-hoverable-params))
                 (append-hash-table param-rectangles rectangles-with-op)
                 (hash-set tline-positions tline
                           (hash-set
                            (hash-set param-positions
                                      "lhs" lhs-display-bound)
                            "op" op-display-bound))
                 (max max-w current-x-after-params)
                 (+ current-h lhs-h LINE-GAP)))]
      [(operation-tline? tline)
       (let* ([op (operation-tline-op tline)]
              [args (operation-tline-args tline)])

         (define-values (op-w op-h op__ op___) (send dc get-text-extent op))
         (define current-x-after-op (+ INDENT op-w))
         (define op-display-bound
           (display-bound INDENT current-h op-w op-h))

         (define hoverables-with-op
           (list (vector op INDENT current-x-after-op)))
         (define rectangles-with-op ;; let's make the ops highligted too
           (cons-hash-table op op-display-bound hilite-rectangle-positions))

         (define-values (op-hoverable-params param-positions param-rectangles current-x-after-params)
           (compute/render-params dc args current-x-after-op current-h "(" ")"))

         (values (hash-set hoverable-positions tline
                           (append hoverables-with-op op-hoverable-params))
                 (append-hash-table param-rectangles rectangles-with-op)
                 (hash-set tline-positions tline
                           (hash-set param-positions "op" op-display-bound))
                 (max max-w current-x-after-params)
                 (+ current-h op-h LINE-GAP)))]
      [else
       (error 'compute-tline-positions-and-dimensions (format "this is not a tline : ~a\n" tline))])))

(define (render-tlines dc tlines tline-positions no-debug-tlines?)
  (send dc set-font t-font)
  (for ([tline (in-list tlines)])
    (send dc set-text-foreground tline-color)
    (let ([line-info (hash-ref tline-positions tline #f)])
      (when (and (not (debug-merge-point? tline)) (not line-info))
        (error 'render-tlines "no pre-computed information for : ~a" tline))
      (cond
        [(info-tline? tline)
         (let ([str (info-tline-line-str tline)]
               [db line-info])
           (send dc draw-text str (display-bound-x db) (display-bound-y db) #t))]
        [(param-tline? tline)
         (compute/render-params dc (param-tline-params tline) 'dummy 'dummy "[" "]" line-info)]
        [(debug-merge-point? tline)
         (unless no-debug-tlines?
           (let ([str (car line-info)]
                 [db (cdr line-info)])
             (send dc draw-text str (display-bound-x db) (display-bound-y db) #t)))]
        [(guard? tline)
         (let ([name (guard-type tline)]
               [args (guard-args tline)]
               [bridge? (guard-bridge? tline)]
               [guard-name-db (hash-ref line-info "guard-name")])
           (send dc set-text-foreground "red")
           (send dc draw-text name (display-bound-x guard-name-db) (display-bound-y guard-name-db) #t)
           (compute/render-params dc args 'dummy 'dummy "(" ")" line-info "red")
           (when bridge?
             (let ([sb-db (hash-ref line-info "show-bridge")])
               (send dc set-text-foreground "blue")
               (send dc draw-text "show bridge" (display-bound-x sb-db) (display-bound-y sb-db) #t))
             (let ([run-text (hash-ref line-info "run-text")]
                   [run-text-db (hash-ref line-info "run-text-position")])
               (send dc set-text-foreground tline-color)
               (send dc draw-text run-text (display-bound-x run-text-db) (display-bound-y run-text-db) #t))))]
        [(assignment-tline? tline)
         (let ([lhs (assignment-tline-lhs tline)]
               [op (assignment-tline-op tline)]
               [args (assignment-tline-args tline)])
           (let ([lhs-db (hash-ref line-info "lhs")]
                 [op-db (hash-ref line-info "op")])
             (send dc draw-text (string-append lhs " = ") (display-bound-x lhs-db) (display-bound-y lhs-db) #t)
             (send dc draw-text op (display-bound-x op-db) (display-bound-y op-db) #t)
             (compute/render-params dc args 'dummy 'dummy "(" ")" line-info)))]
        [(operation-tline? tline)
         (let ([op (operation-tline-op tline)]
               [args (operation-tline-args tline)])
           (let ([op-db (hash-ref line-info "op")]
                 [color (if (or (equal? op "jump") (equal? op "label")) "blue" tline-color)])
             (send dc set-text-foreground color)
             (send dc draw-text op (display-bound-x op-db) (display-bound-y op-db) #t)
             (compute/render-params dc args 'dummy 'dummy "(" ")" line-info color)))]
        [else
         (error 'render-tlines (format "this is not a tline : ~a\n" tline))]))))

(define (render-hilite dc rectangle-positions)
  (send dc set-brush tline-highlight-brush)
  (for ([db (in-list rectangle-positions)])
    (send dc draw-rounded-rectangle
          (- (display-bound-x db) GAP)
          (display-bound-y db)
          (+ (display-bound-w db) TGAP)
          (display-bound-h db))))

(define (render-hilites dc hilite-param pinned-param hilite-rectangle-positions)
  ;; hilite-param
  (when hilite-param
    (let ([rectangle-positions (hash-ref hilite-rectangle-positions hilite-param)])
      (render-hilite dc rectangle-positions)))
  ;; pinned-param
  (when pinned-param
    (let ([rectangle-positions (hash-ref hilite-rectangle-positions pinned-param)])
      (render-hilite dc rectangle-positions))))


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

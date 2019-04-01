#lang racket/base

(require racket/gui/base
         racket/class
         racket/string
         racket/path
         "draw.rkt"
         "struct.rkt"
         "config.rkt"
         "jump-deps.rkt"
         "process.rkt")

(provide make-gui)
(define (->int n) (ceiling (inexact->exact n)))

(define (make-gui #:traces trace-lines
                  #:bridges bridge-lines
                  #:max-trace-shown max-trace-shown
                  #:jit-summary jit-summary
                  #:jit-counts jit-count-lines
                  #:trace-file-name trace-file)

  (define trace-candidates (pre-process-trace-lines trace-lines))
  (define bridge-candidates (pre-process-bridge-lines bridge-lines))

  ; mapping from numbers (counts) -> trace-labels
  ; and   ; trace-labels -> counts
  (define-values (jit-counts labeled-counts)
    (process-jit-counts jit-count-lines))

  (define no-count? (null? jit-count-lines))

  (define traces
    (pick-most-used-traces trace-candidates
                           jit-counts labeled-counts max-trace-shown
                           bridge-candidates
                           no-count?))

  (define bridges (pick-bridges-for traces bridge-candidates labeled-counts))

  (define pinned-trace #f)
  (define hover #f)
  (define hilites null)

  (define-values (screen-w screen-h) (get-display-size))

  (define y-init 50)
  (define x-entry 100)
  (define x-trace (+ x-entry t-width X-GAP))
  (define x-bridge (+ x-trace t-width X-GAP))

  (define total-w (+ x-bridge X-GAP))  ;(* 2 (- screen-w 400))
  (define total-h
    (+ y-init
       (max (* (length traces) (+ t-height Y-GAP))
            (* (length bridges) (+ b-height Y-GAP))))) ; #;(* 2 (- screen-h 400))

  (define f (new frame%
                 [label (format "Trace Draw : ~a" (file-name-from-path trace-file))]
                 [width (- screen-w 100)]
                 [height (- screen-h 100)]))

  (define main-panel
    (new vertical-panel%
         [parent f]
         [alignment '(left top)]))

  (define panel
    (new horizontal-panel%
         [parent main-panel]
         [alignment '(left center)]))

  (define below-panel
    (new horizontal-panel%
         [parent main-panel]
         [stretchable-height #f]
         [alignment '(center center)]))

  (define below-message
    (new message%
         [parent below-panel]
         [label ""]))

  (define below-controls
    (new horizontal-panel%
         [parent below-panel]
         [alignment '(right center)]))

  (define no-debug-tlines-check
    (new check-box%
         [label "Hide Interpreted Codes"]
         [parent below-controls]
         [callback (lambda (cb ce)
                     (set! no-debug-tlines? (send cb get-value))
                     (set! tline-offscreen #f)
                     (set! refresh-tline-canvas? #t)
                     (send trace-info-canvas refresh))]))

  (define refresh-offscreen? #t)
  (define offscreen #f)
  (define offscreen-dc #f)
  (define view-scale 1)

  (define display-bounds-ht ; display locations for traces and bridges
    (let ()
      (define sorted-traces
        (if no-count?
            traces
            (sort traces (lambda (t1 t2)
                           (> (if (trace-inner-loop t1) (trace-use-count (trace-inner-loop t1)) (trace-use-count t1))
                              (if (trace-inner-loop t2) (trace-use-count (trace-inner-loop t2)) (trace-use-count t2)))))))
      (define-values (new-ht y-entry-next y-trace-next)
        (for/fold ([ht (hash)]
                   [y-entry y-init]
                   [y-trace y-init])
                  ([t (in-list sorted-traces)])
          (if (trace-is-entry? t)
              (values
               (hash-set ht t (make-display-bound
                               x-entry y-entry
                               t-width t-height))
               (+ y-entry t-height Y-GAP)
               y-trace)
              (values
               (hash-set ht t (make-display-bound
                               x-trace y-trace
                               t-width t-height))
               y-entry
               (+ y-trace t-height Y-GAP)))))
      (define-values (final-ht y-bridge-next)
        (for/fold ([ht new-ht]
                   [y-bridge y-init])
                  ([b (in-list bridges)])
          (values
           (hash-set ht b (make-display-bound
                           x-bridge y-bridge
                           b-width b-height))
           (+ y-bridge b-height Y-GAP))))
      final-ht))

  ;; same hash table with keys as labels instead of objects
  (define labeled-bounds-ht
    (for/hash ([(t-b b) (in-hash display-bounds-ht)])
      (if (trace? t-b)
          (values (trace-label t-b) b)
          (values (bridge-guard-id t-b) b))))

  ;; trace-jumps : mapping from traces/bridges to trace labels
  ;; guard-exits : mapping from traces to (listof bridge-guard-ids)
  ;; inner-loop-of : mapping from trace label to trace label (which is inner of which)
  (define-values (trace-jumps guard-exits inner-loop-of)
    (compute-jumps traces bridges))

  (define c (new (class canvas%
                   (super-new)
                   (inherit refresh
                            get-view-start
                            get-client-size
                            scroll
                            init-auto-scrollbars)
                   (define queued-refresh? #f)
                   (define/private (low-priority-refresh)
                     (unless queued-refresh?
                       (set! queued-refresh? #t)
                       (queue-callback
                        (lambda ()
                          (set! queued-refresh? #f)
                          (refresh))
                        #f)))
                   (define/private (adjust-scroll dx dy)
                     (define-values (w h) (get-client-size))
                     (define-values (x y) (get-view-start))
                     (define adj-w (- (* view-scale total-w) w))
                     (define adj-h (- (* view-scale total-h) h))
                     (define sx (min 1 (max 0 (+ (* dx 1/40) (/ x adj-w)))))
                     (define sy (min 1 (max 0 (+ (* dy 1/40) (/ y adj-h)))))
                     (define tx (and (not (= x (* sx adj-w)))
                                     sx))
                     (define ty (and (not (= y (* sy adj-h)))
                                     sy))
                     (when (or tx ty)
                       (scroll tx ty)))
                   (define/private (adjust-scale ds)
                     (define scale (max 1/5 (+ view-scale ds)))
                     (unless (= scale view-scale)
                       (set! view-scale scale)
                       (set! offscreen #f)
                       (set! refresh-offscreen? #t)
                       (low-priority-refresh)
                       (define-values (w h) (get-client-size))
                       (define-values (x y) (get-view-start))
                       (define adj-w (- (* view-scale total-w) w))
                       (define adj-h (- (* view-scale total-h) h))
                       (init-auto-scrollbars (->int (* scale total-w))
                                             (->int (* scale total-h))
                                             (min 1 (max 0 (/ x adj-w)))
                                             (min 1 (max 0 (/ y adj-h))))))
                   ;; KEY
                   (define/override (on-char e)
                     (for/or ([key-code (list (send e get-key-code)
                                              (send e get-other-shift-key-code)
                                              (send e get-other-altgr-key-code))])
                       (case key-code
                         [(wheel-up up) (adjust-scroll 0 (- scroll-speed)) #t]
                         [(wheel-down down) (adjust-scroll 0 scroll-speed) #t]
                         [(wheel-left left) (adjust-scroll (- scroll-speed) 0) #t]
                         [(wheel-right right) (adjust-scroll scroll-speed 0) #t]
                         [(#\+) (adjust-scale 1/10) #t]
                         [(#\-) (adjust-scale -1/10) #t]
                         [else #f])))

                   ;; MOUSE
                   (define prev-mouse-x -1)
                   (define prev-mouse-y -1)
                   (define (reset-mouse)
                     (set! prev-mouse-x -1)
                     (set! prev-mouse-y -1))
                   (define/override (on-event e)
                     (define-values (dx dy) (get-view-start))

                     (define mouse-x (send e get-x)) ; relative to the visible area
                     (define mouse-y (send e get-y))
                     (when (and (send e dragging?)
                                (send e get-left-down))
                       (define hor-dir (if (> mouse-x prev-mouse-x) 'right 'left))
                       (define ver-dir (if (> mouse-y prev-mouse-y) 'down 'up))

                       (when (or (> (abs (- mouse-x prev-mouse-x)) 20)
                                 (> (abs (- mouse-y prev-mouse-y)) 20))
                         (reset-mouse))
                       (unless (= prev-mouse-x -1)
                         (when (or (equal? hor-dir 'right) (equal? ver-dir 'down))
                           (adjust-scale 1/20))
                         (when (or (equal? hor-dir 'left) (equal? ver-dir 'up))
                           (adjust-scale -1/20)))
                       (set! prev-mouse-x mouse-x)
                       (set! prev-mouse-y mouse-y))

                     (define hover-trace
                       (for/or ([(t-b bounds) (in-hash display-bounds-ht)])
                         ; (define-values (w h) (get-client-size)) will be important when window resize
                         (define-values (x y) (get-view-start))
                         (let* ([x-left (- (* view-scale (display-bound-x bounds)) x)]
                                [y-top (- (* view-scale (display-bound-y bounds)) y)]
                                [x-right (+ x-left (* view-scale (display-bound-w bounds)))]
                                [y-bottom (+ y-top (* view-scale (display-bound-h bounds)))])
                           (and (mouse-x . >= . x-left)
                                (mouse-y . >= . y-top)
                                (mouse-x . <= . x-right)
                                (mouse-y . <= . y-bottom)
                                t-b))))
                     (when (send e leaving?)
                       (set! hover-trace #f)
                       (reset-hilites))
                     ;; setting a pinned-trace
                     (when (and hover-trace
                                (send e button-down?)
                                (not (equal? pinned-trace hover-trace)))
                       (set! pinned-trace hover-trace)
                       (when (= (send tpanel get-selection) 0)
                         (send tpanel delete-child infobox)
                         (send tpanel add-child trace-info-canvas)
                         (send tpanel set-selection 1))
                       (set! tline-offscreen #f)
                       (set! refresh-tline-canvas? #t)
                       (send trace-info-canvas refresh)
                       (reset-hilites)
                       (update-message-bar))
                     ;; unsetting a pinned-trace
                     (when (and (not hover-trace)
                                (send e button-down?))
                       (when (= (send tpanel get-selection) 1)
                         (set! refresh-tline-canvas? #t)
                         (send trace-info-canvas refresh))
                       (set! pinned-trace #f)
                       (set! refresh-offscreen? #t)
                       (update-message-bar)
                       (low-priority-refresh))
                     (unless (equal? hover hover-trace)
                       (set! hover hover-trace)
                       (reset-hilites)))
                   (define/public (reset-hilites)
                     (define hover-trace hover)
                     (set! hilites null)
                     (when hover-trace
                       (set! hilites (get-jump-deps hover-trace
                                                    trace-jumps
                                                    guard-exits)))
                     (unless (and hover-param-trace
                                  (memv hover-param-trace hilites))
                       (set! hilites (cons hover-param-trace hilites)))
                     (set! refresh-offscreen? #t)
                     (low-priority-refresh)))
                 [parent panel]
                 [style '(hscroll vscroll)]
                 [min-width (/ total-w 2)]
                 [paint-callback (lambda (c c-dc)
                                   (when refresh-offscreen?
                                     (unless offscreen
                                       (set! offscreen (send c make-bitmap
                                                             (->int (* view-scale total-w))
                                                             (->int (* view-scale total-h))))
                                       (set! offscreen-dc (send offscreen make-dc)))
                                     (set! refresh-offscreen? #f)

                                     (define dc offscreen-dc)
                                     (send dc clear)
                                     (send dc set-smoothing 'smoothed)

                                     (draw dc))
                                   (send c-dc draw-bitmap offscreen 0 0))]))

  (define (draw dc)
    (draw-all dc
              #:traces traces
              #:bridges bridges
              #:inner-loop-of inner-loop-of
              #:labeled-bounds-ht labeled-bounds-ht
              #:display-bounds-ht display-bounds-ht
              #:view-scale view-scale
              #:hilites hilites
              #:pinned-trace pinned-trace
              #:trace-jumps trace-jumps
              #:guard-exits guard-exits))

  (send c init-auto-scrollbars
        (->int (* view-scale total-w))
        (->int (* view-scale total-h))
        0.0 0.0)

  (define summary
    (let ([from-log-file (string-join (map string-trim jit-summary) "\n")])
      (if (null? jit-summary)
          (format "No jit-summary is provided in the input : \"~a\"

Consider using PYPYLOG=jit-summary...\n" trace-file)
          from-log-file)))

  (define vpanel (new vertical-panel%
                      [parent panel]
                      [alignment '(left top)]))

  (define hpanel (new horizontal-panel%
                      [parent vpanel]
                      [min-height 34]
                      [stretchable-height #f]
                      [alignment '(right top)]))

  (define left-h-panel (new horizontal-panel%
                            [parent hpanel]
                            [alignment '(left center)]))
  (define main-message
    (new message%
         [parent left-h-panel]
         [font main-msg-font]
         [label "Welcome to TraceDraw!"]))

  (define right-h-panel (new horizontal-panel%
                            [parent hpanel]
                            [alignment '(right center)]))

  (define history-pinned-trace '()) ; stack of visited pinned-traces

  (define back-button (new button%
                           [parent right-h-panel]
                           [label "Back"]
                           [style '(deleted)]
                           [stretchable-width #t]
                           [callback (lambda (b ce)
                                       (set! pinned-trace (car history-pinned-trace))
                                       (set! history-pinned-trace
                                             (or (and (null? history-pinned-trace) null)
                                                 (cdr history-pinned-trace)))

                                       (set! refresh-offscreen? #t)
                                       (send c refresh)
                                       (send c reset-hilites)
                                       (update-message-bar)
                                       (set! hilite-param #f)
                                       (set! pinned-param #f)
                                       (set! hover-tline #f)
                                       (if (null? history-pinned-trace)
                                           (send right-h-panel delete-child back-button)
                                           (send back-button set-label
                                                 (format "Back to : ~a" (get-label (car history-pinned-trace)))))
                                       (set! tline-offscreen #f)
                                       (set! refresh-tline-canvas? #t)
                                       (send trace-info-canvas refresh))]))

  (define-values (client-w client-h)
    (send f get-client-size))

  (define tpanel (new tab-panel%
                      [choices (list "Summary" "Trace Codes")]
                      [parent vpanel]
                      [alignment '(right top)]
                      [stretchable-height #t]
                      [callback (lambda (b e)
                                  (if
                                   (= (send b get-selection) 0)
                                   ;; overall
                                   (let ()
                                     (send tpanel add-child infobox)
                                     (send tpanel delete-child trace-info-canvas))
                                   ;; trace
                                   (let ()
                                     (send tpanel delete-child infobox)
                                     (send tpanel add-child trace-info-canvas))))]))

  (define refresh-tline-canvas? #t)
  (define tline-offscreen #f)
  (define tline-offscreen-dc #f)

  (define hover-tline #f)
  (define prev-pinned-trace #f)
  (define trace-w #f)
  (define trace-h #f)
  (define hilite-param #f)
  (define pinned-param #f)

  (define hover-param-trace #f) ;; a tracebox to look like a hover because of a tline param

  (define no-debug-tlines? #f)

  (define trace-info-canvas
    (new (class canvas%
           (super-new)
           (inherit refresh
                    get-view-start
                    get-client-size
                    scroll
                    init-auto-scrollbars)
           (define/private (adjust-scroll dx dy)
             (define-values (w h) (get-client-size))
             (define-values (x y) (get-view-start))
             (define adj-w (- (* view-scale trace-w) w))
             (define adj-h (- (* view-scale trace-h) h))
             (define sx (min 1 (max 0 (+ (* dx 1/40) (/ x adj-w)))))
             (define sy (min 1 (max 0 (+ (* dy 1/40) (/ y adj-h)))))
             (define tx (and (not (= x (* sx adj-w))) sx))
             (define ty (and (not (= y (* sy adj-h))) sy))
             (when (or tx ty) (scroll tx ty)))
           (define/override (on-char e)
             (for/or ([key-code (list (send e get-key-code)
                                      (send e get-other-shift-key-code)
                                      (send e get-other-altgr-key-code))])
               (and trace-h
                    (case key-code
                      [(A a) (begin (printf "hele\n") (scroll 0 1))]
                      [(wheel-up up) (adjust-scroll 0 (- scroll-speed)) #t]
                      [(wheel-down down) (adjust-scroll 0 scroll-speed) #t]
                      [(wheel-left left) (adjust-scroll (- scroll-speed) 0) #t]
                      [(wheel-right right) (adjust-scroll scroll-speed 0) #t]
                      [else #f]))))
           (define/override (on-event e)
             (define-values (dx dy) (get-view-start))
             (define mouse-x (send e get-x))
             (define mouse-y (send e get-y))
             (when pinned-trace
               (let* ([codes* (if (trace? pinned-trace)
                                  (trace-code pinned-trace)
                                  (bridge-code pinned-trace))]
                      [codes (if no-debug-tlines?
                                 (filter (lambda (c) (not (debug-merge-point? c))) codes*)
                                 codes*)]
                      [line-#-ref (quotient (+ mouse-y dy) TLINE-H)]
                      [current-tline (and (>= line-#-ref 0)
                                          (< line-#-ref (length codes))
                                          (list-ref codes line-#-ref))])
                   (set! hover-tline current-tline)
                   (unless current-tline
                     (set! hover-param-trace #f)
                     (send c reset-hilites))

                   (when hover-tline
                     (let ([bounds (tline-hbounds current-tline)])
                       (define-values (prev-hilite-param prev-pinned-param)
                         (values hilite-param pinned-param))
                       (if bounds
                           (let ([hover-param
                                  (for/or ([(p p-bounds) (in-hash bounds)])
                                    (let ([p-x-left (- (car p-bounds) dx)]
                                          [p-x-right (- (cdr p-bounds) dx)])
                                      (and (mouse-x . >= . p-x-left)
                                           (mouse-x . <= . p-x-right)
                                           p)))])
                             (when (send e leaving?)
                               (set! hover-tline #f))
                             ;; switch traces
                             (when (and hover-param
                                        (send e button-down?)
                                        (string-contains? hover-param "show bridge"))
                               (for ([b (in-list bridges)])
                                 (when (equal? (guard-id current-tline)
                                               (bridge-guard-id b))
                                   (context-switch-to b))))

                             ;; switch traces again (jump)
                             (when (and hover-param
                                        (string-contains? hover-param "TargetToken"))
                               (define target-label* (get-target hover-param))
                               (define target-label (or (hash-ref inner-loop-of target-label* #f) target-label*))
                               (set! hover-param-trace target-label)
                               (send c reset-hilites)
                               (when (send e button-down?)
                                 (for ([t (in-list traces)])
                                   (when (equal? (trace-label t) target-label)
                                     (context-switch-to (or (hash-ref inner-loop-of t #f) t)))))
                               )

                             ;; unset the hover-param-trace
                             (when (and hover-param-trace
                                        (or (not hover-param)
                                            (and hover-param-trace
                                                 (not (string-contains? hover-param "TargetToken")))))
                               (set! hover-param-trace #f)
                               (send c reset-hilites))

                             ;; reset the pinned-param
                             (when (and hover-param
                                        (is-param? hover-param)
                                        (send e button-down?)
                                        (not (equal? pinned-param hover-param)))
                               (set! pinned-param hover-param))
                             ;; un-set the pinned-param
                             (when (and (not hover-param)
                                        (send e button-down?))
                               (set! pinned-param #f))
                             ;; reset the hilite-param
                             (when (or (not hover-param) (is-param? hover-param))
                               (set! hilite-param hover-param)))
                           (begin
                             ;; unset all
                             (when hover-param-trace
                               (set! hover-param-trace #f)
                               (send c reset-hilites))

                             (set! hilite-param #f)
                             (when (send e button-down?)
                               (set! pinned-param #f))))
                       (unless (and (equal? hilite-param prev-hilite-param)
                                    (equal? pinned-param prev-pinned-param))
                         (set! refresh-tline-canvas? #t)
                         (refresh)))
                     (set! refresh-tline-canvas? #t)))))

           )
         [parent tpanel]
         [min-width (/ total-w 2)]
         [style '(hscroll vscroll deleted)]
         [paint-callback
          (lambda (c t-dc)
            (when refresh-tline-canvas?
              (unless tline-offscreen
                (draw-tlines t-dc)
                (define-values (w h) (send t-dc get-size))
                (set! tline-offscreen (send trace-info-canvas make-bitmap
                                            (->int (* view-scale w))
                                            (->int (* view-scale (+ h (* 3 TLINE-H))))))
                (set! tline-offscreen-dc (send tline-offscreen make-dc)))
              (set! refresh-tline-canvas? #f)

              (define dc tline-offscreen-dc)
              (send dc clear)
              (send dc set-smoothing 'smoothed)
              (draw-tlines dc))
            (send t-dc draw-bitmap tline-offscreen 0 0))]))

    (define (draw-tlines dc)
      (when pinned-trace
        (let ([codes (if (trace? pinned-trace)
                         (trace-code pinned-trace)
                         (bridge-code pinned-trace))]
              [jump-target (if (trace? pinned-trace)
                               (trace-jump-target pinned-trace)
                               (bridge-jump-target pinned-trace))])

          (define filtered-codes (if no-debug-tlines? (filter (lambda (c) (not (debug-merge-point? c))) codes) codes))
          (define-values (final-tline-# max-width)
            (for/fold ([tline-# 0][max-w 0])
                      ([tline (in-list filtered-codes)])
              (define new-w
                (render-tline dc tline tline-#
                              hover-tline hilite-param pinned-param labeled-counts))
              (values (add1 tline-#) (max max-w new-w))))


          (unless (eq? pinned-trace prev-pinned-trace)
            (set! trace-w max-width)
            (set! trace-h (* (+ final-tline-# GAP) TLINE-H))
            (send trace-info-canvas init-auto-scrollbars
                  (->int trace-w)
                  (->int trace-h)
                  0 0)
            (set! prev-pinned-trace pinned-trace)))))

  (define infobox
    (new text-field%
         [parent tpanel]
         [label #f]
         [font summary-font]
         [min-width (/ total-w 2)]
         [callback (lambda (tf ce) ; not changable
                     (send tf set-value summary))]
         [init-value summary]
         [style '(multiple hscroll)]))

  (define (update-message-bar)
    (and pinned-trace
         (let* ([is-trace? (trace? pinned-trace)]
                [inner-loop (and is-trace? (trace-inner-loop pinned-trace))]
                [cnt (if is-trace?
                         (trace-use-count pinned-trace)
                         (bridge-use-count pinned-trace))]
                [cnt-inner (and inner-loop
                                (trace-use-count inner-loop))]
                [msg
                 (if no-count?
                     "No count info in the input file"
                     (if inner-loop
                         (format "Run ~a / ~a times" cnt cnt-inner)
                         (format "Run ~a times" cnt)))])
           (send main-message set-label msg))))

  (define (context-switch-to t)
    (when (null? history-pinned-trace)
      (send right-h-panel add-child back-button))
    (send back-button set-label
          (format "Back to : ~a" (get-label pinned-trace)))
    (set! history-pinned-trace (cons pinned-trace
                                     history-pinned-trace))

    (send right-h-panel refresh)
    (set! pinned-trace t)
    (send c reset-hilites)
    (update-message-bar)
    (set! tline-offscreen #f)
    (set! refresh-tline-canvas? #t)
    (send trace-info-canvas refresh))

  (send f center 'both)
  (send f show #t)
  )

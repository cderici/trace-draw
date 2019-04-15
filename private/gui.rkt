#lang racket/base

(require racket/gui/base
         racket/class
         racket/string
         racket/format
         racket/path
         racket/bool
         "draw.rkt"
         "struct.rkt"
         "config.rkt"
         "jump-deps.rkt"
         "process.rkt"
         "util.rkt")

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
  (define-values (trace-blocks extra-entry-bridges total-number-of-loops jit-counts labeled-counts)
    (process-jit-counts jit-count-lines trace-candidates))

  (define no-count? (null? jit-count-lines))

  (define traces
    (pick-most-used-traces trace-candidates
                           jit-counts labeled-counts max-trace-shown
                           trace-blocks
                           extra-entry-bridges
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

  (define file-name (file-name-from-path trace-file))

  (define f (new frame%
                 [label (format "Trace Draw : ~a" file-name)]
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

  (define show-guards-check
    (new check-box%
         [label "Show Guards"]
         [parent below-controls]
         [callback (lambda (cb ce)
                     (set! hilite-all-guards? (send cb get-value))
                     (set! refresh-tline-canvas? #t)
                     (set! refresh-tline-canvas-hilites? #t)
                     (send trace-info-canvas refresh))]))

  (define no-frame-tlines-check
    (new check-box%
         [label "Hide Frame Enter/Leave"]
         [parent below-controls]
         [callback (lambda (cb ce)
                     (set! no-frame-tlines? (send cb get-value))
                     (set! tline-offscreen #f)
                     (set! refresh-tline-canvas? #t)
                     (set! recompute-tline-positions #t)
                     (send trace-info-canvas refresh))]))

  (define no-debug-tlines-check
    (new check-box%
         [label "Hide Interpreted Codes"]
         [parent below-controls]
         [callback (lambda (cb ce)
                     (set! no-debug-tlines? (send cb get-value))
                     (set! tline-offscreen #f)
                     (set! refresh-tline-canvas? #t)
                     (set! recompute-tline-positions #t)
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
                       (reset-hilites-lhs))
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
                       (when (trace? pinned-trace)
                         (unless (memv in-trace-jump-button (send right-h-panel get-children))
                           (send right-h-panel add-child in-trace-jump-button)))
                       (when (and (bridge? pinned-trace)
                                  (memv in-trace-jump-button (send right-h-panel get-children)))
                         (send right-h-panel delete-child in-trace-jump-button))
                       (reset-hilites-lhs)
                       (update-message-bar))
                     ;; unsetting a pinned-trace
                     (when (and (not hover-trace)
                                (send e button-down?))
                       (when (= (send tpanel get-selection) 1)
                         (set! refresh-tline-canvas? #t)
                         (send trace-info-canvas refresh))
                       (set! pinned-trace #f)
                       (when (memv in-trace-jump-button (send right-h-panel get-children))
                                     (send right-h-panel delete-child in-trace-jump-button))
                       (set! refresh-offscreen? #t)
                       (update-message-bar)
                       (low-priority-refresh))
                     (unless (equal? hover hover-trace)
                       (set! hover hover-trace)
                       (reset-hilites-lhs)))
                   (define/public (reset-hilites-lhs)
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
    (if (null? jit-summary)
        (format "No jit-summary is provided in the input : \"~a\"\n\nConsider using PYPYLOG=jit-summary...\n" trace-file)
        ;; FIXME : change the "infobox" to be a canvas and draw the
        ;; text manually to align things
        (string-join
         (map (lambda (l-r)
                (string-append
                 (~a (car l-r) #:width 55 #:align 'left #:right-pad-string " . ")
                 (~a (cdr l-r) #:min-width 15 #:align 'left #:right-pad-string " ")))
              jit-summary) "\n")))

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

  (define back-button
    (new button%
         [parent right-h-panel]
         [label "Back"]
         [style '(deleted)]
         [stretchable-width #t]
         [callback (lambda (b ce)
                     (set! pinned-trace (car history-pinned-trace))
                     (when (trace? pinned-trace)
                       (send right-h-panel add-child in-trace-jump-button))
                     (set! history-pinned-trace
                           (or (and (null? history-pinned-trace) null)
                               (cdr history-pinned-trace)))

                     (set! refresh-offscreen? #t)
                     (send c refresh)
                     (send c reset-hilites-lhs)
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

  (define in-trace-jump-button
    (new button%
         [parent right-h-panel]
         [label "Jump to Optimized Loop"]
         [style '(deleted)]
         [stretchable-width #t]
         [callback (lambda (b ce)
                     (if (equal? (send b get-label) "Jump to Optimized Loop")
                         (when current-optimized-loop-y-position
                           (send b set-label "Jump to Preamble")
                           (send trace-info-canvas adjust-scroll 0
                                 current-optimized-loop-y-position))
                         (let ()
                           (send b set-label "Jump to Optimized Loop")
                           (send trace-info-canvas adjust-scroll 0 0))))]))



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

  (define position-cache (make-hash))
  (define recompute-tline-positions #f)

  ;; FIXME : cache these with trace-labels
  (define current-hoverable-positions #f) ;; (hash tline (listof (cons number number)))
  (define current-hilite-rectangle-positions #f) ;; (hash "str" (listof display-bounds))
  (define current-tline-positions #f) ;; (hash tline <everything needed to draw the tline>)
  (define current-optimized-loop-y-position #f)
  (define refresh-tline-canvas-hilites? #t)

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
  (define no-frame-tlines? #f)
  (define hilite-all-guards? #f)

  (define trace-info-canvas
    (new (class canvas%
           (super-new)
           (inherit refresh
                    get-view-start
                    get-client-size
                    scroll
                    init-auto-scrollbars)
           (define/public (adjust-scroll dx dy)
             (define-values (w h) (get-client-size))
             (define-values (x y) (get-view-start))
             (define adj-w (- (* view-scale trace-w) w))
             (define adj-h (- (* view-scale trace-h) h))
             (define sx (min 1 (max 0 (+ (* dx 1/40) (/ x adj-w)))))
             (define sy (min 1 (max 0 (+ (* dy 1/40) (/ y adj-h)))))
             (define tx (and (not (= x (* sx adj-w))) sx))
             (define ty (and (not (= y (* sy adj-h))) sy))
             (when (or tx ty) (scroll tx ty)))
           (define/public (scroll-to x y)
             #;(define-values (w h) (get-client-size))
             (scroll (/ x trace-w) (/ y trace-h)))
           (define/override (on-char e)
             (for/or ([key-code (list (send e get-key-code)
                                      (send e get-other-shift-key-code)
                                      (send e get-other-altgr-key-code))])
               (and trace-h
                    (case key-code
                      [(A a) (scroll 0 1)]
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
                      [codes (filter (lambda (c)
                                       (and (not (and no-debug-tlines?
                                                      (debug-merge-point? c)))
                                            (not (and no-frame-tlines?
                                                      (is-frame-tline? c))))) codes*)]
                      [line-# (->int (quotient (+ mouse-y dy) (quotient trace-h (length codes))))]
                      [current-tline (and (>= line-# 0)
                                          (< line-# (length codes))
                                          (list-ref codes line-#))])
                 (set! hover-tline current-tline)
                 ;; unset the hover-param-trace if we're not hovering
                 ;; any tline (recall that hovering on a trace label
                 ;; on the rhs should hilite the trace on the lhs)
                 (unless current-tline
                   (set! hover-param-trace #f)
                   (send c reset-hilites-lhs))

                 (when hover-tline
                   (let ()
                     ;; record the previous hilites
                     (define-values (prev-hilite-param prev-pinned-param)
                       (values hilite-param pinned-param))
                     (let ([hoverable-positions (hash-ref current-hoverable-positions hover-tline #f)])
                       ;; does the currently hovered tline has any hoverables?
                       (if hoverable-positions
                         (let ([hover-param ;; what are we hovering over?
                                (for/or ([hoverable (in-list hoverable-positions)])
                                  (let ([p-x-left (vector-ref hoverable 1)]
                                        [p-x-right (vector-ref hoverable 2)])
                                    (and (mouse-x . >= . p-x-left)
                                         (mouse-x . <= . p-x-right)
                                         (vector-ref hoverable 0))))])
                           ;; unset the hover-tline if the mouse is
                           ;; leaving
                           (when (send e leaving?)
                             (set! hover-tline #f))

                           ;; hover & context-switch to bridge
                           (when (and (guard? hover-tline)
                                      hover-param
                                      (string-contains? hover-param "show-bridge-hover"))
                             (for ([b (in-list bridges)])
                               (when (equal? (guard-id hover-tline) (bridge-guard-id b))
                                 #;(set! hover b) ;; FIXME: revisit this
                                 (send c reset-hilites-lhs)
                                 (when (send e button-down?)
                                   (set! hover-param #f)
                                   (when (memv in-trace-jump-button (send right-h-panel get-children))
                                     (send right-h-panel delete-child in-trace-jump-button))
                                   (set! pinned-param #f)
                                   (context-switch-to b)))))

                           ;; context-switch on jump operation ;; FIXME: add "call" operations to this
                           (when (and (is-jump? hover-tline)
                                      hover-param
                                      (string-contains? hover-param "TargetToken"))
                             (define target-label* (get-target hover-param))
                             (define target-label (or (hash-ref inner-loop-of target-label* #f) target-label*))
                             (set! hover-param-trace target-label)
                             (send c reset-hilites-lhs)
                             (when (send e button-down?)
                               (for ([t (in-list traces)])
                                 (when (equal? (trace-label t) target-label)
                                   (set! hover-param #f)
                                   (set! pinned-param #f)
                                   (context-switch-to (or (hash-ref inner-loop-of t #f) t))))))

                           ;; un-hilite the hilited trace on the lhs
                           (when (and hover-param-trace
                                      (or (not hover-param)
                                          (and hover-param-trace
                                               (not (string-contains? hover-param "TargetToken")))))
                             (set! hover-param-trace #f)
                             (send c reset-hilites-lhs))

                           ;; reset the pinned-param
                           (when (and hover-param
                                      (is-param? hover-param)
                                      (send e button-down?)
                                      (not (equal? pinned-param hover-param)))
                             (set! pinned-param hover-param))

                           ;; or un-set the pinned-param if we clicked
                           ;; somewhere else
                           (when (and (not hover-param)
                                      (send e button-down?))
                             (set! pinned-param #f))

                           ;; reset the hilite-param (recall:
                           ;; "pinned-param" is different than
                           ;; "hilite-param")
                           (when (or (not hover-param) (is-param? hover-param))
                             (set! hilite-param hover-param)))

                         ;; if the currently hovered tline doesn't
                         ;; have any hoverable positions
                         (begin
                           ;; un-hilite the trace on the lhs
                           (when hover-param-trace
                             (set! hover-param-trace #f)
                             (send c reset-hilites-lhs))

                           (set! hilite-param #f)
                           (when (send e button-down?)
                             (set! pinned-param #f))))

                       ;; if either hilite-param or pinned-param has
                       ;; changed then refresh the hilites on the
                       ;; tline canvas
                       (unless (and (equal? hilite-param prev-hilite-param)
                                    (equal? pinned-param prev-pinned-param))
                         (set! refresh-tline-canvas-hilites? #t)
                         #;(set! refresh-tline-canvas? #t)
                         (refresh))))

                   (set! refresh-tline-canvas? #t))))))
         [parent tpanel]
         [min-width (/ total-w 2)]
         [style '(hscroll vscroll deleted)]
         [paint-callback
          (lambda (c t-dc)

            (when refresh-tline-canvas?

              (unless tline-offscreen

                (let ([codes (if (trace? pinned-trace)
                                 (trace-code pinned-trace)
                                 (bridge-code pinned-trace))])
                  (when pinned-trace
                    (when (or recompute-tline-positions
                              (not (hash-ref position-cache pinned-trace #f))
                              (xor no-debug-tlines?
                                   (hash-ref (hash-ref position-cache pinned-trace) 'cached-no-debug-status))
                              (xor no-frame-tlines?
                                   (hash-ref (hash-ref position-cache pinned-trace) 'cached-no-frame-status)))
                      (send t-dc set-font t-font) ; to match the rendering font
                      (define-values (hoverable-positions
                                    hilite-rectangle-positions
                                    tline-positions
                                    max-w
                                    current-h
                                    optimized-loop-h)
                        (compute-tline-positions-and-dimensions t-dc codes no-debug-tlines? no-frame-tlines? labeled-counts))
                      (set! recompute-tline-positions #f)
                      (hash-set! position-cache
                                 pinned-trace
                                 (hash 'hoverable-positions hoverable-positions
                                       'rectangle-positions hilite-rectangle-positions
                                       'tline-positions tline-positions
                                       'tline-canvas-height current-h
                                       'tline-max-width max-w
                                       'cached-no-debug-status no-debug-tlines?
                                       'cached-no-frame-status no-frame-tlines?
                                       'optimized-loop-y-position optimized-loop-h)))
                    (define cached-positions (hash-ref position-cache pinned-trace))
                    (set! current-hoverable-positions (hash-ref cached-positions 'hoverable-positions))
                    (set! current-hilite-rectangle-positions (hash-ref cached-positions 'rectangle-positions))
                    (set! current-tline-positions (hash-ref cached-positions 'tline-positions))
                    (set! current-optimized-loop-y-position (hash-ref cached-positions 'optimized-loop-y-position))

                    (set! no-debug-tlines? (hash-ref cached-positions 'cached-no-debug-status))
                    (set! no-frame-tlines? (hash-ref cached-positions 'cached-no-frame-status))

                    (set! trace-w (hash-ref cached-positions 'tline-max-width))
                    (set! trace-h (hash-ref cached-positions 'tline-canvas-height))

                    (send trace-info-canvas init-auto-scrollbars
                          (->int trace-w)
                          (->int trace-h)
                          0 0)

                    (set! tline-offscreen (send trace-info-canvas make-bitmap
                                                (->int (* view-scale trace-w))
                                                (->int (* view-scale trace-h))))
                    (set! tline-offscreen-dc (send tline-offscreen make-dc)))))

              (set! refresh-tline-canvas? #f)
              (define dc tline-offscreen-dc)
              (send dc clear)
              (send dc set-smoothing 'smoothed)
              (draw-tlines dc))

            (when (and refresh-tline-canvas-hilites?
                       current-hilite-rectangle-positions
                       (or hilite-all-guards? hilite-param pinned-param))
              (render-hilites tline-offscreen-dc hilite-param pinned-param current-hilite-rectangle-positions hilite-all-guards?)
              (set! refresh-tline-canvas-hilites? #f))

            (send t-dc draw-bitmap tline-offscreen 0 0))]))

  (define (draw-tlines dc)
    (when pinned-trace
      (let ([codes (if (trace? pinned-trace)
                       (trace-code pinned-trace)
                       (bridge-code pinned-trace))])
        (render-tlines dc codes current-tline-positions no-debug-tlines? no-frame-tlines?)
        (unless (eq? pinned-trace prev-pinned-trace)
          (send trace-info-canvas init-auto-scrollbars
                (->int trace-w)
                (->int trace-h)
                0 0)
          (set! prev-pinned-trace pinned-trace)))))

  (define infobox-content
    (format "
Trace file                                :      ~a

~a

~a" file-name (if no-count?
                  "No count info found. Displaying everything we can find."
                  (format "There are ~a loops in the trace log.\nDisplaying ~a, along with every associated bridge.\nOrdered from top to bottom by loop counts."
                          total-number-of-loops
                          (length traces)))
            summary))

  (define infobox
    (new text-field%
         [parent tpanel]
         [label #f]
         [font summary-font]
         [min-width (/ total-w 2)]
         [callback (lambda (tf ce) ; not changable
                     (send tf set-value infobox-content))]
         [init-value infobox-content]
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
    (send c reset-hilites-lhs)
    (update-message-bar)
    (set! tline-offscreen #f)
    (set! refresh-tline-canvas? #t)
    (send trace-info-canvas refresh))

  (send f center 'both)
  (send f show #t)
  )

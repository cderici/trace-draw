#lang racket/base

(require racket/gui/base
         racket/class
         "draw.rkt"
         "struct.rkt"
         "config.rkt"
         "jump-deps.rkt")

(provide make-gui)
(define (->int n) (ceiling (inexact->exact n)))

(define (make-gui #:traces traces
                  #:bridges bridges)

  (define pinned-trace #f)
  (define hover #f)
  (define hilites null)

  (define-values (screen-w screen-h) (get-display-size))

  (define f (new frame%
                 [label "Trace Draw"]
                 [width (- screen-w 400)]
                 [height (- screen-h 400)]))

  (define panel (new vertical-panel%
                     [parent f]
                     [alignment '(left center)]))

  (define refresh-offscreen? #t)
  (define offscreen #f)
  (define offscreen-dc #f)
  (define view-scale 1)

  (define total-w (* 2 (- screen-w 400)))
  (define total-h (* 2 (- screen-h 400)))

  (define display-bounds-ht ; display locations for traces and bridges
    (let* ([y-init 50]
           [x-entry 100]
           [x-trace (+ x-entry t-width X-GAP)]
           [x-bridge (+ x-trace t-width X-GAP)])
      (define-values (new-ht y-entry-next y-trace-next)
        (for/fold ([ht (hash)]
                   [y-entry y-init]
                   [y-trace y-init])
                  ([t (in-list traces)])
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

  ;; trace-jumps : mapping from traces/bridges to trace labels
  ;; guard-exits : mapping from traces to (listof bridge-guard-ids)
  (define-values (trace-jumps guard-exits)
    (compute-jumps traces bridges))

  (printf "trace-jumps : ~a\nguard-exits : ~a\n" trace-jumps guard-exits)

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
                         [(wheel-up up) (adjust-scale 1/20) #t]
                         [(wheel-down down) (adjust-scale -1/20) #t]
                         [(wheel-left left) (adjust-scroll -1 0) #t]
                         [(wheel-right right) (adjust-scroll 1 0) #t]
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
                       (when (or (> (abs (- mouse-x prev-mouse-x)) 20)
                                 (> (abs (- mouse-y prev-mouse-y)) 20))
                         (reset-mouse))
                       (unless (= prev-mouse-x -1)
                         (adjust-scroll (/ (- prev-mouse-x mouse-x) 8)
                                        (/ (- prev-mouse-y mouse-y) 8)))
                       (set! prev-mouse-x mouse-x)
                       (set! prev-mouse-y mouse-y))

                     (define hover-trace
                       (for/or ([(t-b bounds) (in-hash display-bounds-ht)])
                         (let* ([x-left (display-bound-x bounds)]
                                [y-top (display-bound-y bounds)]
                                [x-right (+ x-left (display-bound-w bounds))]
                                [y-bottom (+ y-top (display-bound-h bounds))])
                           (and (mouse-x . >= . x-left)
                                (mouse-y . >= . y-top)
                                (mouse-x . <= . x-right)
                                (mouse-y . <= . y-bottom)
                                t-b))))
                     (when (and hover-trace
                                (send e button-down?)
                                (not (equal? pinned-trace hover-trace)))
                       (set! pinned-trace hover-trace))
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
                     (set! refresh-offscreen? #t)
                     (low-priority-refresh)))
                 [parent panel]
                 [style '(hscroll vscroll)]
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
              #:display-bounds-ht display-bounds-ht
              #:view-scale view-scale
              #:hilites hilites)
    #;(draw-graph dc
                #:pkgs pkgs
                #:invert-pkgs invert-pkgs
                #:pkg-bounds pkg-bounds
                #:at-depth at-depth
                #:reps reps
                #:no-build-reps no-build-reps
                #:total-w total-w
                #:total-h total-h
                #:view-scale view-scale
                #:invert? (send invert-checkbox get-value)
                #:all-deps? (send build-deps-checkbox get-value)
                #:trans-deps? (send trans-deps-checkbox get-value)
                #:hilites hilites))

  (send c init-auto-scrollbars
        (->int (* view-scale total-w))
        (->int (* view-scale total-h))
        0.0 0.0)

  (define hpanel (new horizontal-panel%
                      [parent panel]
                      [alignment '(left center)]
                      [stretchable-height #f]))

  (define infobox
    (new message%
         [parent hpanel]
         [label (format "~a traces\n...with ~a entry bridges\n~a bridges\n"
                        (length traces)
                        (length (filter (lambda (t) (trace-is-entry? t)) traces))
                        (length bridges))]
         ))

  #;(define build-deps-checkbox
    (new check-box%
         [parent hpanel]
         [label "Show build dependencies"]
         [value #t]
         [callback (lambda (cb e) (send c reset-hilites))]))
  #;(define trans-deps-checkbox
    (new check-box%
         [parent hpanel]
         [label "Show transitive dependencies"]
         [value #t]
         [callback (lambda (cb e) (send c reset-hilites))]))
  #;(define invert-checkbox
    (new check-box%
         [parent hpanel]
         [label "Reverse links"]
         [value #f]
         [callback (lambda (cb e) (send c reset-hilites))]))

  (send f show #t))

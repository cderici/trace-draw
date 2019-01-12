#lang racket/base

(require racket/class
         racket/draw
         "config.rkt"
         "struct.rkt")


(provide draw-all)

;;; UTILITIES

(define (trace-middle-right t-x t-y)
  (values (+ t-x t-width)
          (+ t-y (/ t-height 2))))


(define (trace-middle-left t-x t-y)
  (values t-x
          (+ t-y (/ t-height 2))))


(define (draw-arrow dc from-x from-y to-x to-y)
  (send dc draw-spline
        from-x from-y
        (+ (/ (+ from-x to-x) 2) SPLINE-SHIFT)
        (+ (/ (+ from-y to-y) 2) SPLINE-SHIFT)
        to-x to-y)
  (send dc set-brush (make-brush #:style 'transparent))
  (send dc draw-ellipse
        (- to-x 5) (- to-y 5) 10 10))

(define (draw-trace dc t bounds)
  (send dc set-brush tracebox-brush)
  (define current-x (display-bound-x bounds))
  (define current-y (display-bound-y bounds))
  (send dc draw-rounded-rectangle
        current-x
        current-y
        (display-bound-w bounds)
        (display-bound-h bounds))

  #;(define-values (w h d a) (send dc get-text-extent s trace-title-font #t))
  (send dc draw-text (trace-label t) (+ current-x TGAP) (+ current-y TGAP) #t)

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

(define (draw-bridge dc b bounds)
  (define current-x (display-bound-x bounds))
  (define current-y (display-bound-y bounds))

  (send dc set-brush bridgebox-brush)
  (send dc draw-rounded-rectangle
        current-x current-y
        (display-bound-w bounds)
        (display-bound-h bounds))

  (send dc draw-text "bridge for" (+ current-x TGAP) (+ current-y TGAP) #t)
  (send dc draw-text (bridge-guard-id b) (+ current-x TGAP) (+ current-y TGAP TGAP) #t))

;;;; MAIN DRAW

(define (draw-all dc
                  #:traces traces
                  #:bridges bridges
                  #:display-bounds-ht display-bounds-ht
                  #:view-scale view-scale)
  (send dc set-font font)
  (send dc set-text-foreground "black")
  (send dc set-smoothing 'aligned)
  (send dc set-scale view-scale view-scale)

  (for ([(t-b bounds) (in-hash display-bounds-ht)])
    (if (trace? t-b)
        (draw-trace dc t-b bounds)
        (draw-bridge dc t-b bounds)))

  #;(for/fold ([y-entry 50][y-trace 50]) ([t (in-list traces)])
    (if (trace-is-entry? t)
        (begin
          (draw-trace dc t x-entry y-entry)
          (values (+ y-entry t-height Y-GAP) y-trace))
        (begin
          (draw-trace dc t x-trace y-trace)
          (values y-entry (+ y-trace t-height Y-GAP)))))

  #;(for/fold ([y 50]) ([b (in-list bridges)])
    (draw-bridge dc b x-bridge y)
    (+ y b-height Y-GAP))

  (define t1-x 50)
  (define t1-y 50)

  #;(send dc draw-rounded-rectangle
        t1-x t1-y
        t-width t-height)

  (define t2-x 190)
  (define t2-y 300)
  #;(send dc draw-rounded-rectangle
        t2-x t2-y
        t-width t-height)


  (define-values (from-x from-y)
    (trace-middle-right t1-x t1-y))

  (define-values (to-x to-y)
    (trace-middle-left t2-x t2-y))

  (draw-arrow dc from-x from-y to-x to-y)

  )

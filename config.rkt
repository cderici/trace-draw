#lang racket/base
(require racket/draw)

(provide (all-defined-out))

(define FONT-SIZE 9)
(define T-FONT-SIZE 10)
(define SPACE-SIZE 10)
(define SEP-SIZE FONT-SIZE)
(define MARGIN FONT-SIZE)

(define t-width 70) ; trace width
(define t-height 100); trace height

(define b-width t-width)
(define b-height (/ t-height 2))

(define SPLINE-SHIFT 100)
(define GAP 5)
(define TGAP (* 2 GAP))
(define X-GAP (* 3 t-width))
(define Y-GAP (* 2 GAP))

(define face "Helvetica")
(define trace-title-font
  (make-font #:face face #:size FONT-SIZE #:weight 'bold #:size-in-pixels? #t))

(define font (make-font #:face face #:size FONT-SIZE #:size-in-pixels? #t))
(define t-font (make-font #:face face #:size T-FONT-SIZE #:size-in-pixels? #t))
(define tb-font (make-font #:face face #:size T-FONT-SIZE #:weight 'bold #:size-in-pixels? #t))

;; Pens
(define box-pen (make-pen #:color "black" #:width 1 #:style 'solid))
(define line-pen (make-pen #:color "black" #:width 1 #:style 'short-dash))
(define hilite-pen (make-pen #:color "blue" #:width 1 #:style 'solid))

(define tracebox-color (make-color 150 0 100 0.3))
(define tracebox-brush
  (make-brush #:color tracebox-color #:style 'solid))
(define trace-innerbox-brush
  (make-brush #:color (make-color 100 0 50 0.3) #:style 'solid))

(define tracebox-highlight-brush
  (make-brush #:color (make-color 0 0 100 0.3) #:style 'solid))

(define bridgebox-brush
  (make-brush #:color (make-color 0 100 100 0.3) #:style 'solid))

(define bridgebox-highlight-brush
  (make-brush #:color (make-color 0 0 100 0.3) #:style 'solid))
   
(define jump-color "blue")

(define trace-pen-width 2)
(define pen-width 1)

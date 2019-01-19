#lang racket/base
(require racket/string)

(provide (all-defined-out))

(define-struct trace (label         ; number
                      is-entry?     ; boolean
                      inner-loop    ; #f or inner trace
                      code          ; racket code at the first merge point (string)
                      use-count     ; -1 for unknown
                      guards        ; (listof number) guard ids
                      jump-target   ; id number of where it jumps to
                      ))

(define-struct bridge (guard-id ; string
                       guards
                       use-count
                       code
                       jump-target))

(define-struct display-bound (x y w h))

;; For trace internals
;; 1 -- starts with #
(define-struct info-tline (line-str))
;; 2 -- [p0, i1, i2, p3]
(define-struct param-tline (params hbounds)) ; list of str
;; 3 -- debug_merge_point
(define-struct debug-merge-point (code))
;; 4 -- guard_class(....) [p0, i1, i2, p3]
(define-struct guard (id line type args jump-bridge-params bridge? hbounds))
#;(define-struct guard-tline (guard-type check-args jump-args))
;; 5 -- assignment line -> p5 = getfield_gc_r(p0, .....)
(define-struct assignment-tline (lhs op args hbounds))
;; 6 -- operation line -> setfield_gc(p34, p28, descr=<FieldP pycket.cont.BaseCont.inst_marks 8>)
(define-struct operation-tline (op args hbounds))

(define (tline-hbounds t)
  (cond
    [(param-tline? t) (param-tline-hbounds t)]
    [(guard? t) (guard-hbounds t)]
    [(assignment-tline? t) (assignment-tline-hbounds t)]
    [(operation-tline? t) (operation-tline-hbounds t)]
    [else #f]))

(define (is-label? tline)
  (and (operation-tline? tline)
       (equal? (operation-tline-op tline) "label")))

(define (is-entry-bridge-label? tline)
  (and (info-tline? tline)
       (string-contains? (info-tline-line-str tline) "entry bridge")))

(define (is-bridge-label? tline)
  (and (info-tline? tline)
       (string-contains? (info-tline-line-str tline) "bridge out of Guard ")))

(define (is-jump? tline)
  (and (operation-tline? tline)
       (equal? (operation-tline-op tline) "jump")))

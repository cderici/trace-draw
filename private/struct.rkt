#lang racket/base

(provide (all-defined-out))

(define-struct trace-block 
  (entry-label outer-label inner-label))

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

;; FIXME : rename -> display-position
(define-struct display-bound (x y w h))

;; For trace internals
;; 1 -- starts with #
(define-struct info-tline (line-str))
;; 2 -- [p0, i1, i2, p3]
(define-struct param-tline (params)) ; list of str
;; 3 -- debug_merge_point
(define-struct debug-merge-point (code))
;; 4 -- guard_class(....) [p0, i1, i2, p3]
(define-struct guard (id line type args jump-bridge-params bridge? belongs-to))
#;(define-struct guard-tline (guard-type check-args jump-args))
;; 5 -- assignment line -> p5 = getfield_gc_r(p0, .....)
(define-struct assignment-tline (lhs op args))
;; 6 -- operation line -> setfield_gc(p34, p28, descr=<FieldP pycket.cont.BaseCont.inst_marks 8>)
(define-struct operation-tline (op args))

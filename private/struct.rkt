#lang racket/base
(require racket/string)

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

(define (is-param? p)
  (not (or (string->number p) (equal? p "show bridge"))))

(define (get-label t-b)
  (if (trace? t-b)
      (trace-label t-b)
      (bridge-guard-id t-b)))

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

(define (is-frame-tline? tline)
  (and (operation-tline? tline)
       (or (equal? (operation-tline-op tline) "enter_portal_frame")
           (equal? (operation-tline-op tline) "leave_portal_frame"))))

(define (get-target param)
  (if (string-contains? param "TargetToken")
      (substring param 12 (sub1 (string-length param)))
      param))

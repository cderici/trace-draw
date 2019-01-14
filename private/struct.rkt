#lang racket/base

(provide (all-defined-out))

(define-struct trace (label         ; number
                      id-line       ; string line containing the label
                      is-entry?     ; boolean
                      inner-loop    ; #f or inner trace
                      code          ; racket code at the first merge point (string)
                      use-count     ; -1 for unknown
                      guards        ; (listof number) guard ids
                      jump-target   ; id number of where it jumps to
                      text          ; raw text of the entire trace (string)
                      ))

(define-struct guard (id line))

(define-struct bridge (guard-id ; string
                       id-line
                       guards
                       use-count
                       jump-target
                       text))

(define-struct display-bound (x y w h))

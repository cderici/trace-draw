#lang racket/base

(require "struct.rkt" racket/string)

(provide (all-defined-out))

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

(define (is-call? tline)
  (and (assignment-tline? tline)
       (string-contains? (assignment-tline-op tline) "call_assembler")))

;(define (get-call-target tline)

(define (is-frame-tline? tline)
  (and (operation-tline? tline)
       (or (equal? (operation-tline-op tline) "enter_portal_frame")
           (equal? (operation-tline-op tline) "leave_portal_frame"))))

(define (get-target param)
  (if (string-contains? param "TargetToken")
      (substring param 12 (sub1 (string-length param)))
      param))

;; h1 -> (hash "string" display-bound)
;; h2 -> (hash "string" (listof display-bound))
(define (append-hash-table h1 h2)
  (for/fold ([return-hash h2])
            ([(s d) (in-hash h1)])
    (let ([current-bounds (hash-ref return-hash s #f)])
      (if current-bounds
          (hash-set return-hash
                    s (if (list? current-bounds) (cons d current-bounds) (list d current-bounds)))
          (hash-set return-hash
                    s (list d))))))

;; ht -> (hash "string" (listof display-bound))
(define (cons-hash-table key value ht)
  (let ([current-val (hash-ref ht key #f)])
    (if current-val
        (hash-set ht key (if (list? current-val) (cons value current-val) (list value current-val)))
        (hash-set ht key (list value)))))

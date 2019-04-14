#lang racket/base

(provide append-hash-table
         cons-hash-table)

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

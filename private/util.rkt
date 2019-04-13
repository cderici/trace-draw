#lang racket/base

;; h1 -> (hash "string" display-bound)
;; h2 -> (hash "string" (listof display-bound))
(define (append-hash-table h1 h2)
  (for/fold ([return-hash h2])
            ([(s d) (in-hash h1)])
    (let ([current-bounds (hash-ref return-hash s #f)])
      (if current-bounds
          (hash-set return-hash
                    s (cons d current-bounds))
          (hash-set return-hash
                    s (list d))))))

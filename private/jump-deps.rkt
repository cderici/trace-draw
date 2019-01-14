#lang racket/base
(require "struct.rkt")

(provide compute-jumps get-jump-deps)

(define (get-jump-deps t-b trace-jumps guard-exits)
  (if (trace? t-b)
      (let ([tj (hash-ref trace-jumps t-b #f)]
            [inner-loop (trace-inner-loop t-b)])
        (cons (trace-label t-b)
              (if inner-loop
                  (cons
                   (hash-ref trace-jumps inner-loop)
                   (append
                    (hash-ref guard-exits t-b)
                    (hash-ref guard-exits inner-loop)))
                  (if tj
                      (cons tj (hash-ref guard-exits t-b))
                      (hash-ref guard-exits t-b)))))
      (append (list (bridge-guard-id t-b) (bridge-jump-target t-b))
              (hash-ref guard-exits t-b))))

;; mapping from traces to trace-labels
(define (compute-jumps traces bridges)

  (define (compute-trace-jump t traces bridges jumps guard-exits)
    (values
     (hash-set jumps t (trace-jump-target t))
     (let ((guards (trace-guards t)))
       (hash-set guard-exits t
                 (for/fold ([ls null])
                           ([g (in-list guards)])
                   (let ([g-id (guard-id g)])
                     (if
                      (for/or ([b (in-list bridges)])
                        (equal? g-id (bridge-guard-id b)))
                      (cons g-id ls)
                      ls)))))))

  (define-values (trace-jumps guard-exits inners)
    (for/fold ([jumps (hash)]
               [guard-exits (hash)]
               [inners (hash)])
              ([t (in-list traces)])

      (define-values (t-jumps t-guard-exits)
        (compute-trace-jump t traces bridges jumps guard-exits))

      (define-values (next-jumps next-guard-exits)
        (if (trace-inner-loop t)
            (compute-trace-jump (trace-inner-loop t) traces bridges jumps t-guard-exits)
            (values t-jumps t-guard-exits)))

      (if (trace-inner-loop t)
          (values next-jumps next-guard-exits
                  (hash-set inners
                            (trace-label (trace-inner-loop t))
                            (trace-label t)))
          (values next-jumps next-guard-exits inners))))


  ;; add bridge guard exits
  (define final-guard-exits
    (for/fold
        ([g-e guard-exits])
        ([b (in-list bridges)])
      (hash-set g-e b
                (for/fold ([ls null])
                          ([g (in-list (bridge-guards b))])
                  (let ([g-id (guard-id g)])
                    (if (for/or ([to-b (in-list bridges)])
                          (equal? g-id (bridge-guard-id to-b)))
                        (cons g-id ls)
                        ls))))))

  (values trace-jumps final-guard-exits inners))

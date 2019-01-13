#lang racket/base
(require "struct.rkt")

(provide compute-jumps get-jump-deps)

(define (get-jump-deps trace trace-jumps guard-exits)
  (cons (trace-label trace)
        (cons (hash-ref trace-jumps trace)
              (append (if (not (trace-inner-loop trace))
                          null
                          (hash-ref guard-exits (trace-inner-loop trace)))
                      (hash-ref guard-exits trace)))))


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

  (define-values (trace-jumps guard-exits)
    (for/fold ([jumps (hash)]
               [guard-exits (hash)])
              ([t (in-list traces)])
      (define-values (_jumps _guard-exits)
        (if (not (trace-inner-loop t))
            (values jumps guard-exits)
            (compute-trace-jump (trace-inner-loop t) traces bridges jumps guard-exits)))
      (compute-trace-jump t traces bridges _jumps _guard-exits)))

  (for/fold ([ht trace-jumps])
            ([b (in-list bridges)])
    (hash-set ht b (bridge-jump-target b)))
  (values trace-jumps guard-exits))

#;(define (compute-jump-deps current-t-b traces bridges) ; -> (hash)
  (define jump-f (if (trace? current-t-b) trace-jump-target bridge-jump-target))
  (define guards-f (if (trace? current-t-b) trace-guards bridge-guards))

  (define j-target (jump-f current-t-b))
  (define guards (guards-f current-t-b))
  (when (and (trace? current-t-b)
             (trace-inner-loop current-t-b))
    (set! guards (append guards (trace-guards (trace-inner-loop current-t-b)))))

  (define hilites (list j-target))

  (define guard-bridges (filter (lambda (b)
                                  (for/or ([g (in-list guards)])
                                    (equal? (bridge-guard-id b) g))) bridges))
  )

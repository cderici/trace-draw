#lang racket/base

(require racket/string
         racket/system
         racket/port
         racket/match
         racket/class
         racket/gui/base)


(provide read-all)

(define (init-gui trace-file)
  ;; Gather meta information (e.g. # of lines in the file)
  (define wc-output (with-output-to-string (lambda () (system (format "wc -l ~a" trace-file)))))
  (define number-of-lines (string->number (car (string-split (string-trim wc-output) " "))))

  ;; FEEDBACK LOADING GUI
  (eprintf "Starting to work on file : ~a ... \n" trace-file)
  (define load-w 400)
  (define load-h 100)
  (define loading (new frame% [label "Trace Draw"][width load-w][height load-h][border 50]))
  (define status-panel (new vertical-panel%
                            [parent loading]
                            [spacing 30]
                            [alignment '(center center)]))
  (define oversized? (>= number-of-lines 1000000))
  (define status-bar (new gauge% [label #f]
                          [parent status-panel]
                          [range (if oversized?
                                     (inexact->exact (ceiling (/ number-of-lines 10)))
                                     number-of-lines)]
                          [style '(horizontal vertical-label)]))
  (define msg (new message%
                   [label (format "Loading : ~a (~a lines)"
                                  trace-file
                                  number-of-lines)]
                   [parent status-panel]))
  (send loading center 'both)
  (send loading show #t)
  (send status-bar set-value 0)

  (values status-bar loading oversized?))

(define (read-all trace-file)
  (define-values (status-bar loading oversized?) (init-gui trace-file))

  ;; MAIN INITIAL LOOP
  (define all-loops null)
  (define all-bridges null)
  (define jit-summary null)
  (define jit-backend-count-lines null)

  (define (parse-seq close-str iter skip? current-line-count)
    (for/fold ([line-count current-line-count]
               [next #f]
               [acc null]
               #:result (if (or (not next) skip?)
                            (values (reverse acc) (add1 line-count))
                            (values (reverse (cons next acc)) (add1 line-count))))
              ([ln iter])
      (when (or (not oversized?) (= (modulo line-count 10) 0))
        (send status-bar set-value (add1 (send status-bar get-value))))
      #:break (string-contains? ln close-str)
      (values (add1 line-count) ln (if next (cons next acc) acc))))

  (define-syntax-rule (push! v l) (set! l (cons v l)))

  (with-input-from-file trace-file
    (lambda ()
      (define iter (in-lines))
      (for/fold ([line-count 1])
                ([ln iter])
        (define r (string-contains? ln " {jit-"))
        (define new-line-count
          (or (and r
                   (cond
                     [(string-contains? ln " {jit-log-opt-loop")
                      (let-values ([(acc n-line-count)
                                    (parse-seq "jit-log-opt-loop}" iter #t (add1 line-count))])
                        (push! acc all-loops) n-line-count)]
                     [(string-contains? ln " {jit-log-opt-bridge")
                      (let-values ([(acc n-line-count)
                                    (parse-seq "jit-log-opt-bridge}" iter #t (add1 line-count))])
                        (push! acc all-bridges) n-line-count)]
                     [(string-contains? ln " {jit-summary")
                      (let-values ([(acc n-line-count)
                                    (parse-seq "jit-summary}" iter #f (add1 line-count))])
                        (set! jit-summary
                              (for/list ([a (in-list acc)])
                                (let ([splt (string-split a "\t")])
                                  (if (= (length splt) 3)
                                      (cons (string-trim (string-normalize-spaces (car splt)) ":")
                                            (string-normalize-spaces (caddr splt)))
                                      (cons (string-trim (string-normalize-spaces (car splt)) ":")
                                            (string-normalize-spaces (cadr splt)))))))
                        n-line-count)]
                     [(string-contains? ln " {jit-backend-counts")
                      (let-values ([(acc n-line-count)
                                    (parse-seq "jit-backend-counts}" iter #f (add1 line-count))])
                        (set! jit-backend-count-lines acc) n-line-count)]
                     [else (add1 line-count)]))
              (add1 line-count)))
        (when (or (not oversized?) (= (modulo new-line-count 10) 0))
          (send status-bar set-value (add1 (send status-bar get-value))))
        new-line-count)))

  (send loading show #f)
  (eprintf "DONE in ")

  (define ordered-loop-lines (reverse all-loops))
  (define ordered-bridge-lines (reverse all-bridges))

  (values ordered-loop-lines
          ordered-bridge-lines
          jit-summary
          jit-backend-count-lines))

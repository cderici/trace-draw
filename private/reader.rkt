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
  (eprintf "Starting to work on file : ~a ... " trace-file)
  (define load-w 400)
  (define load-h 100)
  (define loading (new frame% [label "Trace Draw"][width load-w][height load-h][border 50]))
  (define status-panel (new vertical-panel%
                            [parent loading]
                            [spacing 30]
                            [alignment '(center center)]))
  (define status-bar (new gauge% [label #f]
                          [parent status-panel]
                          [range number-of-lines]
                          [style '(horizontal vertical-label)]))
  (define msg (new message%
                   [label (format "Loading : ~a (~a lines)"
                                  trace-file
                                  number-of-lines)]
                   [parent status-panel]))
  (send loading center 'both)
  (send loading show #t)
  (send status-bar set-value 0)
  (values status-bar loading))

(define (read-all trace-file)
  (define-values (status-bar loading) (init-gui trace-file))
  ;; MAIN INITIAL LOOP
  (define all-loops null)
  (define all-bridges null)
  (define jit-summary-lines null)
  (define jit-backend-count-lines null)

  (define (parse-seq close iter skip?)
    (define close-str (string-append " jit" close "}"))
    (for/fold ([next #f]
               [acc null]
               #:result (if (or (not next) skip?) (reverse acc) (reverse (cons next acc))))
              ([ln iter])
      (send status-bar set-value (add1 (send status-bar get-value)))
      #:break (string-contains? ln close-str)
      (values ln (if next (cons next acc) acc))))

  (define-syntax-rule (push! v l) (set! l (cons v l)))

  (with-input-from-file trace-file
    (lambda ()
      (define iter (in-lines))
      (for ([ln iter])
        (send status-bar set-value (add1 (send status-bar get-value)))
        (define r (regexp-match #rx"[[0-9a-zA-Z]+] {jit([-a-z]+)$" ln))
        (match (and r (cadr r))
          ["-log-opt-loop"   (push! (parse-seq (cadr r) iter #t) all-loops)]
          ["-log-opt-bridge" (push! (parse-seq (cadr r) iter #t) all-bridges)]
          ["-summary"        (set! jit-summary-lines (parse-seq (cadr r) iter #f))]
          ["-backend-counts" (set! jit-backend-count-lines (parse-seq (cadr r) iter #f))]
          [_ (void)]))))

  (send loading show #f)
  (eprintf "DONE in ")

  (define ordered-loop-lines (reverse all-loops))
  (define ordered-bridge-lines (reverse all-bridges))

  (values ordered-loop-lines
          ordered-bridge-lines
          jit-summary-lines
          jit-backend-count-lines))

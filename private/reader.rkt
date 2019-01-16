#lang racket/base

(require racket/string
         racket/system
         racket/port

         racket/class
         racket/gui/base)


(provide read-all)

(define (read-all trace-file)
  ;; Gather meta information (e.g. # of lines in the file)
  (define wc-output (with-output-to-string (lambda () (system (format "wc ~a" trace-file)))))
  (define number-of-lines (string->number (car (string-split (string-trim wc-output) " "))))

  (define all-loops null)
  (define all-bridges null)
  (define jit-summary-lines null)
  (define jit-backend-count-lines null)

  (define record-loop #f)
  (define record-bridge #f)
  (define record-summary #f)
  (define record-counts #f)

  (define start-rec-loop? #f)
  (define start-rec-bridge? #f)
  (define start-rec-summary? #f)
  (define start-rec-counts? #f)

  (define current-loop-lines null)
  (define current-bridge-lines null)
  (define current-summary-lines null)
  (define current-backend-count-lines null)

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

  (with-input-from-file trace-file
    (lambda ()
      (for ([ln (in-lines)])
        ;; these starts are to jump over the first line
        (when start-rec-loop?
          (set! record-loop #t))
        (when start-rec-bridge?
          (set! record-bridge #t))
        (when start-rec-summary?
          (set! record-summary #t))
        (when start-rec-counts?
          (set! record-counts #t))

        ;; enter recording a loop/bridge/summary
        (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-log-opt-loop" ln)
          (set! start-rec-loop? #t))
        (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-log-opt-bridge" ln)
          (set! start-rec-bridge? #t))
        (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-summary" ln)
          (set! start-rec-summary? #t))
        (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-backend-counts" ln)
          (set! start-rec-counts? #t))

        ;; exit recording loop/bridge/summary
        (when (and record-counts
                   (regexp-match #rx"[[0-9a-zA-Z]+] jit-backend-counts}" ln))
          (set! start-rec-counts? #f)(set! record-counts #f)
          (set! jit-backend-count-lines (reverse current-backend-count-lines))
          (set! current-backend-count-lines null))
        (when (and record-summary
                   (regexp-match #rx"[[0-9a-zA-Z]+] jit-summary}" ln))
          (set! start-rec-summary? #f)(set! record-summary #f)
          (set! jit-summary-lines (reverse current-summary-lines))
          (set! current-summary-lines null))
        (when (and record-loop
                   (regexp-match #rx"[[0-9a-zA-Z]+] jit-log-opt-loop}" ln))
          (set! start-rec-loop? #f)(set! record-loop #f)
          (set! all-loops (cons (reverse current-loop-lines) all-loops))
          (set! current-loop-lines null))
        (when (and record-bridge
                   (regexp-match #rx"[[0-9a-zA-Z]+] jit-log-opt-bridge}" ln))
          (set! start-rec-bridge? #f)(set! record-bridge #f)
          (set! all-bridges (cons (reverse current-bridge-lines) all-bridges))
          (set! current-bridge-lines null))

        ;; record
        (when record-loop
          (set! current-loop-lines (cons ln current-loop-lines)))
        (when record-bridge
          (set! current-bridge-lines (cons ln current-bridge-lines)))
        (when record-summary
          (set! current-summary-lines (cons ln current-summary-lines)))
        (when record-counts
          (set! current-backend-count-lines (cons ln current-backend-count-lines)))

        (send status-bar set-value (add1 (send status-bar get-value))))))

  (send loading show #f)
  (eprintf "DONE ...\n")

  (define ordered-loop-lines (reverse all-loops))
  (define ordered-bridge-lines (reverse all-bridges))

  (values ordered-loop-lines
          ordered-bridge-lines
          jit-summary-lines
          jit-backend-count-lines))

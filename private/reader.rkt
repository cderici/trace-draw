#lang racket/base

(require racket/string
         racket/system
         racket/port

         racket/class
         racket/gui/base)


(provide read-all)

(define (init-gui trace-file)
  ;; Gather meta information (e.g. # of lines in the file)
  (define wc-output (with-output-to-string (lambda () (system (format "wc ~a" trace-file)))))
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

  (define record-loop #f)
  (define record-bridge #f)
  (define record-summary #f)
  (define record-counts #f)

  (define current-loop-lines null)
  (define current-bridge-lines null)
  (define current-summary-lines null)
  (define current-backend-count-lines null)

  (with-input-from-file trace-file
    (lambda ()
      (for ([ln (in-lines)])
        ;; enter recording a loop/bridge/summary
        (when (and (not (or record-loop
                            record-bridge
                            record-summary
                            record-counts)) ; no nesting in the log
                   (regexp-match #rx"[[0-9a-zA-Z]+] {jit" ln))
          (when (string-contains? ln "-log-opt-loop")
            (set! record-loop #t))
          (when (string-contains? ln "-log-opt-bridge")
            (set! record-bridge #t))
          (when (string-contains? ln "-summary")
            (set! record-summary #t))
          (when (string-contains? ln "-backend-counts")
            (set! record-counts #t)))

        ;; record the line (or record the collection)
        (when record-loop
          (if (string-contains? ln " jit-log-opt-loop}")
              (begin ;; finish record
                (set! record-loop #f)
                (set! all-loops (cons (cdr (reverse (cdr current-loop-lines))) all-loops))
                (set! current-loop-lines null))
              ;; continue record
              (set! current-loop-lines (cons ln current-loop-lines))))

        (when record-bridge
          (if (string-contains? ln " jit-log-opt-bridge}")
              (begin
                (set! record-bridge #f)
                (set! all-bridges (cons (cdr (reverse (cdr current-bridge-lines))) all-bridges))
                (set! current-bridge-lines null))
              (set! current-bridge-lines (cons ln current-bridge-lines))))
        ;; (cdr (reverse (cdr pattern is for
        ;; removing the first and the last two lines when taking reverse
        ;; e.g.
        ;; [3e4c1616dbff0e8] {jit-log-opt-loop
        ;; +592: --end of the loop--
        ;; [3e4c1616dc9dc53] jit-log-opt-loop}

        (when record-summary
          (if (string-contains? ln " jit-summary}")
              (begin
                (set! record-summary #f)
                (set! jit-summary-lines (cdr (reverse current-summary-lines)))
                (set! current-summary-lines null))
              (set! current-summary-lines (cons ln current-summary-lines))))

        (when record-counts
          (if (string-contains? ln " jit-backend-counts}")
              (begin
                (set! record-counts #f)
                (set! jit-backend-count-lines (cdr (reverse current-backend-count-lines)))
                (set! current-backend-count-lines null))
              (set! current-backend-count-lines (cons ln current-backend-count-lines))))


        (send status-bar set-value (add1 (send status-bar get-value))))))

  (send loading show #f)
  (eprintf "DONE in ")

  (define ordered-loop-lines (reverse all-loops))
  (define ordered-bridge-lines (reverse all-bridges))

  (values ordered-loop-lines
          ordered-bridge-lines
          jit-summary-lines
          jit-backend-count-lines))

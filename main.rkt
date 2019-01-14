#lang racket/base

(require racket/runtime-path
         racket/cmdline
         racket/string
         "private/struct.rkt")

(define-runtime-module-path gui "private/gui.rkt")

(define entry-bridge-count 0)

(define (get-entry-bridge-id)
  (let ([new-id (format "entry-bridge-~a" entry-bridge-count)])
    (begin
      (set! entry-bridge-count (add1 entry-bridge-count))
      new-id)))

(define (get-label-id label-line-str)
  (let ([token-part (cadr (string-split label-line-str "TargetToken("))])
    (substring token-part 0 (- (string-length token-part) 2))))

(define (get-guard-info guard-line-str)
  (let* ([second-part (cadr (string-split guard-line-str "Guard"))]
         [id (car (string-split second-part ">)"))])
    (make-guard id guard-line-str)))

(define (get-jump-info jump-line-str)
  (get-label-id jump-line-str))

;; (listof string) -> trace
;; ASSUMES : entry bridges never contain labels (noone jumps to them)
;; ASSUMES : a trace never contains more than 2 "label"s (one for peeled and one for main)
(define (process-trace-lines trace-lines-str)
  ;; let's try to make it in a single pass

  (define is-entry-bridge? #f)
  (define outer-label #f)
  (define outer-label-line #f)
  (define inner-label #f)
  (define inner-label-line #f)
  (define are-we-in-inner-loop #f)

  (define outer-code null)
  (define inner-code null)

  (define outer-guards null)
  (define inner-guards null)

  (define jump #f)

  (define already-processed #f)
  (define racket-code #f)

  (for ([line-str (in-list trace-lines-str)])
    (set! already-processed #f)

    (when (and (not outer-label) (not is-entry-bridge?) (string-contains? line-str "entry bridge"))
      (set! is-entry-bridge? #t)
      (set! already-processed #t)
      (set! outer-label-line line-str)
      (set! racket-code line-str))

    (when (and (not racket-code) (string-contains? line-str "# Loop "))
      (set! racket-code line-str))

    (when (and (not is-entry-bridge?) outer-label (not inner-label) (string-contains? line-str "label("))
      (set! inner-label (get-label-id line-str))
      (set! are-we-in-inner-loop #t)
      (set! inner-label-line line-str)
      (set! already-processed #t))

    (when (and (not is-entry-bridge?) (not outer-label) (string-contains? line-str "label("))
      (set! outer-label (get-label-id line-str))
      (set! already-processed #t)
      (set! outer-label-line line-str))

    (when (and (not already-processed) (string-contains? line-str " guard_"))
      (let ([guard-info (get-guard-info line-str)])
        (if are-we-in-inner-loop
            (set! inner-guards (cons guard-info inner-guards))
            (set! outer-guards (cons guard-info outer-guards)))))

    (when (and (not already-processed) (not jump) (string-contains? line-str " jump("))
      (set! jump (get-jump-info line-str)))

    (cond
      [are-we-in-inner-loop (set! inner-code (cons line-str inner-code))]
      [else (set! outer-code (cons line-str outer-code))])
    )

  (define ordered-outer-guards (reverse outer-guards))
  (define ordered-inner-guards (reverse inner-guards))
  (define ordered-outer-code (reverse outer-code))
  (define ordered-inner-code (reverse inner-code))

  (if is-entry-bridge?
      (make-trace (get-entry-bridge-id)
                  outer-label-line
                  #t
                  #f
                  racket-code
                  -1
                  ordered-outer-guards
                  jump
                  (string-join ordered-outer-code "\n"))
      (let ([maybe-inner (if (not inner-label)
                             #f
                             (make-trace inner-label
                                         inner-label-line
                                         #f
                                         #f
                                         #f
                                         -1
                                         ordered-inner-guards
                                         jump
                                         (string-join ordered-inner-code "\n")))])
        (make-trace outer-label
                    outer-label-line
                    #f
                    maybe-inner
                    racket-code
                    -1
                    ordered-outer-guards
                    jump
                    (string-join (append ordered-outer-code ordered-inner-code) "\n")))))

(define (get-bridge-guard-id line-str)
  (let* ([second-part (cadr (string-split line-str "Guard "))])
    (car (string-split second-part " "))))

;; (listof string) -> bridge
;; ASSUMES : bridges don't contain labels (no-one jumps onto a bridge)
;; ASSUMES : bridges don't contain inner loops
(define (process-bridge-trace-lines trace-lines-str)

  (define guard-id #f)
  (define id-line #f)
  (define guards null)
  (define jump #f)
  (define code null)

  (for ([line-str (in-list trace-lines-str)])

    (when (and (not guard-id) (string-contains? line-str "bridge out of Guard "))
      (set! guard-id (get-bridge-guard-id line-str))
      (set! id-line line-str))

    (when (string-contains? line-str " guard_")
      (let ([guard-info (get-guard-info line-str)])
        (set! guards (cons guard-info guards))))

    (when (and (not jump) (string-contains? line-str " jump("))
      (set! jump (get-jump-info line-str)))

    (set! code (cons line-str code))

    )

  (define ordered-guards (reverse guards))
  (define ordered-code (reverse code))

  (make-bridge guard-id id-line ordered-guards -1 jump (string-join ordered-code "\n"))
  )


#;(current-command-line-arguments
 #("old-ack.trace"))

(define loops null)
(define bridges null)
(define summary null)
; just for debugging
(define (expose a b c)
  (set! loops a)
  (set! bridges b)
  (set! summary c))

(module+ main

  (define all-loops null)
  (define all-bridges null)
  (define jit-summary-lines null)

  (command-line
   #:args (trace-file)
   (define record-loop #f)
   (define record-bridge #f)
   (define record-summary #f)

   (define current-loop-lines null)
   (define current-bridge-lines null)
   (define current-summary-lines null)

   (with-input-from-file trace-file
     (lambda ()
       (for ([ln (in-lines)])
         ;; enter recording a loop/bridge/summary
         (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-log-opt-loop" ln)
           (set! record-loop #t))
         (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-log-opt-bridge" ln)
           (set! record-bridge #t))
         (when (regexp-match #rx"[[0-9a-zA-Z]+] {jit-summary" ln)
           (set! record-summary #t))

         ;; record
         (when record-loop
           (set! current-loop-lines (cons ln current-loop-lines)))
         (when record-bridge
           (set! current-bridge-lines (cons ln current-bridge-lines)))
         (when record-summary
           (set! current-summary-lines (cons ln current-summary-lines)))

         ;; exit recording loop/bridge/summary
         (when (and record-summary
                    (regexp-match #rx"[[0-9a-zA-Z]+] jit-summary}" ln))
           (set! record-summary #f)
           (set! jit-summary-lines (reverse current-summary-lines))
           (set! current-summary-lines null))
         (when (and record-loop
                    (regexp-match #rx"[[0-9a-zA-Z]+] jit-log-opt-loop}" ln))
           (set! record-loop #f)
           (set! all-loops (cons (process-trace-lines (reverse current-loop-lines)) all-loops))
           (set! current-loop-lines null))
         (when (and record-bridge
                    (regexp-match #rx"[[0-9a-zA-Z]+] jit-log-opt-bridge}" ln))
           (set! record-bridge #f)
           (set! all-bridges (cons (process-bridge-trace-lines (reverse current-bridge-lines)) all-bridges))
           (set! current-bridge-lines null))
         ))
     ))

  (define ordered-loops (reverse all-loops))
  (define ordered-bridges (reverse all-bridges))

  (expose ordered-loops ordered-bridges jit-summary-lines)

  ((dynamic-require gui 'make-gui)
   #:traces ordered-loops
   #:bridges ordered-bridges
   #:jit-summary jit-summary-lines)
  )

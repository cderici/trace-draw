#lang racket/base

(require racket/string
         racket/list
         "struct.rkt")

(provide pick-most-used-traces
         pick-bridges-for
         pre-process-trace-lines
         pre-process-bridge-lines
         process-jit-counts
         process-trace-lines
         process-bridge-lines)

(define entry-bridge-count 0)

(define (clear-trace-code-line line-str)
  (cond
    [(string-contains? line-str "debug_merge_point")
     (car (string-split (cadr (string-split line-str ", '")) "')"))]
    [(string-contains? line-str ": ")
     (string-join (cons "\t" (cdr (string-split line-str " "))) " ")]
    [else line-str]))

;; (listof trace-candidates) jit-counts number -> (listof traces)
(define (pick-most-used-traces candidates jit-counts lbl->counts n [no-count-info #f])
  (if no-count-info
      (map (lambda (c) (process-trace-lines c lbl->counts)) (hash-values candidates))
      ;; ASSUMES : jit-counts doesn't contain duplicate counts
      ;; (i.e. no two traces that are used the same many times) (cross-fingers)
      (let* ([l (hash-count jit-counts)]
             [chosen-counts (take (sort (hash-keys jit-counts) >) (min n l))]
             [chosen-labels (for/hash ([c (in-list chosen-counts)])
                              (values c (hash-ref jit-counts c)))])
        (for/fold ([traces null])
                  ([(lbls lines) (in-hash candidates)])
          (or (for/or ([(cnt cl) (in-hash chosen-labels)])
                (for/or ([l (in-list lbls)])
                  (and (equal? cl l)
                       (cons (process-trace-lines lines lbl->counts) traces))))
              traces)))))

(define (pick-bridges-for traces bridge-candidates)
  (define processed (make-hash))

  (define (get-guard-bridge-exits guards)
    (for/fold ([current null])
              ([g (in-list guards)])
      (let ((bridge-lines (hash-ref bridge-candidates (guard-id g) #f)))
        (if (and bridge-lines (not (hash-ref processed (guard-id g) #f)))
            (begin
              (hash-set! processed (guard-id g) #t)
              (cons (process-bridge-lines bridge-lines) current))
            current))))

  (define first-batch-bridges
    (for/fold ([bridges null])
              ([t (in-list traces)])
      (append bridges
              (get-guard-bridge-exits (append (trace-guards t)
                                       (if (not (trace-inner-loop t)) null
                                           (trace-guards (trace-inner-loop t))))))))

  ; bridges guard-exit to other bridges too
  (append first-batch-bridges
          (for/fold ([next null])
                    ([b (in-list first-batch-bridges)])
            (append next
                    (get-guard-bridge-exits (bridge-guards b))))))

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

;; (listof string) -> (hash label trace-lines-str)
;; extracts the label and uses it to label its input
(define (pre-process-trace-lines all-trace-lines)
  (for/fold ([trace-candidates (hash)])
            ([trace-lines-str (in-list all-trace-lines)])
    (define labels null)
    (define break? #f)

    (for ([line-str (in-list trace-lines-str)])
      #:break break?
      (when (string-contains? line-str "entry bridge")
        (let* ((splt (string-split line-str " "))
               (n-str (list-ref splt 2)))
          (unless (string->number n-str)
            (error (format
                    "couldn't get the loop number from : ~a\n"
                    line-str)))
          (set! labels (cons (string-append "Loop " n-str) labels)))
        (set! break? #t))

      (when (string-contains? line-str "label(")
        (set! labels (cons (get-label-id line-str) labels))
        (when (= (length labels) 2)
          (set! break? #t))))

    (hash-set trace-candidates labels trace-lines-str)))

;; (listof string) -> (hash label bridge-lines-str)
;; extracts the label and uses it to label its input
(define (pre-process-bridge-lines all-bridge-lines)
  (for/fold ([bridge-candidates (hash)])
            ([bridge-lines-str (in-list all-bridge-lines)])
    (define guard-id #f)
    (define break? #f)

    (for ([line-str (in-list bridge-lines-str)])
      #:break break?

      (when (string-contains? line-str "bridge out of Guard ")
        (set! guard-id (get-bridge-guard-id line-str))
        (set! break? #t)))

    (hash-set bridge-candidates guard-id bridge-lines-str)))

(define (process-jit-counts jit-count-lines)
  (for/fold ([count->lbl (hash)]
             [lbl->count (hash)])
            ([line-str (in-list jit-count-lines)])
    (cond
      [(string-contains? line-str "TargetToken(")
       (let* ([f (string-split line-str "):")]
              [cnt (string->number (cadr f))]
              [lbl (car (string-split (car f) "TargetToken("))])
         (values (hash-set count->lbl cnt lbl)
                 (hash-set lbl->count lbl cnt)))]
      [(string-contains? line-str "entry ")
       (let* ([f (string-split line-str ":")]
              [cnt (string->number (cadr f))]
              [lbl (format "Loop ~a" (cadr (string-split (car f) " ")))])
         (values (hash-set count->lbl cnt lbl)
                 (hash-set lbl->count lbl cnt)))]
      [else (values count->lbl lbl->count)])))

;; (listof string) -> trace
;; ASSUMES : entry bridges never contain labels (noone jumps to them)
;; ASSUMES : a trace never contains more than 2 "label"s (one for peeled and one for main)
(define (process-trace-lines trace-lines-str lbl->counts)
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
      (let* ((splt (string-split line-str " "))
               (n-str (list-ref splt 2)))
          (unless (string->number n-str)
            (error (format
                    "couldn't get the loop number from : ~a\n"
                    line-str)))
          (set! outer-label (string-append "Loop " n-str)))
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

    (define clean-line-str (clear-trace-code-line line-str))

    (cond
      [are-we-in-inner-loop (set! inner-code (cons clean-line-str inner-code))]
      [else (set! outer-code (cons clean-line-str outer-code))])
    )

  (define ordered-outer-guards (reverse outer-guards))
  (define ordered-inner-guards (reverse inner-guards))
  (define ordered-outer-code (reverse outer-code))
  (define ordered-inner-code (reverse inner-code))

  (let ([maybe-inner (if (not inner-label)
                         #f
                         (make-trace inner-label
                                         inner-label-line
                                         #f
                                         #f
                                         #f
                                         (hash-ref lbl->counts inner-label -1)
                                         ordered-inner-guards
                                         jump
                                         (string-join ordered-inner-code "\n")))])
    (make-trace outer-label
                outer-label-line
                is-entry-bridge?
                maybe-inner
                racket-code
                (hash-ref lbl->counts outer-label -1)
                ordered-outer-guards
                jump
                (string-join (append ordered-outer-code ordered-inner-code) "\n"))))

(define (get-bridge-guard-id line-str)
  (let* ([second-part (cadr (string-split line-str "Guard "))])
    (car (string-split second-part " "))))

;; (listof string) -> bridge
;; ASSUMES : bridges don't contain labels (no-one jumps onto a bridge)
;; ASSUMES : bridges don't contain inner loops
(define (process-bridge-lines trace-lines-str)

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

    (define clean-line-str (clear-trace-code-line line-str))

    (set! code (cons clean-line-str code))

    )

  (define ordered-guards (reverse guards))
  (define ordered-code (reverse code))

  (make-bridge guard-id id-line ordered-guards -1 jump (string-join ordered-code "\n"))
  )

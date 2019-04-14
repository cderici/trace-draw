#lang racket/base

(require racket/string
         racket/list
         "struct.rkt"
         "config.rkt")

(provide pick-most-used-traces
         pick-bridges-for
         pre-process-trace-lines
         pre-process-bridge-lines
         process-jit-counts
         process-trace-lines
         process-bridge-lines)

(define entry-bridge-count 0)

;; (hash ) jit-counts number -> (listof traces)
(define (pick-most-used-traces candidates jit-counts lbl->counts N
                               trace-blocks extra-entry-bridges
                               bridge-candidates [no-count-info #f])
  (if no-count-info
      (map (lambda (c) (process-trace-lines c lbl->counts bridge-candidates)) (hash-values candidates))
      ;; ASSUMES : jit-counts doesn't contain duplicate counts
      ;; (i.e. no two traces that are used the same many times) (cross-fingers)
      (let* ([chosen-blocks
              (map cdr
                   (take
                    (sort
                     (for/fold ([block-counts null])
                               ([block (in-list trace-blocks)])
                       (let ([cnt (+ (hash-ref lbl->counts (trace-block-entry-label block))
                                     (hash-ref lbl->counts (trace-block-outer-label block))
                                     (hash-ref lbl->counts (trace-block-inner-label block)))])
                         (cons (cons cnt block) block-counts)))
                     (lambda (a b) (> (car a) (car b))))
                    (min N (length trace-blocks))))]
             ;; if trace-blocks are less than N, try to get more from the extra entry bridges
             [from-extra-entry
              (if (>= (length trace-blocks) N)
                  null
                  (map (lambda (chosen-l-c)
                         (process-trace-lines
                          (hash-ref candidates (list (car chosen-l-c)))
                          lbl->counts bridge-candidates))
                       (take
                        (sort
                         (map (lambda (ebl) (cons ebl (hash-ref lbl->counts ebl))) extra-entry-bridges)
                         (lambda (a b) (> (cdr a) (cdr b))))
                        (min (- N (length trace-blocks)) (length extra-entry-bridges)))))])

        (append
         (for/fold ([traces null])
                   ([block (in-list chosen-blocks)])
           (let ([block-lines
                  (hash-ref candidates
                            (list (trace-block-inner-label block) (trace-block-outer-label block)))]
                 [entry-lines
                  (hash-ref candidates (list (trace-block-entry-label block)) #f)])
             (if entry-lines
                 (cons (process-trace-lines block-lines lbl->counts bridge-candidates)
                       (cons
                        (process-trace-lines entry-lines lbl->counts bridge-candidates)
                        traces))
                 (cons (process-trace-lines block-lines lbl->counts bridge-candidates)
                       traces))))
         from-extra-entry))))

(define (pick-bridges-for traces bridge-candidates lbl->counts)
  (define processed (make-hash))

  (define (get-guard-bridge-exits guards)
    (for/fold ([current null])
              ([g (in-list guards)])
      (let ((bridge-lines (hash-ref bridge-candidates (guard-id g) #f)))
        (if (and bridge-lines (not (hash-ref processed (guard-id g) #f)))
            (begin
              (hash-set! processed (guard-id g) #t)
              (cons (process-bridge-lines bridge-lines bridge-candidates lbl->counts) current))
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

(define (get-entry-bridge-label-id line-str)
  (let* ((splt (string-split line-str " "))
         (n-str (list-ref splt 2)))
    (unless (string->number n-str)
      (error (format
              "couldn't get the loop number from : ~a\n"
              line-str)))
    (string-append "Loop " n-str)))

(define (get-label-id label-line-str)
  (let ([token-part (cadr (string-split label-line-str "TargetToken("))])
    (substring token-part 0 (- (string-length token-part) 2))))

(define (extract-guard guard-line-str bridge-candidates belongs-lbl)
  (let* ([type (let ([t (regexp-match #px"guard[\\w]*" guard-line-str)]) (and t (car t)))]
         [id (let ([i (regexp-match #px"0x[\\w]+" guard-line-str)]) (and i (car i)))]
         [args (let* ([<str> (let ([s (regexp-match #px"\\(.*\\)" guard-line-str)]) (and s (car s)))]
                      [str (and <str> (substring <str> 1 (sub1 (string-length <str>))))]
                      [args-ls (string-split str ", ")]) ; args are only a few
                 (if (null? (cdr args-ls)) null (reverse (cdr (reverse args-ls)))))]
         [jump-params* (let ([p (regexp-match #px"\\[.*\\]" guard-line-str)]) (and p (car p)))]
         [jump-params
          (string-split (substring jump-params* 1 (sub1 (string-length jump-params*))) ", ")]
         [bridge? (hash-has-key? bridge-candidates id)])
    (make-guard id guard-line-str type args jump-params bridge? belongs-lbl)))

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
        (set! labels (cons (get-entry-bridge-label-id line-str) labels))
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

(define (process-jit-counts jit-count-lines candidates)
  ;; (define-struct trace-block
  ;;   (entry-number outer-label inner-label))
  (define seen-entry #f)
  (define seen-token #f)

  (for/fold ([blocks null]
             [extra-entry-bridges null]
             [trace-count 0]
             [count->lbl (hash)]
             [lbl->count (hash)])
            ([line-str (in-list jit-count-lines)])
    (cond
      [(string-contains? line-str "TargetToken(")
       (let* ([f (string-split line-str "):")]
              [cnt (string->number (cadr f))]
              [lbl (car (string-split (car f) "TargetToken("))])
         (if seen-token
             (let ([new-blocks (cons (make-trace-block seen-entry seen-token lbl) blocks)])
               (set! seen-entry #f)
               (set! seen-token #f)
               (values new-blocks
                       extra-entry-bridges
                       trace-count
                       (hash-set count->lbl cnt lbl)
                       (hash-set lbl->count lbl cnt)))
             (begin
               (set! seen-token lbl)
               (values blocks
                       extra-entry-bridges
                       trace-count
                       (hash-set count->lbl cnt lbl)
                       (hash-set lbl->count lbl cnt)))))]
      [(string-contains? line-str "entry ")
       (let* ([f (string-split line-str ":")]
              [cnt (string->number (cadr f))]
              [lbl (format "Loop ~a" (cadr (string-split (car f) " ")))])
         (let ([new-b (if seen-entry
                          (if (hash-ref candidates (list seen-entry) #f)
                              (cons seen-entry extra-entry-bridges)
                              extra-entry-bridges)
                          extra-entry-bridges)])
           (set! seen-entry lbl)
           (set! seen-token #f)
           (values blocks
                   new-b
                   (add1 trace-count)
                   (hash-set count->lbl cnt lbl)
                   (hash-set lbl->count lbl cnt))))]
      [(string-contains? line-str "bridge ")
       (let* ([nums (regexp-match* #px"[0-9]+" line-str)]
              [lbl (string-append
                    "0x" (number->string
                          (string->number (car nums)) 16))]
              [cnt (string->number (cadr nums))])
         (set! seen-entry #f)
         (set! seen-token #f)
         (values blocks
                 extra-entry-bridges
                 trace-count
                 (hash-set count->lbl cnt lbl)
                 (hash-set lbl->count lbl cnt)))]
      [else (values blocks extra-entry-bridges trace-count count->lbl lbl->count)])))

(define (process-trace-internals trace-lines-str bridge-candidates [for-a-bridge? #f])
  (for/fold ([is-entry-bridge? #f]
             [outer-label #f]
             [inner-label #f]
             [jump #f]
             [are-we-in-inner-loop #f]
             [outer-code null]
             [inner-code null]
             [outer-guards null]
             [inner-guards null])
            ([line-str (in-list trace-lines-str)])
    ;; figure out the line
    (define tline
      (cond
        ;; info-tline
        [(char=? (string-ref line-str 0) #\#) (make-info-tline line-str)]
        ;; param-tline
        [(char=? (string-ref line-str 0) #\[)
         (let ([params (string-split
                        (substring line-str 1 (sub1 (string-length line-str))) ", ")])
           (make-param-tline params))]
        ;; debug-merge-point
        [(string-contains? line-str "debug_merge_point")
         (make-debug-merge-point (car (string-split (cadr (string-split line-str ", '")) "')")))]
        ;; guard-tline
        [(string-contains? line-str " guard_")
         (let ([belongs-lbl (if are-we-in-inner-loop inner-label outer-label)])
           (extract-guard line-str bridge-candidates belongs-lbl))]
        ;; assignment-tline
        [(string-contains? line-str " = ")
         (let ([usual-line? (regexp-match #px" [\\w]+ " line-str)])
           (if usual-line?
               (let ([lhs (string-trim (car (regexp-match #px" [\\w]+ " line-str)))]
                     [op (let ([o (regexp-match* #px"[\\w]+" line-str)]) (and o (list-ref o 2)))]
                     [args (let ([a (regexp-match #px"\\(.*\\)" line-str)])
                             (and a (string-split (substring (car a) 1 (sub1 (string-length (car a)))) ", ")))])
                 (make-assignment-tline lhs op args))
               (let* ([splt (string-split line-str " = ")]
                      [lhs (car splt)]
                      [op (cadr splt)]
                      [args null])
                 (make-assignment-tline lhs op args))))]
        ;; operation-t-line
        [(regexp-match #px"[\\w]+\\(.*\\)" line-str)
         => (lambda (ln) (and ln
                              (let ([op (let ([o (regexp-match #px"[\\w]+" (car ln))])
                                          (and o (car o)))]
                                    [args (let ([a (regexp-match #px"\\(.*\\)" line-str)])
                                            (and a (string-split (substring (car a) 1 (sub1 (string-length (car a)))) ", ")))])
                                (let ([clean-args
                                       (if (and (not (equal? op "label")) (not (equal? op "jump")))
                                           args
                                           (for/list ([ar (in-list args)])
                                             (if (string-contains? ar "descr=TargetToken")
                                                 (substring ar 6) ar)))])
                                  (make-operation-tline op clean-args)))))]
        [else (error 'trace-line
                     (format "couldn't recognize this line :\n~a\n" line-str))]))

    ;; adjust parameters and go on
    (values (or is-entry-bridge? (and (info-tline? tline) (string-contains? line-str "entry bridge")))
            (or outer-label (or (and (not for-a-bridge?)
                                     (not are-we-in-inner-loop)
                                     (or (and (is-label? tline)
                                              (get-label-id line-str))
                                         (and (is-entry-bridge-label? tline)
                                              (get-entry-bridge-label-id line-str))))
                                (and for-a-bridge?
                                     (is-bridge-label? tline)
                                     (get-bridge-guard-id line-str))))
            (or inner-label (and outer-label (is-label? tline) (get-label-id line-str)))
            (or jump (and (is-jump? tline) (get-jump-info line-str)))
            (or are-we-in-inner-loop (and outer-label (is-label? tline)))
            (or (and (not are-we-in-inner-loop) (cons tline outer-code)) outer-code)
            (or (and are-we-in-inner-loop (cons tline inner-code)) inner-code)
            (or (and (not are-we-in-inner-loop) (guard? tline) (cons tline outer-guards)) outer-guards)
            (or (and are-we-in-inner-loop (guard? tline) (cons tline inner-guards)) inner-guards))))

;; (listof string) -> trace
;; ASSUMES : entry bridges never contain labels (noone jumps to them)
;; ASSUMES : a trace never contains more than 2 "label"s (one for peeled and one for main)
(define (process-trace-lines trace-lines-str lbl->counts bridge-candidates)
  ;; let's try to make it in a single pass

  (define-values
    (is-entry-bridge? outer-label inner-label jump are-we-in-inner-loop outer-code inner-code outer-guards inner-guards)
    (process-trace-internals trace-lines-str bridge-candidates))

  (define ordered-outer-guards (reverse outer-guards))
  (define ordered-inner-guards (reverse inner-guards))
  (define ordered-outer-code (reverse outer-code))
  (define ordered-inner-code (reverse inner-code))

  (let ([maybe-inner (if (not inner-label)
                         #f
                         (make-trace inner-label
                                     #f
                                     #f
                                     ordered-inner-code
                                     (hash-ref lbl->counts inner-label -1)
                                     ordered-inner-guards
                                     jump))])
    (make-trace outer-label
                is-entry-bridge?
                maybe-inner
                (append ordered-outer-code ordered-inner-code)
                (hash-ref lbl->counts outer-label -1)
                ordered-outer-guards
                jump)))

(define (get-bridge-guard-id line-str)
  (let* ([second-part (cadr (string-split line-str "Guard "))])
    (car (string-split second-part " "))))

;; (listof string) -> bridge
;; ASSUMES : bridges don't contain labels (no-one jumps onto a bridge)
;; ASSUMES : bridges don't contain inner loops
(define (process-bridge-lines trace-lines-str bridge-candidates lbl->counts)

  (define-values (_ label __ jump ___ code ____ guards _____)
    (process-trace-internals trace-lines-str bridge-candidates #t))

  (define ordered-guards (reverse guards))
  (define ordered-code (reverse code))
  (define use-count (hash-ref lbl->counts label #f))
  (make-bridge label ordered-guards use-count ordered-code jump)
  )

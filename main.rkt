#lang racket/base

(require racket/runtime-path
         racket/cmdline
         "private/reader.rkt")

(provide trace-draw)

(define-runtime-module-path gui "private/gui.rkt")

(define (trace-draw trace-file max-trace-shown)
  (define-values (loop-lines bridge-lines jit-summary-lines jit-backend-count-lines)
    (time (read-all trace-file)))

  ((dynamic-require gui 'make-gui)
   #:traces loop-lines
   #:bridges bridge-lines
   #:max-trace-shown max-trace-shown
   #:jit-summary jit-summary-lines
   #:jit-counts jit-backend-count-lines
   #:trace-file-name trace-file))

(module+ main
  (define max-trace-shown 20)

  (command-line
   #:once-each
   [("--max-trace") num "Maximum number of most used traces to show in gui" (set! max-trace-shown num)]
   #:args (trace-file)
   (unless (file-exists? trace-file)
     (error "trace-draw : No such file or directory"))
   (unless (string->number max-trace-shown)
     (error "trace-draw --max-trace : expected number"))

   (trace-draw trace-file (string->number max-trace-shown)))
  )

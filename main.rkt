#lang racket/base

(require racket/runtime-path
         racket/cmdline
         "private/reader.rkt")

(provide trace-draw)

(define-runtime-module-path gui "private/gui.rkt")

(define (trace-draw trace-file)
  (define-values (loop-lines bridge-lines jit-summary-lines jit-backend-count-lines)
    (read-all trace-file))

  ((dynamic-require gui 'make-gui)
   #:traces loop-lines
   #:bridges bridge-lines
   #:jit-summary jit-summary-lines
   #:jit-counts jit-backend-count-lines))

(module+ main

  (command-line
   #:args (trace-file)
   (unless (file-exists? trace-file)
     (error "trace-draw : No such file or directory"))
   (trace-draw trace-file))
  )

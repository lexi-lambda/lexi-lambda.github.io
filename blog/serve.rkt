#lang racket/base

(require racket/runtime-path
         web-server/http/empty
         web-server/servlet-env)

(provide start-server)

(define-runtime-path output-dir "../output")

(define (start-server)
  (serve/servlet (λ (req) (response/empty #:code 404))
                 #:extra-files-paths (list (simplify-path output-dir #f))
                 #:port 3000
                 #:command-line? #t
                 #:banner? #t))

(module+ main
  (start-server))

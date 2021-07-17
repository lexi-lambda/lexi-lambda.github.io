#lang racket/base

(require web-server/http/empty
         web-server/servlet-env
         "../paths.rkt")

(provide start-server)

(define (start-server)
  (serve/servlet (λ (req) (response/empty #:code 404))
                 #:extra-files-paths (list output-dir)
                 #:port 3000
                 #:command-line? #t
                 #:banner? #t))

(module+ main
  (start-server))

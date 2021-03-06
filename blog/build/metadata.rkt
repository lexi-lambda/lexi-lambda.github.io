#lang racket/base

(require racket/contract
         racket/serialize
         (only-in xml xexpr/c)

         "../paths.rkt"
         "../lang/metadata.rkt")

(provide (struct-out rendered-post)
         (contract-out
          [rendered-post-path (->* [rendered-post?] [#:file? any/c] site-path?)]))

(serializable-struct rendered-post (title-str title date tags body) #:transparent
  #:guard (struct-guard/c string?
                          (listof xexpr/c)
                          post-date?
                          (listof string?)
                          (listof xexpr/c)))

(define (rendered-post-path post #:file? [file? #f])
  (post-path (rendered-post-date post)
             (rendered-post-title-str post)
             #:file? file?))

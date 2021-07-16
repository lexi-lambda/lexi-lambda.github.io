#lang racket/base

(require racket/contract
         racket/format
         racket/string)

(provide (contract-out
          [index-path (->* [(and/c exact-integer? (>=/c 1))] [#:file? any/c] string?)]
          [tag-index-path (->* [string?] [(and/c exact-integer? (>=/c 1))] string?)]))

(define (index-path page-number #:file? [file? #f])
  (if (= page-number 1)
      (if file? "/index.html" "/")
      (~a "/index-" page-number ".html")))

(define (tag-index-path tag-str [page-number 1])
  (~a "/tags/"
      (string-replace tag-str " " "-")
      (if (= page-number 1) "" (~a "-" page-number))
      ".html"))

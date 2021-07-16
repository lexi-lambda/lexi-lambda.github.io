#lang racket/base

(require racket/contract
         racket/format
         racket/string)

(provide (contract-out
          [index-path (-> (and/c exact-integer? (>=/c 1)) string?)]
          [tag-index-path (-> string? string?)]))

(define (index-path page-number)
  (if (= page-number 1) "/" (~a "/index-" page-number ".html")))

(define (tag-index-path tag-str)
  (string-append "/tags/" (string-replace tag-str " " "-") ".html"))

#lang racket/base

(require racket/contract
         racket/string)

(provide (contract-out
          [tag-index-path (-> string? string?)]))

(define (tag-index-path tag-str)
  (string-append "/tags/" (string-replace tag-str " " "-") ".html"))

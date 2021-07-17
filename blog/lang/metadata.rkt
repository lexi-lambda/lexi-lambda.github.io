#lang racket/base

(require racket/contract
         racket/format
         racket/serialize
         racket/string)

(provide (struct-out post-date)
         (struct-out post-tags)
         (contract-out
          [post-date->string (-> post-date? string?)]
          [post-date->strings (-> post-date? (list/c string? string? string?))]))

(serializable-struct post-date (year month day) #:transparent
  #:guard (struct-guard/c exact-integer? (integer-in 1 12) (integer-in 1 31)))
(struct post-tags (tags) #:transparent
  #:guard (struct-guard/c (listof string?)))

(define (post-date->string date)
  (string-join (post-date->strings date) "-"))

(define (post-date->strings date)
  (list (~a (post-date-year date))
        (~r #:min-width 2 #:pad-string "0" (post-date-month date))
        (~r #:min-width 2 #:pad-string "0" (post-date-day date))))

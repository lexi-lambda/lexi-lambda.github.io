#lang racket/base

(require racket/contract
         racket/format
         racket/serialize
         racket/string
         scribble/core
         scribble/tag)

(provide (struct-out post-date)
         (struct-out post-tags)
         (struct-out link-target)
         (contract-out
          [post-date->string (-> post-date? string?)]
          [post-date->strings (-> post-date? (list/c string? string? string?))]

          [taglet-add-prefix (-> (or/c (or/c string? symbol?)
                                       (listof (or/c string? symbol?)))
                                 taglet?
                                 taglet?)]
          [blog-post-path->tag-prefix (-> (and/c string? relative-path?) string?)]))

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

;; A style property used to mark an arbitrary block or element as a link
;; destination, as if it were a `target-element` with the given tag.
(serializable-struct link-target (tag) #:transparent
  #:guard (struct-guard/c tag?))

(define (taglet-add-prefix prefix taglet)
  (if (list? prefix)
      (foldr taglet-add-prefix taglet prefix)
      (if (list? taglet)
          (cons prefix taglet)
          (list prefix taglet))))

(define (blog-post-path->tag-prefix path)
  (~a `(blog ,path)))

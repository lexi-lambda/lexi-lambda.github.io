#lang racket/base

(require racket/contract
         racket/format
         racket/serialize
         racket/string
         scribble/core
         scribble/tag)

(provide (struct-out post-date)

         (contract-out
          (struct post-tags ([tags (listof string?)]))
          [post-date->string (-> post-date? string?)]
          [post-date->strings (-> post-date? (list/c string? string? string?))]

          (struct link-target ([tag tag?]))
          (struct css-styles ([assoc (listof (cons/c symbol? string?))]))
          (struct table-rows ([styles (listof style?)]))

          [taglet-add-prefix (-> (or/c (or/c string? symbol?)
                                       (listof (or/c string? symbol?)))
                                 taglet?
                                 taglet?)]
          [blog-post-path->tag-prefix (-> (and/c string? relative-path?) string?)]))

(serializable-struct post-date (year month day) #:transparent
  #:guard (struct-guard/c exact-integer? (integer-in 1 12) (integer-in 1 31)))
(struct post-tags (tags) #:transparent)

(define (post-date->string date)
  (string-join (post-date->strings date) "-"))

(define (post-date->strings date)
  (list (~a (post-date-year date))
        (~r #:min-width 2 #:pad-string "0" (post-date-month date))
        (~r #:min-width 2 #:pad-string "0" (post-date-day date))))

;; A style property used to mark an arbitrary block or element as a link
;; destination, as if it were a `target-element` with the given tag.
(struct link-target (tag) #:transparent)

;; A style property like `attributes`, but for inline CSS styles. Unlike using
;; an `attributes` property with a `style` entry, multiple `css-styles`
;; properties are properly merged.
(struct css-styles (assoc) #:transparent)

;; A style property like `table-cells` and `table-columns`, but its styles
;; apply to the table’s <tr> elements.
(struct table-rows (styles) #:transparent)

(define (taglet-add-prefix prefix taglet)
  (if (list? prefix)
      (foldr taglet-add-prefix taglet prefix)
      (if (list? taglet)
          (cons prefix taglet)
          (list prefix taglet))))

(define (blog-post-path->tag-prefix path)
  (~a `(blog ,path)))

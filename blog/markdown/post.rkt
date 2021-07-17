#lang racket/base

(require data/applicative
         data/monad
         megaparsack
         megaparsack/text
         racket/contract
         (only-in scribble/base title)
         (only-in scribble/core part? style)
         threading

         "../lang/metadata.rkt"
         (only-in "parse.rkt" document/p)
         (only-in "parse/content.rkt" content/p simplify-content)
         "parse/util.rkt"
         "to-scribble.rkt")

(provide (contract-out
          [parse-markdown-post (->* [string?] [any/c] part?)]))

;; Parses a markdown blog post, using a header of indented lines to determine
;; the title, date, and tags.
(define (parse-markdown-post str [src-name 'string])
  (parse-result! (parse-string post/p str src-name)))

(define post/p
  (do (string/p "    Title: ")
      [title-content <- (map/p simplify-content (many+/p content/p))]
      newline/p

      (string/p "    Date: ")
      [year <- (digits/p 4)]
      (char/p #\-)
      [month <- (digits/p 2)]
      (char/p #\-)
      [day <- (digits/p 2)]
      rest-of-line/p

      (string/p "    Tags: ")
      [tags <- (many+/p (many+/p (char-not-in/p ",\n")) #:sep (string/p ", "))]
      newline/p
      newline/p

      [doc <- document/p]
      (define title-info (title #:style (style #f (list (post-date year month day)
                                                        (post-tags (map list->string tags))))
                                (content->content title-content)))
      (pure (document->part doc title-info))))

(define (digits/p n)
  (map/p (λ~> list->string string->number)
         (repeat/p n (char-between/p #\0 #\9))))

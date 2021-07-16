#lang racket/base

(require data/applicative
         data/monad
         megaparsack
         megaparsack/text
         racket/contract
         (only-in scribble/base title)
         (only-in scribble/core document-date part? style)
         (only-in "../scribble/post-language.rkt" post-tags)
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
      [year <- (repeat/p 4 (char-between/p #\0 #\9))]
      (char/p #\-)
      [month <- (repeat/p 2 (char-between/p #\0 #\9))]
      (char/p #\-)
      [day <- (repeat/p 2 (char-between/p #\0 #\9))]
      rest-of-line/p

      (string/p "    Tags: ")
      [tags <- (many+/p (many+/p (char-not-in/p ",\n")) #:sep (string/p ", "))]
      newline/p
      newline/p

      [doc <- document/p]
      (define date (list->string `[,@year #\- ,@month #\- ,@day]))
      (define title-info (title #:style (style #f (list (document-date date)
                                                        (post-tags (map list->string tags))))
                                (content->content title-content)))
      (pure (document->part doc title-info))))

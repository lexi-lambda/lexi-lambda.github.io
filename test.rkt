#lang at-exp racket/base

(require racket/pretty
         (only-in scribble/base title)
         (only-in scribble/core style document-date)
         scribble/render
         setup/xref

         (only-in "blog/scribble/post-language.rkt" post-tags)
         "blog/scribble/render.rkt")

#;(begin
    (require (only-in "blog/posts/2021-03-25-an-introduction-to-typeclass-metaprogramming.scrbl" [doc intro-to-tmp]))
    (pretty-print intro-to-tmp))

(begin
  (require racket/file
           megaparsack
           megaparsack/text
           "blog/markdown/parse.rkt"
           "blog/markdown/to-scribble.rkt")
  (define intro-to-tmp-str (file->string "/home/alexis/code/blog/_src/posts/2021-03-25-an-introduction-to-typeclass-metaprogramming.md"))
  (define intro-to-tmp-md (parse-result! (parse-string document/p intro-to-tmp-str)))
  (define intro-to-tmp (document->part intro-to-tmp-md
                                       (title #:style (style #f (list (document-date "2021-03-25")
                                                                      (post-tags (list "haskell" "types" "functional programming"))))
                                              "An introduction to typeclass metaprogramming"))))

(render (list intro-to-tmp) (list "an-introduction-to-typeclass-metaprogramming")
        #:render-mixin render-mixin
        #:dest-dir "output"
        #:xrefs (list (load-collections-xref))
        #:info-out-file "output/an-introduction-to-typeclass-metaprogramming.sxref")

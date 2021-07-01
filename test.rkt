#lang at-exp racket/base

(require racket/pretty
         scribble/render
         setup/xref

         "blog/scribble/render.rkt")

(module document1 scribble/doclang2 @begin{
  @(require scribble/manual)

  @title{Test Document}

  Hello, world! This text is @italic{italic}, and this text is @bold{bold}. This text uses
  @tt{typewriter font}. This text uses @italic{@bold{@tt{all at once}}}.

  Here is a new paragraph.

  @itemlist[
    @item{This is the first element of an unordered list.}
    @item{This is the second element of an unordered list.}]

  @itemlist[#:style 'ordered
    @item{This is the first element of an ordered list.}
    @item{This is the second element of an ordered list.}]

  This defines a @deftech{technical term}.

  This references the @tech{technical term}.

  This references @seclink["top" #:doc '(lib "scribblings/reference/reference.scrbl")]{some other
  document}.
  })

(module document2 scribble/doclang2 @begin{
  @(require scribble/manual)
  @title[#:style 'toc]{Bar}
  @;@section{Bar 1}
  This references the @tech{technical term} from another document.})

(require (only-in 'document1 [doc document1])
         (only-in 'document2 [doc document2])
         (only-in "blog/posts/2021-03-25-an-introduction-to-typeclass-metaprogramming.scrbl" [doc intro-to-tmp]))

#|
(pretty-print document1)
(pretty-print document2)

(render (list document1) (list "foo")
        #:render-mixin render-mixin
        #:dest-dir "output"
        #:xrefs (list (load-collections-xref))
        #:info-out-file "output/foo.sxref"
        #:directory-depth 100)

(render (list document2) (list "bar")
        #:render-mixin render-mixin
        #:dest-dir "output"
        #:xrefs (list (load-collections-xref))
        #:info-in-files (list "output/foo.sxref")
        #:info-out-file "output/bar.sxref"
        #:directory-depth 100)
|#

(pretty-print intro-to-tmp)

(render (list intro-to-tmp) (list "an-introduction-to-typeclass-metaprogramming")
        #:render-mixin render-mixin
        #:dest-dir "output"
        #:xrefs (list (load-collections-xref))
        #:info-out-file "output/an-introduction-to-typeclass-metaprogramming.sxref")

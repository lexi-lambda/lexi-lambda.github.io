#lang scribble/base
@(begin
   (require racket/list
            scribble/core
            scribble/html-properties
            threading
            "../lang/post-language.rkt")

   (define Scribble @seclink["top" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{Scribble}))

@title{About me}

My name is Alexis King, and I write @hyperlink["https://github.com/lexi-lambda?tab=repositories"]{a lot of software}. I currently live in Chicago.

I’m interested in functional programming, static types, and programming language research, and I try to spend as much time as I can writing Haskell and Racket. I write about some of the things I do @hyperlink["/"]{on this blog}, and I sometimes tweet about them and other things @hyperlink["https://twitter.com/lexi_lambda"]{on Twitter}. I work on a @emph{lot} of open-source projects @hyperlink["https://github.com/lexi-lambda"]{on GitHub}, and you can email me at @hyperlink["mailto:lexi.lambda@gmail.com" "lexi.lambda@gmail.com"].

@section{Things I’ve worked on}

This list is just a smattering of the cooler things I’ve worked on over the past few years. It is neither exhaustive nor particularly scientific in its curation.

@(define separator @elem[#:style "about-me-year-separator" "⁓"])
@(define (year-events year . items)
   @para[#:style "about-me-year-events"]{@elem[#:style "about-me-year-label" year] @separator
     @(~> (map (λ (item) @elem[#:style "about-me-year-entry" item]) items)
          (add-between (list " " separator " ") #:splice? #t))})

@year-events["2021"
  @list{Reimplemented @github-repo*["lexi-lambda.github.io"]{this blog} on top of @Scribble, replacing @github-repo{greghendershott/frog}.}
  @list{Released @github-repo{megaparsack} v1.5, adding support for user-defined parser state and parser lookahead.}]

@year-events["2020"
  @list{Published @hyperlink["https://dl.acm.org/doi/abs/10.1145/3428297"]{“Macros for DSLs”} with Michael Ballantyne and Matthias Felleisen at OOPSLA.}
  @list{Presented @hyperlink["https://www.youtube.com/watch?v=0jI-AlWEwYI"]{“Effects for Less”} at ZuriHac 2020 on the design of @github-repo{hasura/eff} and its accompanying @hyperlink["https://github.com/ghc-proposals/ghc-proposals/pull/313"]{GHC proposal} to add delimited continuations to the RTS.}
  @list{Led a large-scale refactoring effort to statically rule out several classes of bugs in @github-repo{hasura/graphql-engine}.}
  @list{Opened @hyperlink["https://github.com/ghc-proposals/ghc-proposals/pull/303"]{a GHC proposal} for improving arrow notation.}
  @list{Started contributing to GHC, including some @hyperlink["https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3041"]{performance fixes} and @hyperlink["https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3115"]{improvements to arrow notation}.}]

@year-events["2019"
  @list{Published @hyperlink["https://dl.acm.org/doi/abs/10.1145/3371133"]{“Does blame shifting work?”} with Lukas Lazarek, Samanvitha Sundar, Robby Findler, and Christos Dimoulas at POPL.}
  @list{Released @hackage-package{monad-validate} based on work done for Hasura.}
  @list{Started working at @hyperlink["https://hasura.io/"]{Hasura}.}]

@year-events["2018"
  @list{Presented @hyperlink["https://www.youtube.com/watch?v=g6UCeHiKodo"]{“Hackett: a metaprogrammable Haskell”} at Curry On, and again a few months later @hyperlink["https://www.youtube.com/watch?v=5QQdI3P7MdY"]{at Strange Loop}.}
  @list{Started working on contract systems with Christos Dimoulas at Northwestern University.}]

@year-events["2017"
  @list{Released the @hackage-package{freer-simple} package.}
  @list{Presented @hyperlink["https://www.youtube.com/watch?v=bOUgXd9XlJ4"]{“Hackett, a Haskell for Racketeers”} at RacketCon.}
  @list{Started working on @github-repo*["hackett"]{Hackett}, a Haskell-like language embedded in Racket.}]

@year-events["2016"
  @list{Started learning about type systems to explore ideas inspired by @seclink["top" #:doc '(lib "turnstile/scribblings/turnstile.scrbl") #:indirect? #t]{Turnstile}.}
  @list{Presented @hyperlink["https://www.youtube.com/watch?v=TfehOLha-18"]{“Languages in an Afternoon”} at RacketCon.}
  @list{Released the @github-repo{megaparsack} and @github-repo{scripty} Racket libraries.}
  @list{Started writing Haskell professionally for the first time.}]

@section{About this blog}

This blog is powered by @Scribble, an unusually flexible document preparation system written in @hyperlink["https://racket-lang.org/"]{Racket}. Unlike most markup languages, every Scribble document is a @emph{program} that evaluates to a document, which can then be rendered using one of a number of different backends. Scribble provides a TeX-like notation for writing such programs, but unlike TeX—which is essentially an overgrown macro preprocessor—Scribble is a full-fledged functional programming language. In fact, the Scribble syntax is really just an alternate notation for Racket itself, so all the libraries and abstractions available in Racket can be used more or less directly in a Scribble document.

This makes Scribble a remarkably powerful tool for writing prose documents, and indeed, it serves as the foundation for Racket’s @hyperlink["https://docs.racket-lang.org/"]{best-in-class documentation system}, among other things. Using it to power a blog is perhaps a bit overkill, but it gives me the wonderful ability to define whatever abstractions I desire to make blogging as effortless as possible. For example, after finding myself linking to Hackage packages quite frequently, I decided to define a one-line function:

@pygments-block[#:language "racket"]{
(define (hackage-package package-name)
  (hyperlink (string-append "https://hackage.haskell.org/package/" package-name) package-name))}

Now all I have to do is write @code["@hackage-package{lens}"] and I get @hackage-package{lens}. Sure, it’s not exactly mind-blowing, but it’s certainly convenient… and of course, the most significant advantages involve abstractions too elaborate to describe here.

This site is, naturally, open source, so if you’d like to see how all the pieces fit together for yourself, feel free to clone @github-repo*["lexi-lambda.github.io"]{the GitHub repository}. And if you’re interested in a simple example of what Scribble looks like to use, you might as well take a peek at @hyperlink["https://github.com/lexi-lambda/lexi-lambda.github.io/blob/source/blog/posts/about-me.scrbl"]{the source code for this page in particular}.

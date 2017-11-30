# Bio

My name is Alexis King, and I write [a lot of software][github-repos]. I live in Los Angeles, but I’m potentially willing relocate, depending on the location.

I’m interested in functional programming, static types, and programming language research, and I try to spend as much time as I can writing Haskell and Racket. I mostly work on web applications and infrastructure, but I’m especially passionate about writing libraries, open-source software, and various kinds of tooling.

I write about some of the things I do [on this blog](/), and I sometimes tweet about them and other things [on Twitter][twitter]. I work on a *lot* of open-source projects [on GitHub][github], and you can email me at [lexi.lambda@gmail.com][email].

# Things I’m good at

## Programming languages and DSLs

I have spent a lot of time turning complicated problems into less complicated ones by building languages to solve them, focusing on everything from [environment variable management][envy] to [type-safe unit testing][monad-mock] to [shell scripts that manage their own dependencies][scripty]. I work on [a programming language called Hackett](#the-hackett-programming-language).

## Web applications and APIs

I’ve been working with web tech since long before I started programming professionally, and I know how to design user-friendly applications and strong, stable APIs. I know how to encode many domains into a type system to make the most common bugs **impossible**, and I can build embedded languages that make things like database queries and templating safe, efficient, and concise.

## Libraries and developer tooling

I dedicate most of my free time to developing and maintaining open source software, and I know how to write **and document** watertight abstractions that can be used and reused. I approach library interfaces from a user experience perspective as well as a technical one.

## Teaching and technical writing

I like explaining things, and I know how to make complex topics accessible, both in writing and in person. I give talks and workshops, write [this blog](/), and [document][hackett-doc] [all][megaparsack-doc] [of my][functional-doc] [software][lens-doc] [extensively][test-fixture-doc].

# Things I like to use

## Haskell

I am deeply familiar with Haskell and its ecosystem, and I know how to do everything from practical type-level programming to metaprogramming with Template Haskell. I am skilled in designing Haskell applications that are testable and easy to change and maintain. I use a specific set of libraries and language extensions to turn GHC into a compile-time assistant that knows so much about my domain that it can write a lot of my code for me.

Recently, I’ve done a lot of work using servant, a Haskell type-level DSL for building REST-y HTTP APIs, which makes implementing complex APIs easy in a composable way. I also maintain four open-source Haskell libraries, [text-conversions][], [monad-mock][], [monad-persist][], and [test-fixture][], and I’ve contributed to many others.

## Racket

I work *on* Racket almost as much as I work *with* Racket, and I know its state of the art macro system inside and out. I use Racket to build extremely flexible tools that are both easy to extend and easy to understand, and I use its documentation language, Scribble, to write general-purpose technical documentation and specifications.

I maintain [too many Racket libraries to list][racket-my-packages].

# Projects I’m working on

## The Hackett programming language

[Hackett][hackett] is a *very* new programming language that combines a Haskell-style type system and Racket’s cutting-edge macro system to form a statically typed Lisp that exposes type information at macroexpansion time. It is based on [research by Stephen Chang, Alex Knauth, and Ben Greenman on embedding types and typechecking in a macro system][types-as-macros]. Hackett is semantically much closer to Haskell than Scheme, but it uses an s-expression-based syntax to enable more powerful syntactic abstractions.

My efforts so far have yielded a working implementation of most of Haskell 98, though it is slow and not especially user-friendly. The implementation involves a fusion of the types-as-macros techniques with a more traditional unification-based typechecker. Hackett is already powerful enough to solve simple programming problems, especially when paired with Racket’s extensive library ecosystem, but there is still much work to be done around improving performance, ease of use, and additional type system features.

For more information [see the Hackett documentation][hackett-doc] and [my blog posts on Hackett][hackett-blog-posts].

## Testing strongly-typed monadic effects in Haskell

I am the primary author of the [monad-mock][] and [test-fixture][] Haskell libraries, which aim to assist Haskell developers in flexibly testing effectful code with zero boilerplate and without giving up any static guarantees. These are implemented with a combination of some type-level trickery, a monadic implementation of explicit typeclass dictionary passing, and some Template Haskell utilities for code generation and unnecessary manual maintenance.

The general techniques are described in more detail in [this blog post on monad-mock][monad-mock-blog-post] and [this blog post on test-fixture][haskell-testing-blog-post]. Open problems with these techniques include cleanly testing polymorphic methods and better supporting concurrent programs, but [I’m working on that][test-fixture-polymorphic-issue].

## racket-tulip and Racket language tooling

I maintain a [working prototype][racket-tulip] of the [tulip][tulip-lang] programming language. While having an implementation of tulip is useful in and of itself, the main purpose of the project is to explore Racket’s language tools in more depth. A handful of libraries have come out of my research, most notably [megaparsack][megaparsack], a Parsec-style parser combinator library that is uniquely capable of automatically producing “syntax objects”, which cooperate with Racket’s advanced set of static analysis tools.

Other research in a related space has involved looking into ways to compose syntactic language extensions in safe and predictable ways, making reader syntax as composable and safe as ordinary Racket macros. This work is demonstrated by the [curly-fn][curly-fn] package, powered by the underlying [namespaced-transformer][namespaced-transformer] library.

# Other things I know about

## AWS

I like to stay as far away from ops as I possibly can, so (perhaps ironically) I’ve gotten pretty good at understanding how to safely and reliably deploy infrastructure on AWS in a way that won’t ever require me to `ssh` into a production box. Most notably, I’m knowledgable in creating *software-defined architecture*, architecture that can be tracked in version control, reliably replicated as necessary, and composed together to achieve coordination between independent subsystems.

## JavaScript

I am an expert in both JavaScript-the-language and the JavaScript front-end ecosystem. It’s not my favorite technology, but knowing the language and its tools is pretty important for building modern web applications, even if you decide to use a compile-to-JS language. I know *all* of the language, good parts and bad, and I have experience working with React, Redux, Backbone, Angular, Babel, Webpack, Browserify, Gulp, Mocha, Ramda, and most of the rest of the JS frontend soup.


[curly-fn]: https://github.com/lexi-lambda/racket-curly-fn
[email]: mailto:lexi.lambda@gmail.com
[envy]: https://github.com/lexi-lambda/envy
[functional-doc]: http://docs.racket-lang.org/functional/interfaces.html
[github]: https://github.com/lexi-lambda
[github-repos]: https://github.com/lexi-lambda?tab=repositories
[hackett]: https://github.com/lexi-lambda/hackett
[hackett-announcement]: /blog/2017/01/02/rascal-a-haskell-with-more-parentheses/
[hackett-blog-posts]: /tags/hackett.html
[hackett-doc]: https://pkg-build.racket-lang.org/doc/hackett@hackett-doc/
[haskell-testing-blog-post]: /blog/2016/10/03/using-types-to-unit-test-in-haskell/
[lens-doc]: http://docs.racket-lang.org/lens/lens-guide.html
[megaparsack]: https://github.com/lexi-lambda/megaparsack
[megaparsack-doc]: http://docs.racket-lang.org/megaparsack/index.html
[monad-persist]: https://github.com/cjdev/monad-persist
[monad-mock]: https://github.com/cjdev/monad-mock
[monad-mock-blog-post]: /blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/
[namespaced-transformer]: https://github.com/lexi-lambda/namespaced-transformer
[racket-my-packages]: https://pkgd.racket-lang.org/pkgn/search?tags=author%3Alexi.lambda%40gmail.com
[racket-tulip]: https://github.com/lexi-lambda/racket-tulip
[scripty]: https://github.com/lexi-lambda/scripty
[test-fixture]: https://github.com/cjdev/test-fixture
[test-fixture-doc]: http://hackage.haskell.org/package/test-fixture/docs/Control-Monad-TestFixture.html
[test-fixture-polymorphic-issue]: https://github.com/cjdev/test-fixture/issues/19#issuecomment-253080750
[text-conversions]: https://github.com/cjdev/text-conversions
[tulip-lang]: http://tuliplang.org
[twitter]: https://twitter.com/lexi_lambda
[types-as-macros]: http://www.ccs.neu.edu/home/stchang/pubs/ckg-popl2017.pdf

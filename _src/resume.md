# Bio

My name is Alexis King, and I write a lot of software. I live in Los Angeles, but I’m potentially willing relocate, depending on location.

I’m interested in functional programming and programming language research, and I try to spend as much time as I can writing Haskell and Racket. I mostly work on web applications and infrastructure, but I’m especially passionate about writing libraries and tooling.

I write about some of the things I do [on this blog](/), and I sometimes tweet about them and other things [on Twitter][twitter]. I work on a *lot* of open-source projects [on GitHub][github], and you can email me at [lexi.lambda@gmail.com][email].

# Things I’m good at

## Programming languages and DSLs

I have spent a lot of time turning complicated problems into less complicated ones by building languages to solve them, focusing on everything from [environment variable management][envy] to [type-safe unit testing][test-fixture] to [shell scripts that manage their own dependencies][scripty].

## Web applications and APIs

I’ve been working with web tech for most of my programming lifetime, and I know how to design user-friendly applications and strong, stable APIs. I know how to encode most domains into a type system in such a way that makes the most common bugs **impossible**, and I can build embedded languages that make things like database queries and templating safe, efficient, and concise.

## Libraries and developer tooling

I dedicate most of my free time to developing and maintaining open source software, and I know how to write **and document** watertight abstractions that can be used and reused. I approach library interfaces from a user experience perspective as well as a technical one.

## Teaching and technical writing

I like explaining things, and I know how to make complex topics accessible, both in writing and in person. I give talks and workshops, write [this blog](/), and [document][megaparsack-doc] [all of my][functional-doc] [software][lens-doc] [extensively][test-fixture-doc].

# Things I like to use

## Haskell

I am deeply familiar with Haskell and its ecosystem, and I know how to do everything from practical type-level programming to metaprogramming with Template Haskell. I am skilled in structuring Haskell applications to make them testable and easy to change. I use a hand-picked complement of libraries and GHC extensions to turn Haskell into a compile-time assistant that knows so much about my domain that it can write a lot of my code for me.

Recently, I’ve done a lot of work using servant, a Haskell type-level DSL for building REST-y HTTP APIs, which makes implementing complex APIs easy in a composable way. I also maintain two open-source Haskell libraries, [text-conversions][text-conversions] and [test-fixture][test-fixture].

## Racket

I work *on* Racket almost as much as I work *with* Racket, and I know its state of the art macro system inside and out. I use Racket to build extremely flexible tools that are both easy to extend and easy to understand, and I use its documentation language, Scribble, to write general-purpose technical documentation and specifications.

I maintain [too many Racket libraries to list][racket-my-packages].

## JavaScript

I am an expert in both JavaScript as a language and the JavaScript front end ecosystem. It’s perhaps not fair to say I *like* to use JavaScript, but knowing the language and its tools is pretty important for building modern web applications. I know *all* of the language, good parts and bad, and I have experience working with Backbone, Angular, React, Babel, Webpack, Browserify, Gulp, Mocha, Ramda, and most of the rest of the JS frontend soup.

# Projects I’m working on

## racket-tulip and Racket language tooling

I maintain a [working prototype][racket-tulip] of the [tulip][tulip-lang] programming language. While having an implementation of tulip is useful in and of itself, the main purpose of the project is to explore Racket’s language tools in more depth. A handful of libraries have come out of my research, most notably [megaparsack][megaparsack], a Parsec-style parser combinator library that is uniquely capable of automatically producing “syntax objects”, which cooperate with Racket’s advanced set of static analysis tools.

Other research in a related space has involved looking into ways to compose syntactic language extensions in safe and predictable ways, making reader syntax as composable and safe as ordinary Racket macros. This work is demonstrated by the [curly-fn][curly-fn] package, powered by the underlying [namespaced-transformer][namespaced-transformer] library.

## Testing strongly-typed monadic effects in Haskell

I am a co-author of the [test-fixture][test-fixture] Haskell library, which aims to assist Haskell developers in flexibly testing effectful code without any boilerplate and without giving up any static guarantees. This is implemented with a combination of some type-level trickery, a monadic implementation of explicit typeclass dictionary passing, and a Template Haskell utility for eliminating boilerplate and unnecessary manual maintenance.

The general technique is described in more detail in [this blog post][haskell-testing-blog-post]. Open research problems with this technique include cleanly testing polymorphic methods and better supporting concurrent programs, but [I’m working on that][test-fixture-polymorphic-issue].

## The Hackett programming language

[Hackett][hackett] is a *very* new programming language that combines a Haskell-style type system and Racket’s cutting-edge macro technology into a statically typed Lisp that exposes type information at macro-expansion time. It is based on [research by Stephen Chang, Alex Knauth, and Ben Greenman about embedding types and typechecking as macros][types-as-macros]. It is semantically much closer to Haskell than Scheme, but it uses an s-expression-based syntax to enable more powerful syntactic abstractions.

My efforts so far have involved fusing the new types-as-macros technique with more traditional unification-based type inference in a composable way. The initial results are promising, and my working prototype is currently pretty close to a somewhat usable ML, but the initial goal is to support all of Haskell 98’s core features.

[curly-fn]: https://github.com/lexi-lambda/racket-curly-fn
[email]: mailto:lexi.lambda@gmail.com
[envy]: https://github.com/lexi-lambda/envy
[functional-doc]: http://docs.racket-lang.org/functional/interfaces.html
[github]: https://github.com/lexi-lambda
[hackett]: https://github.com/lexi-lambda/hackett
[haskell-testing-blog-post]: /blog/2016/10/03/using-types-to-unit-test-in-haskell/
[lens-doc]: http://docs.racket-lang.org/lens/lens-guide.html
[megaparsack]: https://github.com/lexi-lambda/megaparsack
[megaparsack-doc]: http://docs.racket-lang.org/megaparsack/index.html
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

    Title: Rascal: a Haskell with more parentheses
    Date: 2017-01-02T09:16:42
    Tags: rascal, racket, haskell, programming languages, functional programming

“Hey! You got your Haskell in my Racket!”

“No, you got *your* Racket in *my* Haskell!”

Welcome to the [Rascal][rascal] programming language.

<!-- more -->

# Why Rascal?

Why yet *another* programming language? Anyone who knows me knows that I already have two programming languages that I *really* like: Haskell and Racket. Really, I think they’re both great! Each brings some things to the table that aren’t really available in any other programming language I’ve ever used.

Haskell, in many ways, is a programming language that fits my mental model of how to structure programs better than any other programming language I’ve used. Some people would vehemently disagree, and it seems that there is almost certainly some heavy subjectivity in how people think about programming. I think Haskell’s model is awesome once you get used to it, though, but this blog post is not really going to try and convince you why you should care about Haskell (though that *is* something I want to write at some point). What you *should* understand, though, is that to me, Haskell is pretty close to what I want in a programming language.

At the same time, though, Haskell has problems, and a lot of that revolves around its story for metaprogramming. “Metaprogramming” is another M word that people seem to be very afraid of, and for good reason: most metaprogramming systems are ad-hoc, unsafe, unpredictable footguns that require delicate care to use properly, and *even then* the resulting code is brittle and difficult to understand. Haskell doesn’t suffer from this problem as much as some languages, but it isn’t perfect by any means: Haskell has at least two different metaprogramming systems (generics and Template Haskell) that are designed for different tasks, but they’re both limited in scope and both tend to be pretty complicated to use.

Discussing the merits and drawbacks of Haskell’s various metaprogramming capabilities is also outside the scope of this blog post, but there’s one *fact* that I want to bring up, which is that **Haskell does not provide any mechanism for adding syntactic abstractions to the language**. What do I mean by this? Well, in order to understand what a “syntactic abstraction” is and why you should care about it, I want to shift gears a little and take a look at why Racket is so amazing.

## A programmable programming language: theory and practice

I feel confident in saying that Racket has *the* most advanced macro system in the world, and it is pretty much unparalleled in that space. There are many languages with powerful type systems, but Racket is more or less alone in many of the niches it occupies. Racket has a large number of innovations that I don’t know of in any other programming language, and a significant portion of them focus on making Racket a [programmable programming language, a language for building languages][racket-manifesto].

This lofty goal is backed up by decades of research, providing Racket with an unparalleled toolkit for creating languages that can communicate, be extended, and even cooperate with tooling to provide introspection and error diagnostics. Working in Haskell feels like carefully designing a mould that cleanly and precisely fits your domain, carefully carving, cutting, and whittling. In contrast, working with Racket feels like moulding your domain until it looks the way *you* want it to look, poking and prodding at a pliable substrate. The sheer *ease* of it all is impossible for me to convey in words, so [you will have to see it for yourself](https://twitter.com/andmkent_/status/724036694773628930).

All this stuff is super abstract, though. What does it mean for practical programming, and why should you care? Well, I’m not going to try and sell you if you’re extremely skeptical, but if you’re interested, [I gave a talk on some of Racket’s linguistic capabilities last year called *Languages in an Afternoon*][languages-in-an-afternoon]. If you’re curious, give it a watch, and you might find yourself (hopefully) a little impressed. If you prefer reading, well, I have some [blog posts][tr-adts-with-macros] on this very blog that [demonstrate what Racket can do][envy-intro].

The basic idea, though, is that by having a simple syntax and a powerful macro system with a formalization of lexical scope, users can effectively invent entirely new language constructs as ordinary libraries, constructs that would have to be core forms in other programming languages. For example, Racket supports pattern-matching, but it isn’t built into the compiler: it’s simply implemented in the `racket/match` module distributed with Racket. Not only is it defined in ordinary Racket code, it’s actually *extensible*, so users can add their own pattern-matching forms that cooperate with `match`.

This is the power of a macro system to produce “syntactic abstractions”, things that can transform the way a user thinks of the code they’re writing. Racket has the unique capability of making these abstractions both easy to write and watertight, so instead of being a scary tool you have to handle with extreme care, you can easily whip up a powerful, user-friendly embedded domain specific language in a matter of *minutes*, and it’ll be safe, provide error reporting for misuse, and cooperate with existing tooling pretty much out of the box.

## Fusing Haskell and Racket

So, let’s assume that we *do* want Haskell’s strong type system and that we *also* want a powerful metaprogramming model that permits syntactic extensions. What would that look like? Well, one way we could do it is to put one in front of the other: macro expansion is, by nature, a compile-time pass, so we could stick a macroexpander in front of the typechecker. This leads to a simple technique: first, macroexpand the program to erase the macros, then typecheck it and erase the types, then send the resulting code off to be compiled. This technique has the following properties:

  1. First of all, **it’s easy to implement**. Racket’s macroexpander, while complex, is well-documented in academic literature and works extremely well in practice. In fact, this strategy has already been implemented! Typed Racket, the gradually-typed sister language of Racket, expands every program before typechecking. It would be possible to effectively create a “Lisp-flavored Haskell” by using this technique, and it might not even be that hard.

  2. Unfortunately, there’s a huge problem with this approach: **type information is not available at macroexpansion time**. This is the real dealbreaker with the “expand, then typecheck” model, since static type information is some of the most useful information possibly available to a macro writer. In an ideal world, macros should not only have access to type information, they should be able to manipulate it and metaprogram the typechecker as necessary, but if macroexpansion is a separate phase from typechecking, then that information simply doesn’t exist yet.

For me, the second option is unacceptable. I am *not* satisfied by a “Lisp-flavored Haskell”; I want my types and macros to be able to cooperate and communicate with each other. The trouble, though, is that solving that problem is really, really hard! For a couple years now, I’ve been wishing this ideal language existed, but I’ve had no idea how to make it actually work. Template Haskell implements a highly restricted system of interweaving typechecking and splice evaluation, but it effectively does it by running the typechecker and the splice expander alternately, splitting the source into chunks and typechecking them one at a time. This works okay for Template Haskell, but for the more powerful macro system I am looking for, it wouldn’t scale.

There’s something a little bit curious, though, about the problem as I just described it. The processes of “macroexpanding the program to erase the macros” and “typechecking the program to erase the types” sound awfully similar. It seems like maybe these are two sides of the same coin, and it would be wonderful if we could encode one in terms of the other, effectively turning the two passes into a single, unified pass. Unfortunately, while this sounds great, I had no idea how to do this (and it didn’t help that I really had no idea how existing type systems were actually implemented).

Fortunately, last year, Stephen Chang, Alex Knauth, and Ben Greenman put together a rather exciting paper called [*Type Systems as Macros*][types-as-macros], which does precisely what I just described, and it delivers it all in a remarkably simple and elegant presentation. The idea is to “distribute” the task of typechecking over the individual forms of the language, leveraging existing macro communication facilities avaiable in the Racket macroexpander to propagate type information as macros are expanded. To me, it was exactly what I was looking for, and I almost immediately started playing with it and seeing what I could do with it.

The result is [*Rascal*][rascal], a programming language built in the Racket ecosystem that attempts to implement a Haskell-like type system.

# A first peek at Rascal

Rascal is a very new programming language I’ve only been working on over the past few months. It is extremely experimental, riddled with bugs, half-baked, and may turn your computer into scrambled eggs. Still, while I might not recommend that you actually *use* it just yet, I want to try and share what it is I’m working on, since I’d bet at least a few other people will find it interesting, too.

First, let me say this up front: **Rascal is probably a lot closer to Haskell than Racket**. That might come as a surprise, given that Rascal has very Lisp-y syntax, it’s written in Racket, and it runs on the Racket platform, but semantically, Rascal is mostly just Haskell 98. This is important, because it may come as a surprise, given that there are so few statically typed Lisps, but there’s obviously no inherent reason that Lisps need to be dynamically typed. They just seem to have mostly evolved that way.

Taking a look at a snippet of Rascal code, it’s easy to see that the language doesn’t work quite like a traditional Lisp, though:[^1]

```
(def+ map-every-other : (forall [a] {{a -> a} -> (List a) -> (List a)})
  [_ nil            -> nil]
  [_ {x :: nil}     -> {x :: nil}]
  [f {x :: y :: ys} -> {x :: (f y) :: (map-every-other f ys)}])
```

This is a Lisp with all the goodies you would expect out of Haskell: static types, parametric polymorphism, automatically curried functions, algebraic datatypes, pattern-matching, infix operators, and of course, *typeclasses*. Yes, with Rascal you can have your monads in all their statically dispatched glory:

```
(data (Maybe a)
  (just a)
  nothing)

(instance (Monad Maybe)
  [join (case-lambda
          [(just (just x)) (just x)]
          [_               nothing])])
```

So far, though, this really *is* just “Haskell with parentheses”. As alluded to above, however, Rascal is a bit more than that.

## Core forms can be implemented as derived concepts

Rascal’s type system is currently very simple, being nothing more than Hindley-Milner plus ad-hoc polymorphism in the form of typeclasses. Something interesting to note about it is that it does not implement ADTs or pattern-matching anywhere in the core! In fact, ADTs are defined as two macros `data` and `case`, in an entirely separate module, which can be imported just like any other library.

The main `rascal` language provides ADTs by default, of course, but it would be perfectly possible to produce a `rascal/kernel` language which does not include them at all. In this particular case, it seems unlikely that Rascal programmers would want their own implementation of ADTs, but it’s an interesting proof of concept, and it hints at other “core” features that could be implemented using macros.

Simple syntactic transformations are, of course, trivially defined as macros. Haskell `do` notation is defined as [an eleven-line macro in `rascal/monad`][rascal-monad], and GHC’s useful `LambdaCase` extension is also possible to implement without modifying Rascal at all. This is useful, because there are many syntactic shorthands that are extremely useful to implement, but don’t make any sense to be in GHC because they are specific to certain libraries or applications. Racket’s macro system makes those not only possible, but actually pretty easy.

While the extent of what is possible to implement as derived forms remains to be seen, many useful GHC features seem quite possible to implement without touching the core language, including things like `GeneralizedNewtypeDeriving` and other generic deriving mechanisms like `GHC.Generics`, `DeriveGeneric`, and `DeriveAnyClass`.

## The language is not enough

No language is perfect. Most people would agree with this, but I would take it a step further: no language is even sufficient! This makes a lot of sense, given that general-purpose programming languages are designed to do *everything*, and it’s impossible to do everything well.

Haskell programmers know this, and they happily endorse the creation of embedded domain specific languages. These are fantastic, and we need more of them. Things like [servant][servant] let me write a third of the code I might otherwise need to, and the most readable code is the code you didn’t have to write in the first place. DSLs are good.

Unfortunately, building DSLs is traditionally difficult, largely in part because building embedded DSLs means figuring out a way to encode your domain into your host language of choice. Sometimes, your domain simply does not elegantly map to your host language’s syntax or semantics, and you have to come up with a compromise. This is easy to see with servant, which, while it does a remarkably good job, still has to resort to some very clever type magic to create some semblance of an API description in Haskell types:

```
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
          :<|> "users" :> Capture "userid" Integer
                       :> Get '[JSON] User
          :<|> "users" :> Capture "userid" Integer
                       :> ReqBody '[JSON] User
                       :> Put '[JSON] User
```

The above code is *remarkably* readable for what it is, but what if we didn’t have to worry about working within the constraints of Haskell’s syntax? What if we could design a syntax that was truly the best for the job? Perhaps we would come up with something like this:

```
(define-api User-API
  #:content-types [JSON]
  [GET  "users"                    => (List User)]
  [POST "users"                    => User -> User]
  [GET  "users" [userid : Integer] => User]
  [PUT  "users" [userid : Integer] => User -> User])
```

This would be extremely easy to write with Racket’s macro-writing utilities, and it could even be made extensible. This could also avoid having to do the complicated typeclass trickery servant has to perform to then generate code from the above specification, since it would be much easier to just generate the necessary code directly (which still maintaining type safety).

In addition to the type-level hacks that Haskell programmers often have to pull in order to make these kinds of fancy DSLs work, free monads tend to be used to create domain-specific languages. This works okay for some DSLs, but remember that when you use a free monad, you are effectively writing a *runtime interpreter* for your language! Macros, on the other hand, are compiled, and you get ability to *compile* your DSL to code that can be optimized by all the existing facilities of the compiler toolchain.

# Rascal is embryonic

I’m pretty excited about Rascal. I think that it could have the potential to do some pretty interesting things, and I have some ideas in my head for how having macros in a Haskell-like language could change things. I also think that, based on what I’ve seen so far, having both macros and a Haskell-like type system could give rise to *completely* different programming paradigms than exist in either Haskell or Racket today. My gut tells me that this is a case where the whole might actually be greater than the sum of its parts.

That said, Rascal doesn’t really exist yet. Yes, [there is a GitHub repository][rascal], and it has some code in it that does… something. Unfortunately, the code is also currently extremely buggy, to the point of being borderline broken, and it’s also in such early stages that you can’t really do *anything* interesting with it, aside from some tiny toy programs.

As I have worked on Rascal, I’ve come to a somewhat unfortunate conclusion, which is that I really have almost zero interest in implementing type systems. I felt that way before I started the project, but I was hoping that maybe once I got into them, I would find them more interesting. Unfortunately, as much as I love working with powerful type systems (and really, I adore working with Haskell and using all the fancy features GHC provides), I find implementing the software that makes them tick completely dull.

Still, I’m willing to invest the time to get something that I can use. Even so, resources for practical type system implementation are scarce. I want to thank [Mark P Jones][mpj] for his wonderful resource [Typing Haskell in Haskell][thih], without which getting to where I am now would likely have been impossible. I also want to thank [Stephen Diehl][stephen-diehl] for his wonderful [Write You a Haskell][wyah] series, which was also wonderfully useful to study, even if it is unfinished and doesn’t cover anything beyond ML just yet.

Even with these wonderful resources, I’ve come to the realization that **I probably can’t do all of this on my own**. I consider myself pretty familiar with macros and macro expanders at this point, but I don’t know much about type systems (at least not their implementation), and I could absolutely use some help. So if you’re interested in Rascal and think you might be able to pitch in, please: I would appreciate even the littlest bits of help or guidance!

In the meantime, I will try to keep picking away at Rascal in the small amount of free time I currently have. Thanks, as always, to all the amazing people who have contributed to the tools I’ve been using for this project: special thanks to the authors of *Type Systems as Macros* for their help as well as the people I mentioned just above, and also to all of the people who have built Racket and Haskell and made them what they are today. Without them, Rascal would most definitely not exist.


[^1]: Note that most of the Rascal code in this blog post probably doesn’t actually work on the current Rascal implementation. Pretty much all of it can be implemented in the current implementation, the syntax just isn’t quite as nice yet.

[envy-intro]: /blog/2015/08/30/managing-application-configuration-with-envy/
[languages-in-an-afternoon]: https://www.youtube.com/watch?v=TfehOLha-18
[mpj]: https://web.cecs.pdx.edu/~mpj/
[racket-manifesto]: http://www.ccs.neu.edu/home/matthias/manifesto/
[rascal]: https://github.com/lexi-lambda/rascal
[rascal-monad]: https://github.com/lexi-lambda/rascal/blob/87d001a82c86fb66544d25c37ffba9be1ac63464/rascal-lib/rascal/monad.rkt#L48-L58
[servant]: http://hackage.haskell.org/package/servant
[stephen-diehl]: http://www.stephendiehl.com
[thih]: https://web.cecs.pdx.edu/~mpj/thih/
[tr-adts-with-macros]: /blog/2015/12/21/adts-in-typed-racket-with-macros/
[types-as-macros]: http://www.ccs.neu.edu/home/stchang/popl2017/
[wyah]: http://dev.stephendiehl.com/fun/

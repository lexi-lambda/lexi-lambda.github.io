    Title: Rascal is now Hackett, plus some answers to questions
    Date: 2017-01-05T05:25:32
    Tags: hackett, racket, haskell, programming languages

Since I published [my blog post introducing Rascal][rascal-intro], I’ve gotten some *amazing* feedback, more than I had ever anticipated! One of the things that was pointed out, though, is that [Rascal is a language that already exists][rascal-mpl]. Given that the name “Rascal” came from a mixture of “Racket” and “Haskell”, I always had an alternative named planned, and that’s “Hackett”. So, to avoid confusion as much as possible, [**Rascal is now known as Hackett**][hackett].

With that out of the way, I also want to answer some of the other questions I received, both to hopefully clear up some confusion and to have something I can point to if I get the same questions in the future.

<!-- more -->

# What’s in a name?

First, a little trivia.

I’ve already mentioned that the old “Rascal” name was based on the names “Racket” and “Haskell”, which is true. However, it had a slightly deeper meaning, too: the name fit a tradition of naming languages in the Scheme family after somewhat nefarious things, such as “Gambit”, “Guile”, “Larceny”, and “Racket” itself. The name goes back a little bit further to the Planner programming language; Scheme was originally called Schemer, but it was (no joke) shorted due to filename length restrictions.

Still, my language isn’t really a Scheme, so the weak connection wasn’t terribly relevant. Curious readers might be wondering if there’s any deeper meaning to the name “Hackett” than a mixture of the two language names. In fact, there is. Hackett is affectionately named after the [Genesis progressive rock guitarist, Steve Hackett][steve-hackett], one of my favorite musicians. The fact that the name is a homophone with “hack-it” is another convenient coincidence.

Perhaps not the most interesting thing in this blog post, but there it is.

# Why Racket? Why *not* Haskell?

One of the most common questions I received is why I used Racket as the implementation language instead of Haskell. This is a decent question, and I think it likely stems at least in part from an item of common confusion: **Racket is actually two things, a programming language and a programming language platform**. The fact that the two things have the same name is probably not ideal, but it’s what we’ve got.

Racket-the-language is obviously the primary language used on the Racket platform, but there’s actually surprisingly little need for that to be the case; it’s simply the language that is worked on the most. Much of the Racket tooling, including the compiler, macroexpander, and IDE, are actually totally language agnostic. If someone came along and wrote a language that got more popular than `#lang racket`, then there wouldn’t really be anything hardcoded into any existing tooling that would give the impression that `#lang racket` was ever the more “dominant” language, aside from the name.

For this reason, Racket is ideal for implementing new programming languages, moreso than pretty much any other platform out there. The talk I linked to in the previous blog post, [Languages in an Afternoon][languages-in-an-afternoon], describes this unique capability. It’s short, only ~15 minutes, but if you’re not into videos, I can try and explain why Racket is so brilliant for this sort of thing.

By leveraging the Racket platform instead of implementing my language from scratch, I get the following things pretty much for free:

  1. I get a JIT compiler for my code, and I don’t have to implement a compiler myself.

  2. I also get a package manager that can cooperate with Hackett code to deliver Hackett modules.

  3. I get a documentation system that is fully indexed and automatically locally installed when you install Hackett or any package written in Hackett, and that documentation is automatically integrated with the editor.

  4. The DrRacket IDE can be used out of the box with Hackett code, it automatically does syntax highlighting and indenting, and it even provides interactive tools for inspecting bindings (something that I demo in my aforementioned talk).

  5. If you don’t want to use DrRacket, you can use the [racket-mode][racket-mode] major mode for Emacs, which uses the same sets of tools that DrRacket uses under the hood, so you get most of the same DrRacket goodies without sacrificing Emacs’s power of customization.

Reimplementing all of that in another language would take years of work, and I haven’t even mentioned Racket’s module system and macroexpander, which are the underpinnings of Hackett. GHC’s typechecker is likely roughly as complex as Racket’s macroexpander combined with its module system, but I am not currently implementing GHC’s typechecker, since I do not need all of OutsideIn(X)’s features, just Haskell 98 + some extensions.

In contrast, I truly do need all of the Racket macroexpander to implement Hackett, since the *Type Systems as Macros* paper uses pretty much every trick the Racket macro system has to offer to implement typechecking as macroexpansion. For those reasons, implementing the Racket macroexpander **alone** in Haskell would likely be monumentally more work than implementing a Hindley-Milner typechecker in Racket, so it doesn’t really make sense to use Haskell for that job.

## Actually running Hackett code

Now, it’s worth noting that GHC is much more efficient as a compiler than Racket is, for a whole host of reasons. However, since typechecking and macroexpansion are inherently strictly compile-time phases, it turns out to be totally feasible to run the typechecker/macroexpander in Racket (since in Hackett, the two things are one and the same), then compile the resulting fully-expanded, well-typed code to GHC Core. That could then be handed off to GHC itself and compiled using the full power of the GHC optimizer and compiler toolchain.

This would be no small amount of work, but it seems theoretically possible, so eventually it’s something I’d love to look into. There are various complexities to making it work, but I think it would let me get the best of both worlds without reinventing the wheel, so it’s something I want long-term.

There’s also the question of how “native” Hackett code would be, were it compiled to GHC Core. Would Hackett code be able to use Haskell libraries, and vice versa? My guess is that the answer is “yes, with some glue”. It probably wouldn’t be possible to do it completely seamlessly, because Hackett provides type information at macroexpansion time that likely wouldn’t exist in the same form in GHC. It might be possible to do some incredibly clever bridging to be able to use Haskell libraries in Hackett almost directly, but the inverse might not be true if a library’s interface depends on macros.

# How do Template Haskell quasiquoters compete with macros?

Quasiquoters have a number of drawbacks, but the two main ones are complexity and lack of composition.

S-expressions happen to be simple, and this means s-expression macros have two lovely properties: they’re easy to write, given good libraries (Racket has [`syntax/parse`][syntax-parse]), and they’re easy for tools to understand. Quasiquoters force implementors to write their own parsers from raw strings of characters, which is quite a heavy burden, and it usually means those syntaxes are confusing and brittle. To give a good example, consider [persistent’s quasiquoters][persistent-th]: they look *sort of* like Haskell data declarations, but they’re not really, and I honestly have no idea what their actual syntax really is. It feels pretty finicky, though. In contrast, an s-expression based version of the same syntax would basically look just like the usual datatype declaration form, plus perhaps some extra goodies.

Additionally, s-expression macros *compose*, and this should probably be valued more than anything else. If you’re writing code that doesn’t compose, it’s usually a bad sign. So much of functional programming is about writing small, reusable pieces of code that can be composed together, and macros are no different. Racket’s `match`, for example, is an expression, and it contains expressions, so `match` can be nested within itself, as well as other arbitrary macros that produce expressions. Similarly, many Racket macros can be extended, which is possible due to having such uniform syntax.

Making macros “stand out” is an issue of some subjectivity, but in my experience such a fear of macros tends to stem from a familiarity with bad macro systems (which, to be fair, is almost all of them) and poor tooling. I’ve found that, in practice, most of the reasons people want to know “is this a macro??” is because macros are scary black boxes and people want to know which things to be suspicious of.

Really, though, one of the reasons macros are complicated isn’t knowing which things are macros, but it’s knowing *which identifiers are uses and which identifiers are bindings*, and things like that. Just knowing that something is a macro use doesn’t actually help at all there—the syntax won’t tell you. [Solve that problem with tools that address the problem head on, not by making a syntax that makes macros second-class citizens.](http://i.imgur.com/HvYee19.png) One of the reasons I used the phrase “syntactic abstractions” in my previous blog post is because you specifically want them to be **abstractions**. If you have to think of a macro in terms of the thing it expands to then it isn’t a very watertight abstraction. You don’t think about Haskell pattern-matching in terms of what the patterns compile to, you just use them. Macros should be (and can be) just as fluid.

# How can I help?

Right now, what I really need is someone who understands type system implementation. You don’t need to be up to date on what’s cutting edge—I’m not implementing anything nearly as complicated as GADTs or dependent types yet—you just need to understand how to implement Haskell 98. If you have that knowledge and you’re interested in helping, even if it just means answering some of my questions, please contact me via email, IRC (the #racket channel on Freenode is a good place for now), or Slack (I’m active in the snek Slack community, [which you can sign up for here][snek-signup]).

If you aren’t familiar with those things, but you’re still interested in helping out, there’s definitely plenty of work that needs doing. If you want to find somewhere you can pitch in, contacting me via any of the above means is totally fine, and I can point you in the right direction. Even if you just want to be a guinea pig, that’s useful.

[hackett]: https://github.com/lexi-lambda/hackett
[languages-in-an-afternoon]: https://www.youtube.com/watch?v=TfehOLha-18
[persistent-th]: http://www.yesodweb.com/book/persistent#persistent_code_generation
[racket-mode]: https://github.com/greghendershott/racket-mode
[rascal-intro]: /blog/2017/01/02/rascal-a-haskell-with-more-parentheses/
[rascal-mpl]: http://www.rascal-mpl.org
[snek-signup]: http://snek.jneen.net
[steve-hackett]: https://en.wikipedia.org/wiki/Steve_Hackett
[syntax-parse]: http://docs.racket-lang.org/syntax/stxparse.html

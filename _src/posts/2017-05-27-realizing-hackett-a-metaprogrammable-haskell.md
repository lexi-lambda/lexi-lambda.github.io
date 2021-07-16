    Title: Realizing Hackett, a metaprogrammable Haskell
    Date: 2017-05-27T15:30:00
    Tags: hackett, racket, haskell, programming languages

[Almost five months ago, I wrote a blog post about my new programming language, Hackett][hackett-intro], a fanciful sketch of a programming language from a far-off land with Haskell’s type system and Racket’s macros. At that point in time, I had a little prototype that barely worked, that I barely understood, and was a little bit of a technical dead-end. People saw the post, they got excited, but development sort of stopped.

Then, almost two months ago, I took a second stab at the problem in earnest. I read a lot, I asked a lot of people for help, and eventually I got something sort of working. Suddenly, [Hackett is not only real, it’s working, and you can try it out yourself][hackett]!

<!-- more -->

# A first look at Hackett

Hackett is still very new, very experimental, and an enormous work in progress. However, that doesn’t mean it’s useless! Hackett is already a remarkably capable programming language. Let’s take a quick tour.

As Racket law decrees it, every Hackett program must begin with `#lang`. We can start with the appropriate incantation:

```racket
#lang hackett
```

If you’re using DrRacket or racket-mode with background expansion enabled, then congratulations: the typechecker is online. We can begin by writing a well-typed, albeit boring program:

```racket
#lang hackett

(main (println "Hello, world!"))
```

In Hackett, a use of `main` at the top level indicates that running the module as a program should execute some `IO` action. In this case, `println` is a function of type `{String -> (IO Unit)}`. Just like Haskell, Hackett is pure, and the runtime will figure out how to actually run an `IO` value. If you run the above program, you will notice that it really does print out `Hello, world!`, exactly as we would like.

Of course, hello world programs are boring—so imperative! We are functional programmers, and we have our *own* class of equally boring programs we must write when learning a new language. How about some Fibonacci numbers?

```racket
#lang hackett

(def fibs : (List Integer)
  {0 :: 1 :: (zip-with + fibs (tail! fibs))})

(main (println (show (take 10 fibs))))
```

Again, Hackett is just like Haskell in that it is *lazy*, so we can construct an infinite list of Fibonacci numbers, and the runtime will happily do nothing at all. When we call `take`, we realize the first ten numbers in the list, and when you run the program, you should see them printed out, clear as day!

But these programs are boring. Printing strings and laziness may have been novel when you first learned about them, but if you’re reading this blog post, my bet is that you probably *aren’t* new to programming. How about something more interesting, **like a web server**?

```racket
#lang hackett

(require hackett/demo/web-server)

(data Greeting (greeting String))

(instance (->Body Greeting)
  [->body (λ [(greeting name)] {"Hello, " ++ name ++ "!"})])

(defserver run-server
  [GET "/"               -> String   => "Hello, world!"]
  [GET "greet" -> String -> Greeting => greeting])

(main (do (println "Running server on port 8080.")
          (run-server 8080)))
```
<br>
```sh
$ racket my-server.rkt
Running server on port 8080.
^Z
$ bg
$ curl 'http://localhost:8080/greet/Alexis'
Hello, Alexis!
```

**Welcome to Hackett.**

# What is Hackett?

Excited yet? I hope so. I certainly am.

Before you get a little *too* excited, however, let me make a small disclaimer: the above program, while quite real, is a demo. It is certainly not a production web framework, and it actually just uses the Racket web server under the hood. It does not handle very many things right now. You cannot use it to build your super awesome webapp, and even if you could, I would not recommend attempting to do so.

All that said, it is a *real* tech demo, and it shows off the potential for Hackett to do some pretty cool things. While the server implementation is just reusing Racket’s dynamically typed web server, the Hackett interface to it is 100% statically typed, and the above example shows off a host of features:

  - **Algebraic datatypes.** Hackett has support for basic ADTs, including recursive datatypes (though not yet mutually recursive datatypes).

  - **Typeclasses.** The demo web server uses a `->Body` typeclass to render server responses, and this module implements a `->Body` instance for the custom `Greeting` datatype.

  - **Macros.** The `defserver` macro provides a concise, readable, *type safe* way to define a simple, RESTful web server. It defines two endpoints, a homepage and a greeting, and the latter parses a segment from the URL.

  - **Static typechecking.** Obviously. If you try and change the homepage endpoint to produce a number instead of a string, you will get a type error! Alternatively, try removing the `->Body` instance and see what happens.

  - **Infix operators.** In Hackett, `{` curly braces `}` enter *infix mode*, which permits arbitrary infix operators. Most Lisps have variadic functions, so infix operators are not strictly necessary, but Hackett only supports curried, single-argument functions, so infix operators are some especially sweet sugar.

  - **Pure, monadic I/O.** The `println` and `run-server` functions both produce `(IO Unit)`, and `IO` is a monad. `do` notation is provided as a macro, and it works with any type that implements the `Monad` typeclass.

All these features are already implemented, and they really work! Of course, you might look at this list and be a little confused: sure, there are macros, but all these other things are firmly Haskellisms. If you thought that, you’d be quite right! **Hackett is much closer to Haskell than Racket, even though it is syntactically a Lisp.** Keep this guiding principal in mind as you read this blog post or explore Hackett. Where Haskell and Racket conflict, Hackett usually prefers Haskell.

For a bit more information about what Hackett is and what it aims to be, [check out my blog post from a few months ago][hackett-intro] from back when Hackett was called Rascal. I won’t reiterate everything I said there, but I do want to give a bit of a status update, explain what I’ve been working on, and hopefully give you some idea about where Hackett is going.

# The story so far, and getting to Hackett 0.1

In September of 2016, I attended [(sixth RacketCon)][sixth-racketcon], where I saw a [pretty incredible and extremely exciting talk][types-as-macros-talk] about implementing type systems as macros. Finally, I could realize my dream of having an elegant Lisp with a safe, reliable macro system and a powerful, expressive type system! Unfortunately, reality ensued, and I remembered I didn’t actually know any type theory.

Therefore, in October, I started to learn about type systems, and I began to read through Pierce’s Types and Programming Languages, then tried to learn the things I would need to understand Haskell’s type system. I learned about Hindley-Milner and basic typeclasses, and I tried to apply these things to the Type Systems as Macros approach. Throughout October, I hacked and I hacked, and by the end of the month, I stood back and admired my handiwork!

…it *sort of* worked?

The trouble was that I found myself stuck. I wasn’t sure how to proceed. My language had bugs, programs sometimes did things I didn’t understand, the typechecker was clearly unsound, and there didn’t seem to be an obvious path forward. Other things in my life became distracting or difficult, and I didn’t have the energy to work on it anymore, so I stopped. I put Hackett (then Rascal) on the shelf for a couple months, only to finally return to it in late December.

At the beginning of January, I decided it would be helpful to be public about what I was working on, so I wrote a blog post! Feedback was positive, overwhelmingly so, and while it was certainly encouraging, I suddenly felt nervous about expectations I had not realized I was setting. Could I really build this? Did I have the knowledge or the time? At that point, I didn’t really, so work stalled.

Fortunately, in early April, some things started to become clear. I took another look at Hackett, and I knew I needed to reimplement it from the ground up. I also knew that I needed a different technique, but this time, I knew a bit more about where to find it. I got some help from [Sam Tobin-Hochstadt][samth] and put together [an implementation of Pierce and Turner’s Local Type Inference][local-type-inference-impl]. Unfortunately, it didn’t really provide the amount of type inference I was looking for, but fortunately, implementing it helped me figure out how to understand the rather more complicated (though very impressive) [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism][complete-and-easy]. After that, things just sort of started falling into place:

  - First, I [implemented the Complete and Easy paper in Haskell][higher-rank], including building a little parser and interpreter. That helped me actually understand the paper, and Haskell really is a rather wonderful language for doing such a thing.

  - Three days later, I [ported the Haskell implementation to Racket][higher-rank-racket], using (and somewhat abusing) the Type Systems as Macros techniques. It wasn’t the prettiest, but it seemed to work, and that was rather encouraging.

  - After that, however, I got a little stuck again, as I wasn’t sure how to generalize what I had. I was also incredibly busy with my day job, and I wasn’t able to really make progress for a few weeks. In early May, however, I decided to [take a vacation][vacation-tweet] for a week, and with some time to focus, I [souped up the Haskell implementation with products and sums][higher-rank-algebraic]. This was progress!

  - The *following day* I managed to make [similar changes to the Racket implementation][higher-rank-racket-type-constructors], but rather than add anonymous products and sums, I added arbitrary type constructors.

  - A couple days later and with more than a bit of help from [Phil Freeman][phil-freeman], I [rebranded the Racket implementation as Hackett, Mk II][hackett-rebrand], and I started working towards turning it into a real programming language.

*Less than three weeks later*, and I have a programming language with everything from laziness and typeclasses to a tiny, proof-of-concept web server with [editor support][hackett-binding-arrows]. The future of Hackett looks bright, and though there’s a *lot* of work left before I will be even remotely satisfied with it, I am excited and reassured that it already seems to be bearing some fruit.

So what’s left? Is Hackett ready for an initial release? Can you start writing programs in it today? Well, unfortunately, the answer is mostly **no**, at least if you want those programs to be at all reliable in a day or two. If everything looks so cheery, though, what’s left? What is Hackett still missing?

## What Hackett still *isn’t*

I have a laundry list of features I want for Hackett. I want GADTs, indexed type families, newtype deriving, and a compiler that can target multiple backends. These things, however, are not essential. You can probably imagine writing useful software without any of them. Before I can try to tackle those, I first need to tackle some of the bits of the foundation that simply don’t exist yet (or have at least been badly neglected).

Fortunately, these things are not insurmountable, nor are they necessarily especially hard. They’re things like default class methods, static detection and prevention of orphan instances, exhaustiveness checking for pattern-matching, and a real kind system. That’s right—right now, Hackett’s type system is effectively dynamically typed, and even though you can write a higher-kinded type, there is no such thing as a “kind error”.

Other things are simply necessary quality of life improvements before Hackett can become truly usable. Type errors are currently rather atrocious, though they could certainly be worse. Additionally, typechecking currently just halts whenever it encounters a type error, and it makes no attempt to generate more than one type error at a time. Derivation of simple instances like `Show` and `Eq` is important, and it will also likely pave the way for a more general form of typeclass deriving (since it can most certainly be implemented via macros), so it’s uncharted territory that still needs to be explored.

Bits of plumbing are still exposed in places, whether it’s unexpected behavior when interoperating with Racket or errors sometimes reported in terms of internal forms. Local bindings are, if you can believe it, still entirely unimplemented, so `let` and `letrec` need to be written up. The standard library needs fleshing out, and certain bits of code need to be cleaned up and slotted into the right place.

Oh, and of course, **the whole thing needs to be documented**. That in and of itself is probably a pretty significant project, especially since there’s a good chance I’ll want to figure out how to best make use of Scribble for a language that’s a little bit different from Racket.

All in all, there’s a lot of work to be done! I am eager to make it happen, but I also work a full-time job, and I don’t have it in me to continue at the pace I’ve been working at for the past couple of weeks. Still, if you’re interested in the project, stay tuned and keep an eye on it—if all goes as planned, I hope to make it truly useful before too long.

# Answering some questions

It’s possible that this blog post does not seem like much; after all, it’s not terribly long. However, if you’re anything like me, there’s a good chance you are interested enough to have some questions! Obviously, I cannot anticipate all your questions and answer them here in advance, but I will try my best.

## Can I try Hackett?

Yes! With the caveat that it’s alpha software in every sense of the word: undocumented, not especially user friendly, and completely unstable. However, if you *do* want to give it a try, it isn’t difficult: just install Racket, then run `raco pkg install hackett`. Open DrRacket and write `#lang hackett` at the top of the module, then start playing around.

Also, note that the demo web server used in the example at the top of this blog post is *not* included when you install the `hackett` package. If you want to try that out, you’ll have to run `raco pkg install hackett-demo` to install the demo package as well.

## Are there any examples of Hackett code?

Unfortunately, not a lot right now, aside from the tiny examples in this blog post. However, if you are already familiar with Haskell, the syntax likely won’t be hard to pick up. Reading the Hackett source code is not especially recommended, given that it is filled with implementation details. However, if you are interested, reading the module where most of the prelude is defined isn’t so bad. You can [find it on GitHub here][hackett-prim-base], or you can open the `hackett/private/prim/base` module on a local installation.

## How can I learn more / ask questions about Hackett?

Feel free to ping me and ask me questions! I may not always be able to get back to you immediately, but if you hang around, I will eventually send you a response. The best ways to contact me are via the #racket IRC channel on Freenode, the snek Slack community ([which you can sign up for here][snek]), sending me [a DM on Twitter][my-twitter], opening [an issue on the GitHub repo][hackett-issues], or even just [sending me an email][my-email] (though I’m usually a bit slower to respond to the latter).

## How can I help?

Probably the easiest way to help out is to try Hackett for yourself and [report any bugs or infelicities you run into][hackett-issues]. Of course, many issues right now are known, there’s just so much to do that I haven’t had the chance to clean everything up. For that reason, the most effective way to contribute is probably to pick an existing issue and try and implement it yourself, but I wouldn’t be surprised if most people found the existing implementation a little intimidating.

If you *are* interested in helping out, I’d be happy to give you some pointers and answer some questions, since it would be extremely nice to have some help. Please feel free to contact me using any of the methods mentioned in the previous section, and I’ll try and help you find something you could work on.

## How does Hackett compare to *X* / why doesn’t Hackett support *Y*?

These tend to be complex questions, and I don’t always have comprehensive answers for them, especially since the language is evolving so quickly. Still, if you want to ask me about this, feel free to just send the question to me directly. In my experience, it’s usually better to have a conversation about this sort of thing rather than just answering in one big comparison, since there’s usually a fair amount of nuance.

## When will Hackett be ready for me to use?

I don’t know.

Obviously, there is a lot left to implement, that is certainly true, but there’s more to it than that. If all goes well, I don’t see any reason why Hackett can’t be early beta quality by the end of this year, even if it doesn’t support all of the goodies necessary to achieve perfection (which, of course, it never really can).

However, there are other things to consider, too. The Racket package system is currently flawed in ways that make rapidly iterating on Hackett hard, since it is extremely difficult (if not impossible) to make backwards-incompatible changes without potentially breaking someone’s program (even if they don’t update anything about their dependencies)! This is a solvable problem, but it would take some work modifying various elements of the package system and build tools, so that might need to get done before I can recommend Hackett in good faith.

# Appendix

It would be unfair not to mention all the people that have made Hackett possible. I cannot list them all here, but I want to give special thanks to [Stephen Chang][stchang], [Joshua Dunfield][joshdunf], [Robby Findler][robby], [Matthew Flatt][mflatt], [Phil Freeman][phil-freeman], [Ben Greenman][ben-greenman], [Alex Knauth][alex-knauth], [Neelakantan Krishnaswami][neelk], and [Sam Tobin-Hochstadt][samth]. I’d also like to thank everyone involved in the Racket and Haskell projects as a whole, as well as everyone who has expressed interest and encouragement about what I’ve been working on.

As a final point, just for fun, I thought I’d keep track of all the albums I’ve been listening to while working on Hackett, just in the past few weeks. It is [on theme with the name][hackett-whats-in-a-name], after all. This list is not completely exhaustive, as I’m sure some slipped through the cracks, but you can thank the following artists for helping me power through a few of the hills in Hackett’s implementation:

  - The Beach Boys — Pet Sounds
  - Boards of Canada — Music Has The Right To Children, Geogaddi
  - Bruce Springsteen — Born to Run
  - King Crimson — In the Court of the Crimson King, Larks’ Tongues in Aspic, Starless and Bible Black, Red, Discipline
  - Genesis — Nursery Cryme, Foxtrot, Selling England by the Pound, The Lamb Lies Down on Broadway, A Trick of the Tail
  - Mahavishnu Orchestra — Birds of Fire
  - Metric — Fantasies, Synthetica, Pagans in Vegas
  - Muse — Origin of Symmetry, Absolution, The Resistance
  - Peter Gabriel — Peter Gabriel I, II, III, IV / Security, Us, Up
  - Pink Floyd — Wish You Were Here
  - Supertramp — Breakfast In America
  - The Protomen — The Protomen, Act II: The Father of Death
  - Talking Heads — Talking Heads: 77, More Songs About Buildings and Food, Fear of Music, Remain in Light
  - Yes — Fragile, Relayer, Going For The One

And of course, *Voyage of the Acolyte*, by **Steve Hackett**.

[alex-knauth]: https://github.com/AlexKnauth
[ben-greenman]: http://www.ccs.neu.edu/home/types/
[complete-and-easy]: http://www.cs.cmu.edu/~joshuad/papers/bidir/
[hackett]: https://github.com/lexi-lambda/hackett
[hackett-binding-arrows]: https://twitter.com/lexi_lambda/status/867617563206758400
[hackett-intro]: /blog/2017/01/02/rascal-a-haskell-with-more-parentheses/
[hackett-issues]: https://github.com/lexi-lambda/hackett/issues
[hackett-prim-base]: https://github.com/lexi-lambda/hackett/blob/6ceeac05e3d2a4b2dacd39163744baf239cf65a4/hackett-lib/hackett/private/prim/base.rkt
[hackett-rebrand]: https://github.com/lexi-lambda/hackett/commit/1fd7fc905b93f68e39b9d01fedc4fb52aa44c4c4
[hackett-whats-in-a-name]: /blog/2017/01/05/rascal-is-now-hackett-plus-some-answers-to-questions/#whats-in-a-name
[higher-rank]: https://github.com/lexi-lambda/higher-rank
[higher-rank-algebraic]: https://github.com/lexi-lambda/higher-rank/tree/algebraic
[higher-rank-racket]: https://github.com/lexi-lambda/racket-higher-rank
[higher-rank-racket-type-constructors]: https://github.com/lexi-lambda/racket-higher-rank/tree/type-constructors
[joshdunf]: http://www.cs.ubc.ca/~joshdunf/
[local-type-inference-impl]: https://gist.github.com/lexi-lambda/045ba782c8a0d915bd8abf97167d3bb5
[mflatt]: http://www.cs.utah.edu/~mflatt/
[my-email]: mailto:lexi.lambda@gmail.com
[my-twitter]: https://twitter.com/lexi_lambda
[neelk]: http://www.cl.cam.ac.uk/~nk480/
[phil-freeman]: http://functorial.com
[robby]: http://eecs.northwestern.edu/~robby/
[samth]: http://www.ccs.neu.edu/home/samth/
[sixth-racketcon]: http://con.racket-lang.org/2016/
[snek]: http://snek.jneen.net
[stchang]: http://www.ccs.neu.edu/home/stchang/
[types-as-macros-talk]: https://www.youtube.com/watch?v=j5Hauz6cewM
[vacation-tweet]: https://twitter.com/lexi_lambda/status/865026650487967744

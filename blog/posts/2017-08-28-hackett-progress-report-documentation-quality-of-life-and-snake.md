    Title: Hackett progress report: documentation, quality of life, and snake
    Date: 2017-08-28T08:00:00
    Tags: hackett, racket, haskell

Three months ago, [I wrote a blog post describing my new, prototype implementation of my programming language, Hackett][realizing-hackett]. At the time, some things looked promising—the language already included algebraic datatypes, typeclasses, laziness, and even a mini, proof of concept web server. It was, however, clearly still rather rough around the edges—error messages were poor, features were sometimes brittle, the REPL experience was less than ideal, and there was no documentation to speak of. In the time since, while the language is still experimental, I have tackled a handful of those issues, and I am excited to announce [**the first (albeit quite incomplete) approach to Hackett’s documentation**][hackett-doc].

I’d recommend clicking that link above and at least skimming around before reading the rest of this blog post, as its remainder will describe some of the pieces that didn’t end up in the documentation: the development process, the project’s status, a small demo, and some other details from behind the scenes.

# A philosophy of documentation

Racket, as a project, has always had [wonderful documentation][racket-doc]. There are many reasons for this—Racket’s educational origins almost certainly play a part, and it helps that the core packages set the bar high—but one of the biggest reasons is undoubtably [Scribble, the Racket documentation tool][scribble]. Scribble is, in many ways, the embodiment of the Racket philosophy: it is a user-extensible, fully-featured, domain-specific programming language designed for typesetting, with [a powerful library for documenting Racket code][scribble-manual]. Like the Racket language itself, Scribble comes with a hygienic macro system, and in fact, all Racket libraries are trivially usable from within Scribble documents, if desired. The macro system is used to great effect to provide typesetting forms tailored to the various sorts of things a Racket programmer might wish to document, such as procedures, structures, and macros.

Scribble documents are decoupled from a rendering backend, so a single Scribble document can be rendered to plain text, a PDF, or HTML, but the HTML backend is the most useful for writing docs. Scribble documents themselves use a syntax inspired by (La)TeX’s syntax, but Scribble uses an `@` character instead of `\`. It also generalizes and regularizes TeX in many ways, creating a much more uniform language without nearly so much magic or complexity. Since Scribble’s “at-expressions” are merely an alternate syntax for Racket’s more traditional s-expressions, Scribble documents can be built out of ordinary Racket macros. For example, to document a procedure in Racket, one would use [the provided `defproc` form][defproc]:

```racket
@defproc[(add1 [z number?]) number?]{
Returns @racket[(+ z 1)].}
```

This syntax may look alien to someone more familiar with traditional, Javadoc-style documentation comments, but the results are quite impressive. The above snippet renders into [something like this][racket-add1]:

[![](/img/scribble-docs-racket-add1.png)][racket-add1]

The fact that Scribble documents are fully-fledged *programs* equips the programmer with a lot of power. One of the most remarkable tools Scribble provides is [the `scribble/example` module][scribble-example], a library that performs sandboxed evaluation as part of the rendering process. This allows Scribble documents to include REPL-style examples inline, automatically generated as part of typesetting, always kept up to date from a single source of truth: the implementation. It even provides a special `eval:check` form that enables [doctest][]-like checking, which allows documentation to serve double duty as a test suite.

Of course, Hackett is not Racket, though it shares many similarities. Fortunately, all of Racket is *designed* with the goal of supporting many different programming languages, and Scribble is no exception. Things like [`scribble/example`][scribble-example] essentially work out of the box with Hackett, and most of [`scribble/manual`][scribble-manual] can be reused. However, what about documenting algebraic datatypes? What about documenting typeclasses? Well, remember: Scribble is extensible. The `defproc` and `defstruct` forms are hardly builtins; they are defined as part of the `scribble/manual` library in terms of Scribble primitives, and [we can do the same][hackett-manual].

Hackett’s documentation already defines three new forms, `defdata`, `defclass`, and `defmethod`, for documenting algebraic datatypes, typeclasses, and typeclass methods, respectively. They typeset documentation custom-tailored to Hackett’s needs, so Hackett’s documentation need not be constrained by Racket’s design decisions. For example, one could document the `Functor` typeclass using `defclass` like this:

```racket
@defclass[(Functor f)
          [map : (forall [a b] {(a -> b) -> (f a) -> (f b)})]]{

A class of types that are @deftech{functors}, essentially types that provide a
mapping or “piercing” operation. The @racket[map] function can be viewed in
different ways:

...}
```

With only a little more than the above code, [Hackett’s documentation includes a beautifully-typeset definition of the `Functor` typeclass][hackett-functor], including examples and rich prose:

[![](/img/scribble-docs-hackett-functor.png)][hackett-functor]

Scribble makes Hackett’s documentation shine.

## A tale of two users

For a programming language, documentation is critical. Once we have grown comfortable with a language, it’s easy to take for granted our ability to work within it, but there is always a learning period, no matter how simple or familiar the language may be. When learning a new language, we often relate the languages’ concepts and features to those which we already know, which is why having a broad vocabulary of languages makes picking up new ones so much easier.

A new user of a language needs a gentle introduction to its features, structured in a logical way, encouraging this period of discovery and internalization. Such an introduction should come equipped with plenty of examples, and it shouldn’t worry itself with being an authoritative reference. Some innocent simplifications are often conducive to learning, and it is unlikely to be helpful to force the full power of a language onto a user all at once.

However, for experienced users, an authoritative reference is *exactly* what they need. While learners want tutorial-style documentation that encourages experimentation and exploration, working users of a language need something closer to a dictionary or encyclopedia: a way to look up forms and functions by name and find precise definitions, complete explanations, and hopefully a couple of examples. Such a user does not want information to be scattered across multiple chapters of explanatory text; they simply need a focused, targeted, one-stop shop for the information they’re looking for.

This dichotomy is rarely well-served by existing programming language documentation. Most programming languages suffer from either failing entirely to serve both types of users, or doing so in a way that enforces too strong a separation between the styles of documentation. For example:

  - Java ships with a quintessential example of a documentation generator: Javadoc. Java is a good case study because, although its documentation is not particularly good, it still manages to be considerably better than most languages’ docs.

    [Java’s API documentation][java-stdlib] documents its standard library, but it doesn’t document the language. Reference-style language documentation is largely relegated to the Java Language Specification, which is highly technical and rather low-level. It is more readable than the standards for some other languages, but it’s still mostly only useful to language lawyers. For Java, this ends up being mostly okay, largely because Java is a fairly *small* language that does not often change.

    On the other hand, Java’s reference documentation is inconsistent, rarely provides any examples, and certainly does not do a good job of serving new users. Java *does* provide guide-style documentation in the form of the [Java Tutorials][java-tutorials], but they are of inconsistent quality.

    More importantly, while the Java tutorials link to the API docs, the reverse is **not** true, which is a real disservice. One of the most beautiful things about the web is how information can be extensively cross-linked, and exploring links is many times easier than turning pages of a physical book. Anyone who’s explored topics on Wikipedia for an hour (or more) at a time knows how amazing this can be.

    Language documentation isn’t quite the same as an encyclopedia, but it’s a shame that Java’s documentation does not lend itself as easily to curious, open-ended learning. If the API docs frequently linked to relevant portions of the tutorials, then a user could open the Javadoc for a class or method they are using, then quickly jump to the relevant guide. As the documentation is currently organized, this is nearly impossible, and tutorials are only discovered when explicitly looking for them.

  - Other languages, such as JavaScript, are in even worse boats than Java when it comes to documentation. For whatever reason, structured documentation of any kind doesn’t seem to have caught on in the JavaScript world, probably largely because no documentation tool ships with the language, and no such tool ever became standard. Whatever the reason, JavaScript libraries’ documentation largely resides in markdown documents spread across version control repositories and various webpages.

    The closest thing that JavaScript has to official language documentation, aside from the (largely incomprehensible) language standard, is [MDN][mdn]. MDN’s docs are actually quite good, and they tend to mix lots of examples together with reference-style documentation. They’re indexed and searchable, and they have a great Google search ranking. MDN is easily my go-to place to read about core JavaScript functions.

    The trouble, of course, is that MDN only houses documentation for the standard library, and while new standards make it bigger than ever, huge amounts of critical functionality are often offloaded to separate packages. These libraries all have their own standards and styles of documentation, and virtually none of them even compare to MDN.

    This means that documentation for JavaScript libraries, even the most popular ones, tends to be all over the map. [Ramda’s documentation is nothing but a reference][ramda-docs], which makes it easy to look up information about a specific function, but nearly impossible to find anything if you don’t have a specific name to look for. In contrast, [Passport’s docs are essentially *only* a set of tutorials][passport-docs], which is great for learners, but enormously frustrating if I just want to look up what the heck a specific function or method *does*. Fortunately, [there are some libraries, like React][react-docs], that absolutely *nail* this, and they have both styles of documentation that are **actually cross-referenced**. Unfortunately, those are mostly the exceptions, not the norm.

  - [Python’s documentation is interesting][python-docs], since it includes a set of tutorials alongside the API reference, and it *also* ships a language reference written for ordinary users. In many ways, it does everything right, but disappointingly, it generally doesn’t link back to the tutorials from the API docs, even though the reverse is true. For example, the section in the tutorial on `if` links to the section in the reference about `if`, but nothing goes in the other direction, which is something of a missed opportunity.

  - [Haskell manages to be especially bad here][haskell-base] (maybe even notoriously bad) despite having an ubiquitous documentation generator, Haddock. Unfortunately, Haddock’s format makes writing prose and examples somewhat unpleasant, and very few packages provide any sort of tutorial. For those that do, the tutorial is often not included in the API docs, a common theme at this point.

    It’s generally a bad sign when your documentation tool isn’t even powerful enough to document itself, and [Haddock’s docs are pretty impressively bad, though mostly serviceable if you’re willing to look][haddock].

The takeaway here is that I just don’t think most languages’ documentation is particularly good, and programmers seem to have gotten so used to this state of affairs that the bar is set disappointingly low. Fortunately, this is another area where Racket delivers. Racket, like Python, ships with *two* pieces of documentation: the [Racket Guide][racket-guide] and the [Racket Reference][racket-reference]. The guide includes over **one hundred thousand** words of explanations and examples, and the reference includes roughly **half a million**. Racket’s documentation is impressive on its own, but what’s equally impressive is how carefully and methodically cross-linked it is. Margin notes often provide links to corresponding sections in the relevant companion manual, so it’s easy to look up a form or function by name, then quickly jump to the section of the guide explaining it.

Hackett is obviously not going to have hundreds of thousands of words worth of documentation in its first few months of existence, but it already has nearly ten thousand, and that’s not nothing. More importantly, it is structured the same way that Racket’s docs are: it’s split into the [Hackett Guide][hackett-guide] and the [Hackett Reference][hackett-reference], and the two are cross-referenced as much as possible. Haskell is a notoriously difficult language to learn, but my hope is that does not necessarily *need* to be the case. Documentation cannot make the language trivial, but my hope is that it can make it a *lot* more accessible without making it any less useful for power users.

# Rounding Hackett’s library, sanding its edges

One of the best things about sitting down and writing documentation—whether it’s for a tool, a library, or a language—is how it forces you, the author, to think about how someone else might perceive the project when seeing it for the first time. This encompasses everything: error messages, ease of installation, completeness of a standard library, friendliness of tooling, etc. Writing Hackett’s documentation forced me to make a *lot* of improvements, and while very few of them are flashy features, they make Hackett feel much less like a toy and more like a tool.

Hackett currently has no formal changelog because it is considered alpha quality, and its API is still unstable. There is no guarantee that things won’t change at any moment. Still, it’s useful to put together an ad-hoc list of changes made in the past few months. Here’s a very brief summary:

  - Hackett includes a [`Double`][hackett-double] type for working with IEEE 754 double-precision floating-point numbers.

  - Local definitions are supported via the [`let`][hackett-let] and [`letrec`][hackett-letrec] forms.

  - The prelude includes many more functions, especially [functions on lists][hackett-lists].

  - The Hackett reader has been adjusted to support using `.` as a bare symbol, since [`.` is the function composition operator][hackett-composition].

  - The Hackett REPL supports many more forms, including [ADT][hackett-data], [class][hackett-class], and [instance][hackett-instance] definitions. Additionally, the REPL now uses [`Show`][hackett-show] instances to display the results of expressions. To compensate for the inability to print non-[`Show`][hackett-show]able things, a new `(#:type expr)` syntax is permitted to print the type of *any* expression.

  - Missing instance errors are now dramatically improved, now correctly highlighting the source location of expressions that led to the error.

Alongside these changes are a variety of internal code improvements that make the Hackett code simpler, more readable, and hopefully more accessible to contributors. Many of the trickiest functions are now [heavily commented](https://github.com/lexi-lambda/hackett/blob/f472859cfc03086d39563e5c0eb81dcb2ceb49dc/hackett-lib/hackett/private/base.rkt#L77-L189) with the hope that the codebase won’t be so intimidating to people unfamiliar with Racket or the techniques behind Hackett’s typechecker. I will continue to document the internals of Hackett as I change different places of the codebase, and I have even considered writing a separate Scribble document describing the Hackett internals. It certainly wouldn’t hurt.

One of the most exciting things about documenting Hackett has been realizing just *how much* already exists. Seriously, if you have gotten to this point in the blog post but haven’t read [the actual documentation][hackett-doc] yet, I would encourage you to do so. No longer does the idea of writing real programs in this language feel out of reach; indeed, aside from potential performance problems, the language is likely extremely close to being usable for very simple things. After all, that’s the goal, isn’t it? As I’ve mentioned before, I’m writing Hackett for other people, but I’m also very much writing it for *me*: it’s a language I’d like to use.

Still, writing a general-purpose programming language is a lot of work, and I’ve known from the start that it isn’t something I can accomplish entirely on my own. While this iteration of work on Hackett is a sort of “documentation release”, it might be more accurate to call it an “accessibility release”. If you’re interested in contributing, I finally feel comfortable encouraging you to get involved!

# A demo with pictures

Now, if you’re like me, all of this documentation stuff is already pretty exciting. Still, even I view documentation as simply a means to an end, not an end in itself. Documentation is successful when it gets out of the way and makes it possible to write good code that does cool things. Let’s write some, shall we?

Hackett ships with a special package of demo libraries in the aptly-named `hackett-demo` package, which are essentially simple, lightweight bindings to existing, dynamically-typed Racket libraries. In [the previous Hackett blog post][realizing-hackett], I demonstrated the capabilities of `hackett/demo/web-server`. In this blog post, we’re going to use `hackett/demo/pict` and `hackett/demo/pict/universe`, which make it possible to write interactive, graphical programs in Hackett with just a few lines of code!

As always, we’ll start with `#lang hackett`, and we’ll import the necessary libraries:

```racket
#lang hackett

(require hackett/demo/pict
         hackett/demo/pict/universe)
```

With that, we can start immediately with a tiny example. Just to see how `hackett/demo/pict` works, let’s start by rendering a red square. We can do this by writing a `main` action that calls `print-pict`:

```racket
(main (print-pict (colorize red (filled-square 50.0))))
```

If you run the above program in DrRacket, you should see a 50 pixel red square printed into the interactions window!

![](/img/hackett-pict-red-square.png)

Using the REPL, we can inspect the type of `print-pict`:

```racket
> (#:type print-pict)
: (-> Pict (IO Unit))
```

Unsurprisingly, displaying a picture to the screen needs `IO`. However, what’s interesting is that the rest of the expression is totally pure. Take a look at the type of `filled-square`:

```racket
> (#:type filled-square)
: (-> Double Pict)
```

No `IO` to be seen! This is because “picts” are entirely *pure* values that represent images built out of simple shapes, and they can be put together to make more complex images. For example, we can put two squares next to one another:

```racket
(main (print-pict {(colorize red (filled-square 50.0))
                   hc-append
                   (colorize blue (filled-square 50.0))}))
```

This code will print out a red square to the left of a blue one.

![](/img/hackett-pict-red-blue-squares.png)

Again, `hc-append` is a simple, pure function, a binary composition operator that places two picts side by side to produce a new one:

```racket
> (#:type hc-append)
: (-> Pict (-> Pict Pict))
```

Using the various features of this toolkit, not only can we make interesting pictures and diagrams, we can even create a foundation for a game!

## Implementing a snake clone

This blog post is not a Hackett tutorial; it is merely a demo. For that reason, I am not going to spend much time explaining how the following program is built. This section is closer to annotated source code than a guide to the `pict` or `universe` libraries. Hopefully it’s still illustrative.

We’ll start by writing some type definitions. We’ll need a type to represent 2D points on a grid, as well as a type to represent a cardinal direction (to keep track of which direction the player is moving, for example). We’ll also want an `Eq` instance for our points.

```racket
(data Point (point Integer Integer))
(data Direction d:left d:right d:up d:down)

(instance (Eq Point)
  [== (λ [(point a b) (point c d)] {{a == c} && {b == d}})])
```

With these two datatypes, we can implement a `move` function that accepts a point and a direction and produces a new point for an adjacent tile:

```racket
(defn move : {Direction -> Point -> Point}
  [[d:left  (point x y)] (point {x - 1} y)]
  [[d:right (point x y)] (point {x + 1} y)]
  [[d:up    (point x y)] (point x {y - 1})]
  [[d:down  (point x y)] (point x {y + 1})])
```

The next step is to define a type for our world state. The `big-bang` library operates using a game loop, with a function to update the state that’s called each “tick”. Our state will need to hold all the information about our game, which in this case, is just three things:

```racket
(data World-State (world-state
                   Direction    ; snake direction
                   (List Point) ; snake blocks
                   (List Point) ; food blocks
                   ))
```

It will also be useful to have a functional setter for the direction, which we’ll have to write ourselves, since Hackett does not (currently) have anything like Haskell’s record syntax:

```racket
(defn set-ws-direction [[d (world-state a b c)] (world-state d b c)])
```

Next, we’ll write some top-level constants that we’ll use in our rendering function, such as the number of tiles in the game board, the size of each tile in pixels, and some simple picts that represent the tiles we’ll use to draw our game:

```racket
(def board-width 50)
(def board-height 30)
(def tile->absolute {(d* 15.0) . integer->double})
(def empty-board (blank-rect (tile->absolute board-width) (tile->absolute board-height)))

(def block (filled-square 13.0))
(def food-block (colorize red block))
(def snake-block (colorize black block))
```

Now we can write our actual `render` function. To do this, we simply need to render each `Point` in our `World-State`’s two lists as a block on an `empty-board`. We’ll write a helper function, `render-on-board`, which does exactly that:

```racket
(defn render-on-board : {Pict -> (List Point) -> Pict}
  [[pict points]
   (foldr (λ [(point x y) acc]
            (pin-over acc (tile->absolute x) (tile->absolute y) pict))
          empty-board points)])
```

This function uses `foldr` to collect each point and place the provided pict at the right location using `pin-over` on an empty board. Using `render-on-board`, we can write the `render` function in just a couple of lines:

```racket
(defn render : {World-State -> Pict}
  [[(world-state _ snake-points food-points)]
   (pin-over (render-on-board snake-block snake-points)
             0.0 0.0
             (render-on-board food-block food-points))])
```

Next, we’ll need to handle the update logic. On each tick, the snake should advance by a single tile in the direction it’s currently moving. If it runs into a food tile, it should grow one tile larger, and we need to generate a new food tile elsewhere on the board. To help with that last part, the `big-bang` library provides a `random-integer` function, which we can use to write a `random-point` action:

```racket
(def random-point : (IO Point)
  {point <$> (random-integer 0 board-width)
         <*> (random-integer 0 board-height)})
```

Hackett supports applicative notation using infix operators, so `random-point` looks remarkably readable. It also runs in `IO`, since the result is, obviously, random. Fortunately, the `on-tick` function runs in `IO` as well (unlike `render`, which must be completely pure), so we can use `random-point` when necessary to generate a new food block:

```racket
(def init! : (forall [a] {(List a) -> (List a)})
  {reverse . tail! . reverse})

(defn on-tick : {World-State -> (IO World-State)}
  [[(world-state dir snake-points food-points)]
   (let ([new-snake-point (move dir (head! snake-points))])
     (if {new-snake-point elem? food-points}
         (do [new-food-point <- random-point]
             (pure (world-state dir {new-snake-point :: snake-points}
                                {new-food-point :: (delete new-snake-point food-points)})))
         (pure (world-state dir {new-snake-point :: (init! snake-points)}
                            food-points))))])
```

This function is the most complicated one in the whole program, but it’s still not terribly complex. It figures out what the snake’s next location is and binds it to `new-snake-point`, then checks if there is a food block at that location. If there is, it generates a `new-food-point`, then puts it in the new world state. Otherwise, it removes the last snake point and continues as usual.

The game is already almost completely written. The next step is just to handle key events, which are obviously important for allowing the player to control the snake. Fortunately, this is easy, since we can just use our `set-ws-direction` function that we wrote earlier:

```racket
(defn on-key : {KeyEvent -> World-State -> (IO World-State)}
  [[ke:left ] {pure . (set-ws-direction d:left)}]
  [[ke:right] {pure . (set-ws-direction d:right)}]
  [[ke:up   ] {pure . (set-ws-direction d:up)}]
  [[ke:down ] {pure . (set-ws-direction d:down)}]
  [[_       ] {pure . id}])
```

The `on-key` function runs in `IO`, but we don’t actually need that power, since all of our keypress update logic is completely pure, so we just wrap everything in `pure`.

We’re almost done now—all we need to do is set up the *initial* state when the game begins. We’ll write a small binding that creates a world state with the snake in the middle of the board and some random food locations scattered about:

```racket
(def initial-state
  (do [initial-food <- (sequence (take 5 (repeat random-point)))]
      (pure (world-state d:right
                         {(point 25 15) :: (point 24 15) :: (point 23 15) :: nil}
                         initial-food))))
```

Notably, we can use the `repeat` function to create an infinite list of `random-point` actions, `take` the first five of them, then call `sequence` to execute them from left to right. Now, all we have to do is put the pieces together in a `main` block:

```racket
(main (do [state <- initial-state]
          (big-bang state
            #:to-draw render
            #:on-tick on-tick 0.2
            #:on-key on-key)))
```

And that’s it! We haven’t implemented any win or loss conditions, but the basics are all there. In 80 lines of code, we’ve implemented a working snake game in Hackett.

![](/img/hackett-snake-animation.gif)

# Contributing to Hackett

If you are excited enough about Hackett to be interested in contributing, your first question is very likely “What can I do?” or “Where do I start?” My answer to that is (perhaps a little unhelpfully): it depends! My general recommendation is to try and write something with Hackett, and if you run into anything that prevents you from accomplishing your goal, look into what would need to be changed to support your program. Having a use case is a great way to come up with useful improvements.

On the other hand, you might not have anything in mind, or you might find Hackett’s scope a little too overwhelming to just jump right in and start contributing. Fortunately, [Hackett has an issue tracker][hackett-issues], so feel free to take a look and pick something that looks interesting and achievable. Alternatively, the standard library can always use fleshing out, and quite a lot of that can be written without ever even touching the scary Hackett internals.

Additionally, if you have any questions, please don’t hesitate to ask them! If you have a question about the codebase, get stuck implementing something, or just don’t know where to start, feel free to [open an issue on GitHub][hackett-issues], send me a message on the `#racket` IRC channel on Freenode, or ping me on [the Racket Slack team][racket-slack].

# Acknowledgements

Speaking of contributors, I’m excited to say that this is the first time I can truly say Hackett includes code written by someone other than me! I want to call attention to [Samuel Gélineau, aka gelisam][gelisam], who is officially the second contributor to Hackett. He helped to implement the new approach the Hackett REPL uses for printing expressions, which ended up being quite useful when implementing some of the other REPL improvements.

Additionally, I want to specially thank [Matthew Flatt][mflatt], [Robby Findler][robby], and [Sam Tobin-Hochstadt][samth] for being especially responsive and helpful to my many questions about Scribble and the Racket top level. Racket continues to be extremely impressive, both as a project and as a community.

Finally, many thanks to the various people who have expressed interest in the project and continue to push me and ask questions. Working on Hackett is a lot of work—both time and effort—and it’s your continued enthusiasm that inspires me to put in the hours.

[defproc]: http://docs.racket-lang.org/scribble/doc-forms.html#%28form._%28%28lib._scribble%2Fmanual..rkt%29._defproc%29%29
[doctest]: https://docs.python.org/3/library/doctest.html
[gelisam]: https://github.com/gelisam
[hackett-class]: http://docs.racket-lang.org/hackett/reference-typeclasses.html#%28form._%28%28lib._hackett%2Fmain..rkt%29._class%29%29
[hackett-composition]: http://docs.racket-lang.org/hackett/reference-datatypes.html#%28def._%28%28lib._hackett%2Fmain..rkt%29._..%29%29
[hackett-data]: http://docs.racket-lang.org/hackett/reference-datatypes.html#%28form._%28%28lib._hackett%2Fmain..rkt%29._data%29%29
[hackett-doc]: https://pkg-build.racket-lang.org/doc/hackett@hackett-doc/
[hackett-double]: http://docs.racket-lang.org/hackett/reference-datatypes.html#%28form._%28%28lib._hackett%2Fmain..rkt%29._.Double%29%29
[hackett-functor]: http://docs.racket-lang.org/hackett/reference-typeclasses.html#%28def._%28%28lib._hackett%2Fmain..rkt%29._.Functor%29%29
[hackett-guide]: http://docs.racket-lang.org/hackett/guide.html
[hackett-instance]: http://docs.racket-lang.org/hackett/reference-typeclasses.html#%28form._%28%28lib._hackett%2Fmain..rkt%29._instance%29%29
[hackett-issues]: https://github.com/lexi-lambda/hackett/issues
[hackett-let]: http://docs.racket-lang.org/hackett/reference-syntactic-forms.html#%28form._%28%28lib._hackett%2Fmain..rkt%29._let%29%29
[hackett-letrec]: http://docs.racket-lang.org/hackett/reference-syntactic-forms.html#%28form._%28%28lib._hackett%2Fmain..rkt%29._letrec%29%29
[hackett-lists]: http://docs.racket-lang.org/hackett/reference-datatypes.html#%28part._reference-lists%29
[hackett-manual]: https://github.com/lexi-lambda/hackett/blob/f472859cfc03086d39563e5c0eb81dcb2ceb49dc/hackett-doc/scribble/manual/hackett.rkt
[hackett-reference]: http://docs.racket-lang.org/hackett/reference.html
[hackett-show]: http://docs.racket-lang.org/hackett/reference-typeclasses.html#%28def._%28%28lib._hackett%2Fmain..rkt%29._.Show%29%29
[haddock]: https://www.haskell.org/haddock/
[haskell-base]: https://hackage.haskell.org/package/base
[java-stdlib]: https://docs.oracle.com/javase/8/docs/api/
[java-tutorials]: https://docs.oracle.com/javase/tutorial/
[mdn]: https://developer.mozilla.org/en-US/
[mflatt]: http://www.cs.utah.edu/~mflatt/
[passport-docs]: http://passportjs.org/docs
[python-docs]: https://docs.python.org/3/index.html
[racket-add1]: http://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._add1%29%29
[racket-doc]: http://docs.racket-lang.org
[racket-guide]: http://docs.racket-lang.org/guide/index.html
[racket-reference]: http://docs.racket-lang.org/reference/index.html
[racket-slack]: http://racket-slack.herokuapp.com
[ramda-docs]: http://ramdajs.com/docs/
[react-docs]: https://facebook.github.io/react/docs/hello-world.html
[realizing-hackett]: /blog/2017/05/27/realizing-hackett-a-metaprogrammable-haskell/
[robby]: http://eecs.northwestern.edu/~robby/
[samth]: http://www.ccs.neu.edu/home/samth/
[scribble]: http://docs.racket-lang.org/scribble/index.html
[scribble-example]: http://docs.racket-lang.org/scribble/eval.html
[scribble-manual]: http://docs.racket-lang.org/scribble/plt-manuals.html

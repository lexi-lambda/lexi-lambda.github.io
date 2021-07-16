    Title: User-programmable infix operators in Racket
    Date: 2017-08-12T16:26:05
    Tags: racket, hackett, macros

Lisps are not known for infix operators, quite the opposite; infix operators generally involve more syntax and parsing than Lispers are keen to support. However, in [Hackett][hackett], all functions are curried, and variable-arity functions do not exist. Infix operators are almost necessary for that to be palatable, and though there are other reasons to want them, it may not be obvious how to support them without making the reader considerably more complex.

Fortunately, if we require users to syntactically specify where they wish to use infix expressions, support for infix operators is not only possible, but can support be done *without* modifying the stock `#lang racket` reader. Futhermore, the resulting technique makes it possible for fixity information to be specified locally in a way that cooperates nicely with the Racket macro system, allowing the parsing of infix expressions to be manipulated at compile-time by users’ macros.

<!-- more -->

# Our mission

Before we embark, let’s clarify our goal. We want to support infix operators in Racket, of course, but that could mean a lot of different things! Let’s start with what we *do* want:

  - Infix operators should be user-extensible, not limited to a special set of built-in operators.

  - Furthermore, operators’ names should not be restricted to a separate “operator” character set. Any valid Lisp identifier should be usable as an infix operator.

  - We want to be able to support fixity/associativity annotations. Some operators should associate to the left, like subtraction, but others should associate to the right, like `cons`. This allows `5 - 1 - 2` to be parsed as `(- (- 5 1) 2)`, but `5 :: 1 :: nil` to be parsed as `(:: 5 (:: 1 nil))`.

These are nice goals, but we also won’t be too ambitious. In order to keep things simple and achievable, we’ll keep the following restrictions:

  - We will **not** permit infix expressions in arbitrary locations, since that would be impossible to parse given how we want to allow users to pick any names for operators they wish. Instead, infix expressions must be wrapped in curly braces, e.g. replacing `(+ 1 2)` with `{1 + 2}`.

  - Our implementation will **not** support any notion of operator precedence; all operators will have equal precedence, and it will be illegal to mix operators of different associativity in the same expression. Precedence is entirely possible to implement in theory, but it would be considerably more work, so this blog post does not include it.

  - All operators will be binary, and we will **not** support unary or mixfix operators. My intuition is that this technique should be able to be generalized to both of those things, but it would be considerably more complicated.

With those points in mind, what would the interface for our infix operator library look like for our users? Ideally, something like this:

```racket
#lang racket

(require (prefix-in racket/base/ racket/base)
         "infix.rkt")

(define-infix-operator - racket/base/- #:fixity left)
(define-infix-operator :: cons #:fixity right)

{{2 - 1} :: {10 - 3} :: '()}
; => '(1 7)
```

Let’s get started.

# Implementing infix operators

Now that we know what we want, how do we get there? Well, there are a few pieces to this puzzle. We’ll need to solve a two main problems:

  1. How do we “hook into” expressions wrapped with curly braces so that we can perform a desugaring pass?

  2. How can we associate fixity information with certain operators?

We’ll start by tackling the first problem, since its solution will inform the answer to the second. Since we won’t have any fixity information to start with, we’ll just assume that all operators associate left by default.

So, how *do* we detect if a Racket expression is surrounded by curly braces? Normally, in `#lang racket`, parentheses, square brackets, and curly braces are all interchangeable. Indeed, if you use curly braces in the REPL, you will find that they are treated *exactly* the same as parentheses:

```racket
> {+ 1 2}
3
```

If they are treated identically, giving them special behavior might seem hopeless, but don’t despair! Racket is no ordinary programming language, and it provides some tools to help us out here.

Someone who has worked with Lisps before is likely already aware that Lisp source code is a very direct representation of its AST, composed mostly of lists, pairs, symbols, numbers, and strings. In Racket, this is also true, but Racket also wraps these datums in boxes known as [*syntax objects*][syntax-object]. Syntax objects contain extra metadata about the code, most notably its lexical context, necessary for Racket’s hygiene system. However, syntax objects can also contain arbitrary metadata, known as [*syntax properties*][syntax-property]. Macros can attach arbitrary values to the syntax objects they produce using syntax properties, and other macros can inspect them. Racket’s [*reader*][racket-reader] (the syntax parser that turns program text into Racket syntax objects) also attaches certain syntax properties as part of its parsing process. One of those is named [`'paren-shape`][paren-shape].

This syntax property, as the name implies, keeps track of the shape of parentheses in syntax objects. You can see that for yourself by inspecting the property’s value for different syntax objects in the REPL:

```racket
> (syntax-property #'(1 2 3) 'paren-shape)
#f
> (syntax-property #'[1 2 3] 'paren-shape)
#\[
> (syntax-property #'{1 2 3} 'paren-shape)
#\{
```

This syntax property gives us the capability to distinguish between syntax objects that use curly braces and those that don’t, which is a step in the right direction, but it still doesn’t give us any hook with which we can change the behavior of certain expressions. Fortunately, there’s something else that can.

## Customizing application

Racket is a language *designed* to be extended, and it provides a variety of hooks in the language for the purposes of tweaking pieces in minor ways. One such hook is named [`#%app`][hash-percent-app], which is automatically introduced by the macroexpander whenever it encounters a function application. That means it effectively turns this:

```racket
(+ 1 2)
```

…into this:

```racket
(#%app + 1 2)
```

What’s special about `#%app` is that the macroexpander will use whichever `#%app` is in scope in the expression’s lexical context, so if we write our own version of `#%app`, it will be used instead of the one from `#lang racket`. This is what we will use to hook into ordinary Racket expressions.

To write our custom version of `#%app`, we will use the usual tool: Racket’s industrial-strength macro-authoring DSL, [`syntax/parse`][syntax-parse]. We’ll also use a helper library that provides some tools for pattern-matching on syntax objects with the `'paren-shape` syntax property, [`syntax/parse/class/paren-shape`][syntax-classes-paren-shape]. Using these, we can transform expressions that are surrounded in curly braces differently from how we would transform expressions surrounded by parentheses:

```racket
#lang racket

(require (for-syntax syntax/parse/class/paren-shape)
         (prefix-in racket/base/ racket/base)
         syntax/parse/define)

(define-syntax-parser #%app
  [{~braces _ arg ...}
   #'(#%infix arg ...)]
  [(_ arg ...)
   #'(racket/base/#%app arg ...)])
```

This code will transform any applications surrounded in curly braces into one that starts with `#%infix` instead of `#%app`, so `{1 + 2}` will become `(#%infix 1 + 2)`, for example. The identifier `#%infix` isn’t actually special in any way, it just has a funny name, but we haven’t actually defined `#%infix` yet, so we need to do that next!

To start, we’ll just handle the simplest case: infix expressions with precisely three subexpressions, like `{1 + 2}`, should be converted into the equivalent prefix expressions, in this case `(+ 1 2)`. We can do this with a simple macro:

```racket
(define-syntax-parser #%infix
  [(_ a op b)
   #'(racket/base/#%app op a b)])
```

Due to the way Racket propagates syntax properties, we explicitly indicate that the resulting expansion should use the `#%app` from `racket/base`, which will avoid any accidental infinite recursion between our `#%app` and `#%infix`. With this in place, we can now try our code out in the REPL, and believe it or not, we now support infix expressions with just those few lines of code:

```racket
> (+ 1 2)
3
> {1 + 2}
3
```

That’s pretty cool!

Of course, we probably want to support infix applications with more than just a single binary operator, such as `{1 + 2 + 3}`. We can implement that just by adding another case to `#%infix` that handles more subforms:

```racket
(define-syntax-parser #%infix
  [(_ a op b)
   #'(racket/base/#%app op a b)]
  [(_ a op b more ...)
   #'(#%infix (#%infix a op b) more ...)])
```

…and now, just by adding those two lines, we support arbitrarily-large sequences of infix operators:

```racket
> {1 + 2 + 3}
6
> {1 + 2 + 3 + 4}
10
```

I don’t know about you, but I think being able to do this in less than 20 lines of code is pretty awesome. We can even mix different operators in the same expression:

```racket
> {1 + 2 * 3 - 4}
5
```

Of course, all of our infix expressions currently assume that all operators associate left, as was our plan. In general, though, there are lots of useful operators that associate right, such as `cons`, nested `->` types or contracts for curried functions, and `expt`, the exponentiation operator.

## Tracking operator fixity

Clearly, we need some way to associate operator fixity with certain identifiers, and we need to be able to do it at compile-time. Fortunately, Racket has a very robust mechanism for creating compile-time values. Unfortunately, simply associating metadata with an identifier is a little less convenient than it could be, but there is a general technique that can be done with little boilerplate.

Essentially, Racket (like Scheme) uses a `define-syntax` form to define macros, which is what `define-syntax-parser` eventually expands into. However, unlike Scheme, Racket’s `define-syntax` is not *just* for defining macros—it’s for defining arbitrary bindings with compile-time (aka “phase 1”) values. Using this, we can define bindings that have entirely arbitrary values at compile-time, including plain data like numbers or strings:

```racket
(define-syntax foo 3)
```

Once a binding has been defined using `define-syntax`, a macro can look up the value associated with it by using the [`syntax-local-value`][syntax-local-value] function, which returns the compile-time value associated with an identifier:

```racket
(begin-for-syntax
  (println (syntax-local-value #'foo)))
; => 3
```

The cool thing is that `syntax-local-value` gets the value associated with a specific *binding*, not a specific name. This means a macro can look up the compile-time value associated with an identifier provided to it as a subform. This is close to what we want, since we could use `syntax-local-value` to look up something associated with our infix operator bindings, but the trouble is that they would then cease to be usable as ordinary functions. For example, if you try and use the `foo` binding from the above example as an expression, Racket will complain about an “illegal use of syntax”, which makes sense, because `foo` is not bound to anything at runtime.

To solve this problem, we can use something of a trick: any compile-time binding that happens to have a procedure as its value will be treated like a macro—that is, using it as an expression will cause the macroexpander to invoke the procedure with a syntax object representing the macro invocation, and the procedure is expected to produce a new syntax object as output. Additionally, Racket programmers can make custom datatypes valid procedures by using the [`prop:procedure`][prop-procedure] structure type property.

If you are not familiar with the Racket macro system, this probably sounds rather complicated, but in practice, it’s not as confusing as it might seem. The trick here is to create a custom structure type at compile-time that we can use to track operator fixity alongside its runtime binding:

```racket
(require (for-syntax syntax/transformer))

(begin-for-syntax
  (struct infix-operator (runtime-binding fixity)
    #:property prop:procedure
    (λ (operator stx)
      ((set!-transformer-procedure
        (make-variable-like-transformer
         (infix-operator-runtime-binding operator)))
       stx))))
```

This is quite the magical incantation, and all the details of what is going on here are outside the scope of this blog post. Essentially, though, we can use values of this structure as a compile-time binding that will act just like the identifier provided for `runtime-binding`, but we can also include a value of our choosing for `fixity`. Here’s an example:

```racket
(define-syntax :: (infix-operator #'cons 'right))
```

This new `::` binding will act, in every way, just like `cons`. If we use it in the REPL, you can see that it acts exactly the same:

```racket
> (:: 1 '())
'(1)
```

However, we can also use `syntax-local-value` to extract this binding’s fixity at compile-time, and that’s what makes it interesting:

```racket
(begin-for-syntax
  (println (infix-operator-fixity (syntax-local-value #'::))))
; => 'right
```

Using this extra compile-time information, we can adjust our `#%infix` macro to inspect bindings and determine their fixity, then use that to make decisions about parsing. Just like we used `syntax/parse/class/paren-shape` to make decisions based on the `'paren-shape` syntax property, we can use [`syntax/parse/class/local-value`][syntax-classes-local-value] to pattern-match on bindings with a particular compile-time value. We’ll wrap this in a syntax class of our own to make the code easier to read:

```racket
(begin-for-syntax
  (define-syntax-class infix-op
    #:description "infix operator"
    #:attributes [fixity]
    [pattern {~var op (local-value infix-operator?)}
             #:attr fixity (infix-operator-fixity (attribute op.local-value))]))
```

Now, we can update `#%infix` to use our new `infix-op` syntax class:

```racket
(define-syntax-parser #%infix
  [(_ a op:infix-op b)
   #'(racket/base/#%app op a b)]
  [(_ a op:infix-op b more ...)
   #:when (eq? 'left (attribute op.fixity))
   #'(#%infix (#%infix a op b) more ...)]
  [(_ more ... a op:infix-op b)
   #:when (eq? 'right (attribute op.fixity))
   #'(#%infix more ... (#%infix a op b))])
```

Notably, we now require all operators to be bound to compile-time infix operator values, and we include two conditions via `#:when` clauses. These clauses check to ensure that the operator in question has the expected fixity before committing to that clause; if the condition fails, then parsing backtracks. Using this new definition of `#%infix`, we can successfully use `::` in an infix expression, and it will be parsed with the associativity that we expect:

```racket
> {1 :: 2 :: 3 :: '()}
'(1 2 3)
```

Exciting!

## A nicer interface for defining infix operators

We currently have to define infix operators by explicitly using `define-syntax`, but this is not a very good interface. Users of infix syntax probably don’t want to have to understand the internal workings of the infix operator implementation, so we just need to define one final macro to consider this done: the `define-infix-operator` form from the example at the very beginning of this blog post.

Fortunately, this macro is absolutely trivial to write. In fact, we can do it in a mere three lines of code, since it’s very minor sugar over the `define-syntax` definitions we were already writing:

```racket
(define-simple-macro (define-infix-operator op:id value:id
                       #:fixity {~and fixity {~or {~datum left} {~datum right}}})
  (define-syntax op (infix-operator #'value 'fixity)))
```

With this in hand, we can define some infix operators with a much nicer syntax:

```racket
(define-infix-operator + racket/base/+ #:fixity left)
(define-infix-operator - racket/base/- #:fixity left)
(define-infix-operator * racket/base/* #:fixity left)
(define-infix-operator / racket/base// #:fixity left)

(define-infix-operator ^ expt #:fixity right)
(define-infix-operator :: cons #:fixity right)
```

With these simple definitions, we can write some very nice mathematical expressions that use infix syntax, in ordinary `#lang racket`:

```racket
> {1 + 2 - 4}
-1
> {2 ^ 2 ^ 3}
256
> {{2 ^ 2} ^ 3}
64
```

And you know what’s most amazing about this? The entire thing is **only 50 lines of code**. Here is the entire implementation of infix operators from this blog post in a single code block, with absolutely nothing hidden or omitted:

```racket
#lang racket

(require (for-syntax syntax/parse/class/local-value
                     syntax/parse/class/paren-shape
                     syntax/transformer)
         (prefix-in racket/base/ racket/base)
         syntax/parse/define)

(begin-for-syntax
  (struct infix-operator (runtime-binding fixity)
    #:property prop:procedure
    (λ (operator stx)
      ((set!-transformer-procedure
        (make-variable-like-transformer
         (infix-operator-runtime-binding operator)))
       stx)))

  (define-syntax-class infix-op
    #:description "infix operator"
    #:attributes [fixity]
    [pattern {~var op (local-value infix-operator?)}
             #:attr fixity (infix-operator-fixity (attribute op.local-value))]))

(define-syntax-parser #%app
  [{~braces _ arg ...}
   #'(#%infix arg ...)]
  [(_ arg ...)
   #'(racket/base/#%app arg ...)])

(define-syntax-parser #%infix
  [(_ a op:infix-op b)
   #'(racket/base/#%app op a b)]
  [(_ a op:infix-op b more ...)
   #:when (eq? 'left (attribute op.fixity))
   #'(#%infix (#%infix a op b) more ...)]
  [(_ more ... a op:infix-op b)
   #:when (eq? 'right (attribute op.fixity))
   #'(#%infix more ... (#%infix a op b))])

(define-simple-macro (define-infix-operator op:id value:id
                       #:fixity {~and fixity {~or {~datum left} {~datum right}}})
  (define-syntax op (infix-operator #'value 'fixity)))

(define-infix-operator + racket/base/+ #:fixity left)
(define-infix-operator - racket/base/- #:fixity left)
(define-infix-operator * racket/base/* #:fixity left)
(define-infix-operator / racket/base// #:fixity left)

(define-infix-operator ^ expt #:fixity right)
(define-infix-operator :: cons #:fixity right)
```

Racket is a hell of a programming language.

# Applications, limitations, and implications

This blog post has outlined a complete, useful model for infix operators, and it is now hopefully clear how they work, but many of the most interesting properties of this implementation are probably not obvious. As far as I can make out, this embedding of infix operators into a macro system is novel, and I am *almost certain* that the way this implementation tracks fixity information is unique. One of the most interesting capabilities gained from this choice of implementation is the ability for macros to define infix operators and control their fixity, even *locally*.

What does this mean? Well, remember that infix operators are just special syntax bindings. Racket includes a variety of forms for binding or adjusting macros locally, such as `let-syntax` and `syntax-parameterize`. Using these tools, it would be entirely possible to implement a `with-fixity` macro, that could adjust the fixity of an operator within a syntactic block. This could be used, for example, to make `/` right associative within a block of code:

```racket
> {1 / 2 / 3}
1/6
> (with-fixity ([/ right])
    {1 / 2 / 3})
1 1/2
```

In fact, this macro is hardly theoretical, since it could be implemented in a trivial 7 lines, simply expanding to uses of `splicing-let` and `splicing-let-syntax`:

```racket
(define-simple-macro
  (with-fixity ([op:id {~and fixity {~or {~datum left} {~datum right}}}] ...)
    body ...)
  #:with [op-tmp ...] (generate-temporaries #'[op ...])
  (splicing-let ([op-tmp op] ...)
    (splicing-let-syntax ([op (infix-operator #'op-tmp 'fixity)] ...)
      body ...)))
```

This is not especially useful given the current set of infix operator features, but it’s easy to imagine how useful it could be in a system that also supported a notion of precedence. It is not entirely uncommon to encounter certain expressions that could be more cleanly expressed with a local set of operator precedence rules, perhaps described as a set of relations *between* operators rather than a global table of magic precedence numbers. With traditional approaches to infix operators, parsing such code would be difficult without a very rigid syntactic structure, but this technique makes it easy.

As mentioned at the beginning of this blog post, this technique is also not merely a novelty—as of now, I am actively using this in [Hackett][hackett] to support infix operators with all of the features outlined here. The Hackett implementation is a little bit fancier than the one in this blog post, since it works harder to produce better error messages. It explicitly disallows mixing left associative and right associative operators in the same expression, so it does some additional validation as part of expansion, and it arranges for source location information to be copied onto the result. It also make a different design decision to allow *any* expression to serve as an infix operator, assuming left associativity if no fixity annotation is available.

If you’re interested in the code behind the additional steps Hackett takes to make infix operators more usable and complete, take a look at [this file for the definition of infix bindings](https://github.com/lexi-lambda/hackett/blob/0d177d00a9ee96f30dd76761f1cb86f15830779f/hackett-lib/hackett/private/infix.rkt), as well as [this file for the defintion of infix application](https://github.com/lexi-lambda/hackett/blob/0d177d00a9ee96f30dd76761f1cb86f15830779f/hackett-lib/hackett/private/kernel.rkt#L80-L101). My hope is to eventually add support for some sort of precedence information, though who knows—maybe infix operators will be easier to reason about if the rules are kept extremely simple. I am also considering adding support for so-called “operator sections” at some point, which would allow things like `{_ - 1}` to serve as a shorthand for `(lambda [x] {x - 1})`, but I haven’t yet decided if I like the tradeoffs involved.

It’s possible that this implementation of infix operators might also be useful in languages in the Racket ecosystem besides Hackett. However, I’m not sure it makes a ton of sense in `#lang racket` without modifications, as variadic functions subsume many of the cases where infix operators are needed in Haskell. If there is a clamoring for this capability, I would be happy to consider extracting the functionality into a library, but as of right now, I don’t have any plans to do so.

Finally, the main point of this blog post is to showcase how easy it is to do things in Racket that would be impossible in most languages and difficult even in most Lisps. It also helps to show off how Hackett is already benefitting from those capabilities: while this particular feature is built-in to `#lang hackett`, there’s no reason something similar but more powerful couldn’t be built as a separate library by a *user* of Hackett. Even as Hackett’s author, I think that’s exciting, since makes it possible for users to experiment with improvements to the language on their own. Some of those improvements may eventually be rolled into the core language or standard library, but many of them can likely live effectively in separate libraries, accessible on-demand to those who need them. After all, that’s one of Racket’s most important promises—languages as libraries—and it’s why Hackett is a part of the Racket ecosystem.

[hackett]: https://github.com/lexi-lambda/hackett
[hash-percent-app]: http://docs.racket-lang.org/reference/application.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._~23~25app%29%29
[paren-shape]: http://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._30._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
[prop-procedure]: http://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._prop~3aprocedure%29%29
[racket-reader]: http://docs.racket-lang.org/guide/Pairs__Lists__and_Racket_Syntax.html#%28tech._reader%29
[syntax-classes-local-value]: http://docs.racket-lang.org/syntax-classes/index.html#%28mod-path._syntax%2Fparse%2Fclass%2Flocal-value%29
[syntax-classes-paren-shape]: http://docs.racket-lang.org/syntax-classes/index.html#%28mod-path._syntax%2Fparse%2Fclass%2Fparen-shape%29
[syntax-local-value]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-value%29%29
[syntax-object]: http://docs.racket-lang.org/reference/syntax-model.html#%28tech._syntax._object%29
[syntax-parse]: http://docs.racket-lang.org/syntax/stxparse.html
[syntax-property]: http://docs.racket-lang.org/reference/stxprops.html#%28tech._syntax._property%29

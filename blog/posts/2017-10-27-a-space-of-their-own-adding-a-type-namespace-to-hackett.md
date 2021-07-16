    Title: A space of their own: adding a type namespace to Hackett
    Date: 2017-10-27T17:41:17
    Tags: hackett, racket, programming languages

As previously discussed on this blog, [my programming language, Hackett][hackett], is a fusion of two languages, Haskell and Racket. What happens when two distinctly different programming languages collide? Hackett recently faced that very problem when it came to the question of namespacing: Haskell has two namespaces, one for values and another for types, but Racket is a staunch Lisp-1 with a single namespace for all bindings. Which convention should Hackett adopt?

For now, at least, the answer is that Hackett will emulate Haskell: **Hackett now has two namespaces**. Of course, Hackett is embedded in Racket, so what did it take to add an entirely new namespace to a language that possesses only one? The answer was a little more than I had hoped, but it was still remarkably simple given the problem: after two weeks of hacking, I’ve managed to get something working.

# Why two namespaces?

Before delving into the mechanics of how multi-namespace Hackett is implemented, it’s important to understand what Hackett’s namespaces actually are and why they exist in the first place. Its host language, Racket, is a descendant of Scheme, a Lisp derivative that famously chose to only use a single namespace. This means everything—from values to functions to classes—lives in a single namespace in Racket.

This is in stark contrast to Common Lisp, which opts to divide bindings into many namespaces, most notably pushing functions into a separate namespace from other variables. You can see this difference most strikingly when applying higher-order functions. In Racket, Clojure, and Scheme, functions can be passed freely as values:

```racket
> (map first '((1 a) (2 b) (3 c)))
'(1 2 3)
```

In Common Lisp and other languages with two namespaces, functions may still be passed as values, but the programmer must explicitly *annotate* when they wish to use a value from a different namespace:

```lisp
> (mapcar #'car '((1 a) (2 b) (3 c)))
(1 2 3)
```

The Common Lisp `#'x` reader abbreviation is equivalent to `(function x)`, and `function` is a special form that references a value in the function namespace.

While this distinction is somewhat arbitrary, it is generally my belief that the Scheme approach was, indeed, the right one. Runtime values are values, whether they are numbers, strings, or functions, and they ought to all be treated as equal citizens. After all, if a programmer wishes to define their own function-like thing, they should not be forced to make their abstraction a second-class citizen merely because it is slightly different from the built-in notion of a function. Higher-order functional programming encourages treating functions as ordinary values, and an arbitrary stratification of the namespace is antithetical to that mental model.

However, Hackett is a little different from all of the aforementioned languages because Hackett has *types*. Types are rather different from runtime values because they do not exist at all at runtime. One cannot use a type where a value is expected, nor can one use a value where a type is expected, so this distinction is *always* syntactically unambiguous.[^1] Even if types and values live in separate namespaces, there is no need for a `type` form a la CL’s `function` because it can always be determined implicitly.

For this reason, it makes a great deal of sense for Hackett to have separate type and value namespaces, permitting declarations such as the following:

```racket
(data (Tuple a b) (Tuple a b))
```

This defines a binding named `Tuple` at the type level, which is a *type constructor* of two arguments that produces a type of kind `*`,[^2] and another binding named `Tuple` at the value level, which is a *value constructor* of two arguments that produces a value of type `(Tuple a b)`.

But why do we want to overload names in this way, anyway? How hard would it really be to just name the value constructor `tuple` instead of `Tuple`? Well, it wouldn’t be hard at all, if it weren’t for the unpleasant ambiguity such a naming convention introduces when pattern-matching. Consider the following code snippet:

```racket
(data Foo bar (baz Integer))

(defn foo->integer : {Foo -> Integer}
  [[bar    ] 0]
  [[(baz y)] y])
```

This works fine. But what happens if the programmer decides to change the name of the `bar` value?

```racket
(data Foo qux (baz Integer))

(defn foo->integer : {Foo -> Integer}
  [[bar    ] 0]
  [[(baz y)] y])
```

Can you spot the bug? Disturbingly, this code *still compiles*! Even though `bar` is not a member of `Foo` anymore, it’s still a valid pattern, since names used as patterns match anything, just as the `y` pattern matches against any integer inside the `baz` constructor. If Hackett had a pattern redundancy checker, it could at least hopefully catch this mistake, but as things are, this could would silently compile and do the wrong thing: `(foo->integer (baz 42))` will still produce `0`, not `42`, since the first case always matches.

Haskell escapes this flaw by syntactically distinguishing between patterns and ordinary bindings by requiring all constructors start with an uppercase letter. This means that programmers often want to define data constructors and type constructors with the same name, such as the `Tuple` example above, which is illegal if a programming language only supports a single namespace.

Although Hackett now supports two namespaces, it does not currently enforce this naming convention, but it seems like an increasingly good idea. Separating the namespaces is the biggest hurdle needed to implement such a feature, and happily, it is now complete. The `Tuple` example from above is perfectly legal Hackett.

# Adding namespaces to a language

Hopefully, we now agree that it would be nice if Hackett had two namespaces, but that doesn’t really get us any closer to being able to *implement* such a feature. At its core, Hackett is still a Racket language, and Racket’s binding structure has no notion of namespaces. How can it possibly support a language with more than one namespace?

Fortunately, Racket is no ordinary language—it is a language with a highly formalized notion of lexical scope, and many of its low-level scope control features are accessible to ordinary programmers. Before we get into the details, however, a forewarning: **the remainder of this blog post is *highly technical*, and some of it involves some of the more esoteric corners of Racket’s macro system**. This blog post is *not* representative of most macros written in Racket, nor is it at all necessary to understand these things to be a working Racket or Hackett macrologist. It is certainly not a tutorial on any of these concepts, so if you find it intimidating, there is no shame in skipping the rest of this post! If, however, you think you can handle it, or if you simply want to stare into the sun, by all means, read on.

## Namespaces as scopes

With that disclaimer out of the way, let’s begin. As of this writing, the current Racket macroexpander uses a scoping model known as [*sets of scopes*][sets-of-scopes], which characterizes the binding structure of a program by annotating identifiers with sets of opaque markers known as “scopes”. The details of Racket’s macro system are well outside the scope of this blog post, but essentially, two identifiers with the same name can be made to refer to different bindings by adding a unique scope to each identifier.

Using this system of scopes, it is surprisingly simple to create a system of two namespaces: we only need to arrange for all identifiers in a value position to have a particular scope, which we will call the *value scope*, and all identifiers in type position must have a different scope, which we will call the *type scope*. How do we create these scopes and apply them to identifiers? In Racket, we use a function called [`make-syntax-introducer`][make-syntax-introducer], which produces a function that encapsulates a fresh scope. This function can be applied to any syntax object (Racket’s structured representation of code that includes lexical binding information) to do one of three things: it can *add* the scope to all pieces of the syntax object, *remove* the scope, or *flip* the scope (that is, add it to pieces of the syntax object that do not have it and remove it from pieces that do have it). In practice, this means we need to call `make-syntax-introducer` once for each namespace:

```racket
(begin-for-syntax
  (define value-introducer (make-syntax-introducer))
  (define type-introducer (make-syntax-introducer)))
```

We define these in a `begin-for-syntax` block because these definitions will be used in our compile-time macros (aka “phase 1”), not in runtime code (aka “phase 0”). Now, we can write some macros that use these introducer functions to apply the proper scopes to their contents:

```racket
(require syntax/parse/define)

(define-simple-macro (begin/value form ...)
  #:with [form* ...] (map (λ (stx) (value-introducer stx 'add))
                          (attribute form))
  (begin form* ...))

(define-simple-macro (begin/type form ...)
  #:with [form* ...] (map (λ (stx) (type-introducer stx 'add))
                          (attribute form))
  (begin form* ...))
```

Each of these two forms is like `begin`, which is a Racket form that is, for our purposes, essentially a no-op, but it applies `value-introducer` or `type-introducer` to add the appropriate scope. We can test that this works by writing a program that uses the two namespaces:

```racket
(begin/value
  (define x 'value-x))

(begin/type
  (define x 'type-x))

(begin/value
  (println x))

(begin/type
  (println x))
```

This program produces the following output:

```
'value-x
'type-x
```

It works! Normally, if you try to define two bindings with the same name in Racket, it will produce a compile-time error, but by assigning them different scopes, we have essentially managed to create two separate namespaces.

However, although this is close, it isn’t *quite* right. What happens if we nest the two inside each other?

```racket
(begin/value
  (begin/type
    (println x)))
```
```
x: identifier's binding is ambiguous
  context...:
   #(189267 module) #(189268 module anonymous-module 0) #(189464 use-site)
   #(189465 use-site) #(190351 use-site) #(190354 use-site) #(190358 local)
   #(190359 intdef)
  matching binding...:
   #<module-path-index:()>
   #(189267 module) #(189268 module anonymous-module 0) #(189464 use-site)
  matching binding...:
   #<module-path-index:()>
   #(189267 module) #(189268 module anonymous-module 0) #(189465 use-site)
```

Oh no! That didn’t work at all. The error is a bit of a scary one, but the top of the error message is essentially accurate: the use of `x` is *ambiguous* because it has both scopes on it, so it could refer to either binding. What we really want is for nested uses of `begin/value` or `begin/type` to *override* outer ones, ensuring that a use can only be in a single namespace at a time.

To do this, we simply need to adjust `begin/value` and `begin/type` to remove the other scope in addition to adding the appropriate one:

```racket
(define-simple-macro (begin/value form ...)
  #:with [form* ...] (map (λ (stx)
                            (type-introducer (value-introducer stx 'add) 'remove))
                          (attribute form))
  (begin form* ...))

(define-simple-macro (begin/type form ...)
  #:with [form* ...] (map (λ (stx)
                            (value-introducer (type-introducer stx 'add) 'remove))
                          (attribute form))
  (begin form* ...))
```

Now our nested program runs, and it produces `'type-x`, which is exactly what we want—the “nearest” scope wins.

With just a few lines of code, we’ve managed to implement the two-namespace system Hackett needs: we simply maintain two scopes, one for each namespace, and arrange for all the types to have the type scope applied and everything else to have the value scope applied. Easy, right? Well, not quite. Things start to get a lot more complicated once our programs span more than a single module.

## Namespaces that cross module boundaries

The system of using two syntax introducers to manage scopes is wonderfully simple as long as all of our programs are contained within a single module, but obviously, that is never true in practice. It is critical that users are able to export both values and types from one module and import them into another, as that is a pretty fundamental feature of any language. This is, unfortunately, where we start to run into problems.

Racket’s notion of hygiene is pervasive, but it is still essentially scoped to a single module. This makes sense, since each module conceptually has its own “module scope”, and it wouldn’t be very helpful to inject a binding from a different module with the *other* module’s scope—it would be impossible to reference the binding in the importing module. Instead, Racket’s modules essentially export *symbols*, not identifiers (which, in Racket terminology, are symbols packaged together with their lexical scope). When a Racket module provides a binding named `foo`, there is no other information attached to that binding. It does not have any scopes attached to it, since it is the `require` form’s job to attach the correct scopes to imported identifiers.

This completely makes sense for all normal uses of the Racket binding system, but it has unfortunate implications for our namespace system: Racket modules cannot export more than one binding with a given symbolic name\![^3] This won’t work at all, since a Hackett programmer might very well want to export a type and value with the same name from a single module. Indeed, this capability is one of the primary *points* of having multiple namespaces.

What to do? Sadly, Racket does not have nearly as elegant a solution for this problem, at least not at the time of this writing. Fortunately, hope is not lost. While far from perfect, we can get away with a relatively simple name-mangling scheme to prefix types upon export and unprefix them upon import. Since Racket’s `require` and `provide` forms are extensible, it’s even possible to implement this mangling in a completely invisible way.

Currently, the scheme that Hackett uses is to prefix `#%hackett-type:` onto the beginning of any type exports. This can be defined in terms of a [*provide pre-transformer*][provide-pre-transformer], which is essentially a macro that cooperates with Racket’s `provide` form to control the export process. In this case, we can define our `type-out` provide pre-transformer in terms of [`prefix-out`][prefix-out], a form built-in to Racket that allows prefixing the names of exports:

```racket
(define-syntax type-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ provide-spec ...)
        (pre-expand-export
         #`(prefix-out #%hackett-type:
                       #,(type-introducer
                          #'(combine-out provide-spec ...)))
         modes)]))))
```

Note that we call `type-introducer` in this macro! That’s because we want to ensure that, when a user writes `(provide (type-out Foo))`, we look for `Foo` in the module’s type namespace. Of course, once it is provided, all that scoping information is thrown away, but we still need it around so that `provide` knows *which* `Foo` is being provided.

Once we have referenced the correct binding, the use of `prefix-out` will appropriately add the `#%hackett-type:` prefix, so the exporting side is already done. Users do need to explicitly write `(type-out ....)` if they are exporting a particular type-level binding, but this is rarely necessary, since most users use `data` or `class` to export datatypes or typeclasses respectively, which can be modified to use `type-out` internally. Very little user code actually needs to change to support this adjustment.

Handling imports is, comparatively, tricky. When exporting, we can just force the user to annotate which exports are types, but we don’t have that luxury when importing, since it is merely whether or not a binding has the `#%hackett-type:` prefix that indicates which namespace it should be imported into. This means we’ll need to explicitly iterate through every imported binding and check if it has the prefix or not. If it does, we need to strip it off and add the type namespace; otherwise, we just pass it through unchanged.

Just as we extended `provide` with a provide pre-transformer, we can extend `require` using a [*require transformer*][require-transformer]. In code, this entire process looks like this:

```racket
(begin-for-syntax
  (define (unmangle-type-name name)
    (and~> (regexp-match #rx"^#%hackett-type:(.+)$" name) second)))

(define-syntax unmangle-types-in
  (make-require-transformer
   (syntax-parser
     [(_ require-spec ...)
      #:do [(define-values [imports sources]
              (expand-import #'(combine-in require-spec ...)))]
      (values
       (map (match-lambda
              [(and i (import local-id src-sym src-mod-path mode req-mode orig-mode orig-stx))
               (let* ([local-name (symbol->string (syntax-e local-id))]
                      [unmangled-type-name (unmangle-type-name local-name)])
                 (if unmangled-type-name
                     (let* ([unmangled-id
                             (datum->syntax local-id
                                            (string->symbol unmangled-type-name)
                                            local-id
                                            local-id)])
                       (import (type-introducer unmangled-id)
                               src-sym src-mod-path mode req-mode orig-mode orig-stx))
                     i))])
            imports)
       sources)])))
```

This is a little intimidating if you are not familiar with the intricacies of Racket’s low-level macro system, but the bulk of the code isn’t as scary as it may seem. It essentially does three things:

  1. It iterates over each import and calls `unmangle-type-name` on the imported symbol. If the result is `#f`, that means the import does not have the `#%hackett-type:` prefix, and it can be safely passed through unchanged.

  2. If `unmangle-type-name` does *not* return `#f`, then it returns the unprefixed name, which is then provided to `datum->syntax`, which allows users to forge new identifiers in an *unhygienic* (or “hygiene-bending”) way. In this case, we want to forge a new identifier with the name we get back from `unmangle-type-name`, but with the lexical context of the original identifier.

  3. Finally, we pass the new identifier to `type-introducer` to properly add the type scope, injecting the fresh binding into the type namespace.

With this in place, we now have a way for Hackett users to import and export type bindings, but while it is not much of a burden to write `type-out` when exporting types, it is unlikely that users will want to write `unmangle-types-in` around each and every import in their program. For that reason, we can define a slightly modified version of `require` that implicitly wraps all of its subforms with `unmangle-types-in`:

```racket
(provide (rename-out [require/unmangle require]))

(define-simple-macro (require/unmangle require-spec ...)
  (require (unmangle-types-in require-spec) ...))
```

…and we’re done. Now, Hackett modules can properly import and export type-level bindings.

## Namespaces plus submodules: the devil’s in the details

Up until this point, adding namespaces has required some understanding of the nuances of Racket’s macro system, but it hasn’t been particularly difficult to implement. However, getting namespaces right is a bit trickier than it appears. One area where namespaces are less than straightforward is Racket’s system of *submodules*.

Submodules are a Racket feature that allows the programmer to arbitrarily nest modules. Each file always corresponds to a single outer module, but that module can contain an arbitrary number of submodules. Each submodule can have its own “module language”, which even allows different languages to be mixed within a single file.

Submodules in Racket come in two flavors: `module` and `module*`. The difference is what order, semantically, they are defined in. Submodules defined with `module` are essentially defined *before* their enclosing module, so they cannot import their enclosing module, but their enclosing module can import them. Modules defined with `module*` are the logical dual to this: they are defined after their enclosing module, so they can import their enclosing module, but the enclosing module cannot import them.

How do submodules interact with namespaces? Well, for the most part, they work totally fine. This is because submodules are really, for the most part, treated like any other module, so the same machinery that works for ordinary Racket modules works fine with submodules.

However, there is [a special sort of `module*` submodule that uses `#f` in place of a module language][guide-submodules], which gives a module access to *all* of its enclosing module’s bindings, even ones that aren’t exported! This is commonly used to create a `test` submodule that contains unit tests, and functions can be tested in such a submodule even if they are not part of the enclosing module’s public API:

```racket
#lang racket

; not provided
(define (private-add1 x)
  (+ x 1))

(module* test #f
  (require rackunit)
  (check-equal? (private-add1 41) 42))
```

It would be nice to be able to use these sorts of submodules in Hackett, too, but if we try, we’ll find that types from the enclosing module mysteriously can’t be referenced by the submodule. Why? Well, the issue is in how we naïvely create our type and value introducers:

```racket
(begin-for-syntax
  (define value-introducer (make-syntax-introducer))
  (define type-introducer (make-syntax-introducer)))
```

Remember that `make-syntax-introducer` is generative—each time it is called, it produces a function that operates on a fresh scope. This is a problem, since those functions will be re-evaluated on every module [instantiation][module-instantiation], as ensured by Racket’s [separate compilation guarantee][separate-compilation-guarantee]. This means that each module gets its *own* pair of scopes. This means the body of a `module*` submodule will have different scopes from its enclosing module, and the enclosing modules bindings will not be accessible.

Fortunately, there is a way to circumvent this. While we cannot directly preserve syntax introducers across module instantiations, we *can* preserve syntax objects by embedding them in the expanded program, and we can attach scopes to syntax objects. Using [`make-syntax-delta-introducer`][make-syntax-delta-introducer], we can create a syntax introducer the adds or removes the *difference* between scopes on two syntax objects. Pairing this with a little bit of clever indirection, we can arrange for `value-introducer` and `type-introducer` to always operate on the same scopes on each module instantiation:

```racket
(define-simple-macro (define-value/type-introducers
                       value-introducer:id type-introducer:id)
  #:with scopeless-id (datum->syntax #f 'introducer-id)
  #:with value-id ((make-syntax-introducer) #'scopeless-id)
  #:with type-id ((make-syntax-introducer) #'scopeless-id)
  (begin-for-syntax
    (define value-introducer
      (make-syntax-delta-introducer #'value-id #'scopeless-id))
    (define type-introducer
      (make-syntax-delta-introducer #'type-id #'scopeless-id))))

(define-value/type-introducers value-introducer type-introducer)
```

The way this trick works is subtle, but to understand it, it’s important to understand that when a module is compiled, its macro uses are only evaluated once. Subsequent imports of the same module will not re-expand the module. *However*, code inside `begin-for-syntax` blocks is still re-evaluated every time the module is instantiated! This means we are *not* circumventing that re-evaluation directly, we are merely arranging for each re-evaluation to always produce the same result.

We still use `make-syntax-introducer` to create our two scopes, but critically, we only call `make-syntax-introducer` inside the `define-value/type-introducers` macro, which is, again, only run once (when the module is expanded). The resulting compiled module embeds `value-id` and `type-id` as syntax objects in the fully-expanded program, so they never change on each module instantiation, and they already contain the appropriate scopes. We can use `make-syntax-delta-introducer` to convert the “inert” scopes into introducer functions that we can use to apply the scopes to other syntax objects as we see fit.

By guaranteeing each namespace’s scope is always the same, even for different modules, `module*` submodules now work properly, and they are able to refer to bindings inherited from their enclosing module as desired.

## The final stretch: making Scribble documentation namespace-aware

As discussed in [my previous blog post][hackett-documentation-post], Hackett has comprehensive documentation powered by Racket’s excellent documentation tool, Scribble. Fortunately for Hackett, Scribble is incredibly flexible, and it can absolutely cope with a language with multiple namespaces. Less fortunately, it is clear that Scribble’s built-in documentation forms were not at all designed with multiple namespaces in mind.

In general, documenting such a language is tricky, assuming one wishes all identifiers to be properly hyperlinked to their appropriate definition (which, of course, I do). However, documentation is far more ambiguous than code when attempting to determine which identifiers belong in which namespace. When actually writing Hackett code, forms can always syntactically deduce the appropriate namespace for their subforms and annotate them accordingly, but this is not true in documentation. Indeed, it’s entirely possible that a piece of documentation might include intentionally incorrect code, which cannot be expanded at all!

Haskell’s documentation tool, Haddock, does not appear to attempt to tackle this problem at all—when given an identifier that exists in both namespaces, it will generate a hyperlink to the type, not the value. I do not know if there is a way around this, but if there is, it isn’t documented. This works alright for Haddock because Haskell’s documentation generally contains fewer examples, and Haskell programmers do not expect all examples to be appropriately hyperlinked, so a best-effort approach is accepted. Racket programmers, however, are used to a very high standard of documentation, and incorrectly hyperlinked docs are unacceptable.

To work around this problem, Hackett’s documentation requires that users explicitly annotate which identifiers belong to the type namespace. Identifiers in the type namespace are prefixed with `t:` upon import, and they are bound to Scribble [*element transformers*][element-transformer] that indicate they should be typeset without the `t:` prefix. Fortunately, Scribble’s documentation forms *do* understand Racket’s model of lexical scope (mostly), so they can properly distinguish between two identifiers with the same name but different lexical context.

In practice, this means Hackett documentation must now include a proliferation of `t:` prefixes. For example, here is the code for a typeset REPL interaction:

```racket
@(hackett-examples
  (defn square : (t:-> t:Integer t:Integer)
    [[x] {x * x}])
  (square 5))
```

Note the use of `t:->` and `t:Integer` instead of `->` and `Integer`. When the documentation is rendered and the example is evaluated, the prefixes are stripped, resulting in properly-typeset Hackett code.

This also means Hackett’s documentation forms have been updated to understand multiple namespaces. Hackett now provides `deftype` and `deftycon` forms for documenting types and type constructors, respectively, which will use the additional lexical information attached to `t:`-prefixed identifiers to properly index documented forms. Similarly, `defdata` and `defclass` have been updated with an understanding of types.

The implementation details of these changes is less interesting than the ones made to the code itself, since it mostly just involved tweaking Racket’s implementation of `defform` slightly to cooperate with the prefixed identifiers. To summarize, Hackett defines a notion of “type binding transformers” that include information about both prefixed and unprefixed versions of types, and Hackett provides documentation forms that consume that information when typesetting. A require transformer converts imported bindings into `t:`-prefixed ones and attaches the necessary compile-time information to them. It isn’t especially elegant, but it works.

# Analysis and unsolved problems

When laid out from top to bottom in this blog post, the amount of code it takes to actually implement multiple namespaces in Racket is surprisingly small. In hindsight, it does not feel like two weeks worth of effort, but it would be disingenuous to suggest that any of this was obvious. I tried a variety of different implementation strategies and spent a great deal of time staring at opaque error messages and begging [Matthew Flatt][mflatt] for help before I got things working properly. Fortunately, with everything in place, the implementation seems reliable, predictable, and useful for Hackett’s users (or, as the case may be, users-to-be).

For the most part, all the machinery behind multiple namespaces is invisible to the average Hackett programmer, and it seems to “just work”. For completeness, however, I must mention one unfortunate exception: remember the work needed to unmangle type names? While it’s true that all imports into Hackett modules are automatically unmangled by the custom `require` form, types provided by a module’s *language* are not automatically unmangled. This is because Racket does not currently provide a hook to customize how bindings from a module language are introduced, unlike `require`’s require transformers.

To circumvent this restriction, `#lang hackett`’s reader includes a somewhat ad-hoc solution that actually inserts a `require` into users’ programs that unmangles and imports all the types provided by the module. This mostly works, but due to the way Racket’s imports work, it isn’t possible for Racket programmers to import different types with the same names as Hackett core types; the two bindings will conflict, and there is no way for users to hide these implicitly imported bindings. Whether or not this is actually a common problem remains to be seen. If it is rare, it might be sufficient to introduce an ad-hoc mechanism to hide certain type imports, but it might be better to extend Racket in some way to better support this use-case.

That issue aside, multi-namespace Hackett is now working smoothly. It’s worth nothing that I did not have to do *any* special work to help Racket’s tooling, such as DrRacket’s Check Syntax tool, understand the binding structure of Hackett programs. Since other tools, such as racket-mode for Emacs, use the same mechanisms under the hood, Racket programmers’ existing tools will be able to properly locate the distinct definition sites for types and values with the same name, another example of how Racket successfully [internalizes extra-linguistic mechanisms][manifesto-extra-linguistic].

As closing notes, even if the majority of this blog post was gibberish to you, do note that Hackett has come quite a long way in just the past two months, adding much more than just a separate type namespace. I might try and give a more comprehensive update at a later date, but here’s a quick summary of the meaningful changes for those interested:

  - **Multi-parameter typeclasses** are implemented, along with **default typeclass method implementations**.

  - Pattern-matching performs basic **exhaustiveness checking**, so unmatched cases are a compile-time error.

  - Hackett ships with a **larger standard library**, including an `Either` type and appropriate functions, an `Identity` type, a `MonadTrans` typeclass, and the `ReaderT` and `ErrorT` monad transformers.

  - **More things are documented**, and parts of the documentation are slightly improved. Additionally, **Hackett’s internals are much more heavily commented**, hopefully making the project more accessible to new contributors.

  - **Parts of the typechecker are dramatically simplified**, improving the mechanisms behind dictionary elaboration and clearing the way for a variety of additional long-term improvements, including multiple compilation targets and a type-aware optimizer.

  - As always, various bug fixes.

Finally, special mention to two new contributors to Hackett, [Milo Turner][iitalics] and [Brendan Murphy][Shamrock-Frost]. Also special thanks to [Matthew Flatt][mflatt] and [Michael Ballantyne][michaelballantyne] for helping me overcome two of the trickiest macro-related problems I’ve encountered in Hackett to date. It has now been just over a year since Hackett’s original conception and roughly six months since the first commit of its current implementation, and the speed at which I’ve been able to work would not have been possible without the valuable help of the wonderful Racket community. Here’s hoping this is only the beginning.


[^1]: “But what about dependent types?” you may ask. Put simply, Hackett is not dependently typed, and it is not going to be dependently typed. Dependent types are currently being bolted onto Haskell, but Haskell does not have `#lang`. Racket does. It seems likely that a dependently-typed language would be much more useful as a separate `#lang`, not a modified version of Hackett, so Hackett can optimize its user experience for what it *is*, not what it might be someday.

[^2]: Hackett does not actually have a real kind system yet, but pleasantly, this same change will allow `*` to be used to mean “type” at the kind level and “multiply” at the value level.

[^3]: This isn’t strictly true, as readers familiar with Racket’s macro system may likely be aware that Racket modules export bindings at different “phase levels”, where phase levels above 0 correspond to compile-time macroexpansion phases. Racket modules are allowed to export a single binding per name, *per phase*, so the same symbolic name can be bound to different things at different phases. This isn’t meaningfully relevant for Hackett, however, since types and values are both exported at phase 0, and there are reasons that must be the case, this phase separation does not make this problem any simpler.


[element-transformer]: https://docs.racket-lang.org/scribble/scheme.html#%28tech._element._transformer%29
[guide-submodules]: https://docs.racket-lang.org/guide/Module_Syntax.html#%28part._submodules%29
[hackett]: https://github.com/lexi-lambda/hackett
[hackett-documentation-post]: /blog/2017/08/28/hackett-progress-report-documentation-quality-of-life-and-snake/
[make-syntax-delta-introducer]: https://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._make-syntax-delta-introducer%29%29
[make-syntax-introducer]: https://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._make-syntax-introducer%29%29
[manifesto-extra-linguistic]: http://www.ccs.neu.edu/home/matthias/manifesto/sec_intern.html
[mflatt]: http://www.cs.utah.edu/~mflatt/
[module-instantiation]: https://docs.racket-lang.org/reference/eval-model.html#%28tech._instantiate%29
[prefix-out]: https://docs.racket-lang.org/reference/require.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._prefix-out%29%29
[provide-pre-transformer]: https://docs.racket-lang.org/reference/stxtrans.html#%28tech._provide._pre._transformer%29
[require-transformer]: https://docs.racket-lang.org/reference/stxtrans.html#%28tech._require._transformer%29
[separate-compilation-guarantee]: https://docs.racket-lang.org/reference/eval-model.html#%28part._separate-compilation%29
[sets-of-scopes]: https://www.cs.utah.edu/plt/scope-sets/
[iitalics]: https://github.com/iitalics
[Shamrock-Frost]: https://github.com/Shamrock-Frost
[michaelballantyne]: https://github.com/michaelballantyne

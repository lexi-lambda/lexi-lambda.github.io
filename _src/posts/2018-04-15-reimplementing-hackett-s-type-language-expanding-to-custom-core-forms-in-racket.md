    Title: Reimplementing Hackett’s type language: expanding to custom core forms in Racket
    Date: 2018-04-15T00:00:00
    Tags: racket, hackett, macros

In the past couple of weeks, I [completely rewrote the implementation of Hackett’s type language][hackett-type-language-commit] to improve the integration between the type representation and Racket’s macro system. The new type language effectively implements a way to reuse as much of the Racket macroexpanding infrastructure as possible while expanding a completely custom language, which uses a custom set of core forms. The fundamental technique used to do so is not novel, and it seems to be periodically rediscovered every so often, but it has never been published or documented anywhere, and getting it right involves understanding a great number of subtleties about the Racket macro system. While I cannot entirely eliminate the need to understand those subtleties, in this blog post, I hope to make the secret sauce considerably less secret.

<!-- more -->

This blog post is both a case study on how I implemented the expander for Hackett’s new type language and a discussion of how such a technique can apply more generally. Like [my previous blog post on Hackett][hackett-namespaces-blog-post], which covered the implementation of its namespace system, the implementation section of this blog post is highly technical and probably requires significant experience with Racket’s macro system to completely comprehend. However, the surrounding material is written to be more accessible, so even if you are not a Racket programmer, you should hopefully be able to understand the big ideas behind this change.

# What are core forms?

Before we can get started writing *custom core forms*, we need to understand the meaning of Racket’s plain old *core forms*. What is a core form? In order to answer that question, we need to think about how Racket’s expansion and compilation model works.

To start, let’s consider a simple Racket program. Racket programs are organized into modules, which are usually written with a `#lang` line at the top. In this case, we’ll use `#lang racket` to keep things simple:

```racket
#lang racket

(define (add2 x)
  (+ x 2))

(add2 3)
```

How does Racket see this program? Well, before it can do anything with it, it must parse the program text, which is known in Racket as *reading* the program. The `#lang` line controls how the program is read—some `#lang`s provide parsers that allow syntax that is very different from the parser used for `#lang racket`—but no matter which reader is used, the result is an s-expression (actually a syntax object, but essentially an s-expression) representing a module. In the case of the above program, the result looks like this:

```racket
(module m racket
  (#%module-begin
    (define (add2 x)
      (+ x 2))

    (add2 3)))
```

Note the introduction of `#%module-begin`. Despite the fancy name, this is really just an ordinary macro provided by the `racket` language. By convention, the reader and expander cooperate to ensure the body of every module is wrapped with `#%module-begin`; as we’ll see shortly, this allows languages to add functionality that affects the entire contents of the module.

One the program has been read, it is subsequently *expanded* by the macroexpander. As the name implies, this is the phase that expands all the macros in a module. What does the above module look like after expansion? Well, it doesn’t look unrecognizable, but it certainly does look different:

```racket
(module m racket
  (#%plain-module-begin
    (define-values (add2)
      (lambda (x) (#%plain-app + x '2)))

    (#%plain-app call-with-values
                 (lambda () (#%plain-app add2 '3))
                 print-values)))
```

Let’s note the things that changed:

  1. `#%module-begin` was replaced with `#%plain-module-begin`. `#%plain-module-begin` is a binding that wraps the body of every expanded module, and all definitions of `#%module-begin` in any language must eventually expand to `#%plain-module-begin`. However, `#lang racket`’s `#%module-begin` doesn’t *just* expand to `#%plain-module-begin`, it also wraps bare expressions at the top level of a module so that their results are printed. This is why running the above program prints `5` even though there is no code related to printing in the original program!

  2. The lambda shorthand used with `define` was converted to an explicit use of `lambda`, and it was expanded to `define-values`. In Racket, `define` and `define-syntax` are really just macros for `define-values` and `define-syntaxes` that only bind a single identifier.

  3. All function applications were tagged explicitly with `#%plain-app`. This syntactically distinguishes function applications from uses of forms like `define-values` or `lambda`. It also allows languages to customize function application by providing their own macros named `#%app` (just like languages can provide their own macros named `#%module-begin` that expand to `#%plain-module-begin`), but that is outside the scope of this blog post.

  4. All literals have been wrapped with `quote`, so `2` became `'2` and `3` became `'3`.

Importantly, the resulting program contains **no macros**. Such programs are called *fully expanded*, since all macros have been eliminated and no further expansion can take place.

So what’s left behind? Well, some of the things in the program are literal data, like the numbers `2` and `3`. There are also some variable references, `x` and `add2`. Most of the program, however, is built out of primitives like `module`, `#%plain-module-begin`, `#%plain-app`, `define-values`, and `lambda`. These primitives are *core forms*—they are not variables, since they do not represent bindings that contain values at runtime, but they are also not macros, since they cannot be expanded any further.

In this sense, a fully-expanded program is just like a program in most languages that do not have macros. Core forms in Racket correspond to the syntax of other languages. We can imagine a JavaScript program similar to the above fully-expanded Racket program:

```python
var add2 =
  function (x) { return x + 2; };

console.log(add2(3));
```

Just as this JavaScript program is internally transformed into an AST containing a definition node, a function abstraction node, and some function application nodes, a fully-expanded Racket program represents an AST ready to be sent off to be *compiled*. The Racket compiler has built-in rules for how to compile core forms like `define-values`, `lambda`, and `#%plain-app`, and the result is optimized Racket bytecode.

In the remainder of this blog post, as most discussions of macros do, we’ll ignore the *read* and *compile* steps of the Racket program pipeline and focus exclusively on the *expand* step. It’s useful, however, to keep the other steps in mind, since we’re going to be discussing what it means to implement custom core forms, and core forms really only make sense in the context of the subsequent compilation step that consumes them.

## Racket’s default core forms

So, now that we know what core forms are in an abstract sense, what are they in practice? We’ve already encountered `module`, `#%plain-module-begin`, `#%plain-app`, `define-values`, `lambda`, and `quote`, but there are many more. The full list is available in the section of the Racket reference named [Fully Expanded Programs][reference-fully-expanded-programs], and I will not list all of them here. In general, they are more or less what you’d expect. The list of Racket’s core forms also includes things like `define-syntaxes`, `if`, `let-values`, `letrec-values`, `begin`, `quote-syntax`, and `set!`. Fundamentally, these correspond to the basic operations the Racket compiler understands, and it allows the remainder of Racket’s compilation pipeline to ignore the complexities of macroexpansion.

These forms are fairly versatile, and it’s easy to build high-level abstractions on top of them. For example, `#lang racket` implements `cond` as a macro that eventually expands into `if`, and it implements `syntax` as a macro that eventually expands into function calls and `quote-syntax`. The real power comes in the way new macros can be built out of other macros, not just core forms, so Racket’s `match` can expand into uses of `let` and `cond`, and it doesn’t need to concern itself with using `let-values` and `if`. For this reason, Racket’s core forms are quite capable of representing any language imaginable, since fully-expanded programs are essentially instructions for the Racket virtual machine, and macros are mini-compilers that can be mixed and matched.

## The need for custom core forms

With that in mind, why might we wish to define *custom* core forms? In fact, what would such a thing even mean? By their very nature, *all* Racket programs eventually expand into Racket’s core forms; new core forms cannot be added because Racket’s underlying compiler infrastructure is not (currently) extensible. New forms can be added that are defined in terms of other forms, but adding new primitives doesn’t make any sense, since the compiler would not know what to do with them.

Despite this, there *are* at least two use-cases in which a programmer might wish to customize the set of core forms produced by the macroexpander. Each situation is slightly different, but they both revolve around the same idea.

### Supporting multiple backends

The most commonly discussed use case for customizing the set of core forms is for languages that wish to use the Racket macroexpander, but target backends that are not the Racket compiler. For example, a user might implement a Racket `#lang` that describes electronic circuits, and they might even implement a way to execute such a program in Racket, but they might *also* wish to compile the result to a more traditional hardware description language. Like other languages in the Racket ecosystem, such a language would be made up of a tower of macros built on top of core forms; unlike other languages, the core forms might need to be more abstract than the ones provided by Racket to efficiently compile to other targets.

In the case of a hardware description language, the custom core forms might include things like `input` and `output` for declaring circuit inputs and outputs, and expressions might be built out of hardware operations rather than high-level things like function calls. The Racket macroexpander would expand the input program into the custom set of core forms, at which point an external compiler program could compile the resutling AST in a more traditional way. If the language author wished, they could *additionally* define implementations of these core forms as Racket macros that eventually expand into Racket, which would allow them to emulate their circuits in Racket at little cost, but this would be a wholly optional step.

Essentially, this use case stems from a desire to reuse Racket’s advanced language-development technology, such as the macroexpander, the module system, and editor tooling, without also committing to using Racket as a runtime, which is not always appropriate for all languages. This use case is not nearly as easy as it ought to be, but it is a common request, and it is possible that future improvements to the Racket toolchain will be designed specifically to address this problem.

### Compiling an extensible embedded language

A second use case for custom core forms is less frequently discussed, but I think it might actually be significantly more common in practice were it available in a form accessible to working macro programmers. In this scenario, users might wish to remain within Racket, but still want to define a custom language that other macros can consume.

This concept is a little more vague and fuzzily-defined than the case of developing a separate backend, so allow me to propose an example. Imagine a Racket programmer decides to build an embedded DSL for asynchronously producing and consuming events, similar to first-order functional reactive programming. In this case, the DSL is designed to be used in larger Racket programs, so it *will* eventually expand to Racket’s core forms. However, it’s possible that such a language might wish to enforce static invariants about the network graph, and in doing so, it might be able to produce significantly more optimal Racket code via a compile-time analysis.

Performing such a compile-time analysis is essentially writing a custom optimizer as part of a macro, which has been done numerous times already within the Racket ecosystem. One of the most prominent examples of such a thing is the `match` macro, which parses users’ patterns into compile-time data structures, performs a fairly traditional optimization pass designed to efficiently compile pattern matching, and it emits optimized Racket code as a result. This approach works well for fairly contained problems like pattern-matching, but it works less well for entirely new embedded languages that include everything from their own notion of evaluation to their own binding forms.

Existing DSLs of this type are rare, but they do exist. `syntax/parse` provides an expressive, specialized pattern-matching language designed specifically for matching syntax objects, and it uses a different model from `racket/match` to be more suitable for that task. It allows backtracking with cuts, an extensible pattern language, an abstraction language for defining reusable parsers that can accept inputs and produce outputs, and fine-grained control over both parsing and binding. While `match` is essentially just a traditional pattern-matcher, albeit an extensible one, `syntax-parse` is its own programming language, closer in some ways to Prolog than to Racket.

For this reason, `syntax/parse` has an extensive language to do everything from creating new bindings to controlling when and how parsing fails. This language is represented in two ways: an inline pattern language, and an alternate syntax known as [*pattern directives*][syntax-parse-pattern-directives]. Here is an example of pattern directives in action, from my own `threading` library:

```racket
[(_ ex:expr cl:clause remaining:clause ...)
 #:do [(define call (syntax->list #'cl.call))
       (define-values (pre post)
         (split-at call (add1 (or (attribute cl.insertion-point) 0))))]
 #:with [pre ...] pre
 #:with [post ...] post
 #:with app/ctx (adjust-outer-context this-syntax #'(pre ... ex post ...) #'cl)
 (adjust-outer-context this-syntax #'(~> app/ctx remaining ...) this-syntax)]
```

Each directive is represented by a keyword, in this case `#:do` and `#:with`. Each directive has a corresponding keyword in the pattern language, in this case `~do` and `~parse`. Therefore, the above pattern could equivalently be written this way:

```racket
[{~and (_ ex:expr cl:clause remaining:clause ...)
       {~do (define call (syntax->list #'cl.call))
            (define-values (pre post)
              (split-at call (add1 (or (attribute cl.insertion-point) 0))))}
       {~parse [pre ...] pre}
       {~parse [post ...] post}
       {~parse app/ctx (adjust-outer-context this-syntax #'(pre ... ex post ...) #'cl)}}
 (adjust-outer-context this-syntax #'(~> app/ctx remaining ...) this-syntax)]
```

The transformation can go in the other direction, too—each syntax class annotation on each pattern variable can be extracted into the directive language using `#:declare`, so this is also equivalent:

```racket
[(_ ex cl remaining ...)
 #:declare ex expr
 #:declare cl clause
 #:declare remaining clause
 #:do [(define call (syntax->list #'cl.call))
       (define-values (pre post)
         (split-at call (add1 (or (attribute cl.insertion-point) 0))))]
 #:with [pre ...] pre
 #:with [post ...] post
 #:with app/ctx (adjust-outer-context this-syntax #'(pre ... ex post ...) #'cl)
 (adjust-outer-context this-syntax #'(~> app/ctx remaining ...) this-syntax)]
```

This is very much a programming language, but it has very different semantics from programming in Racket! Failure to match against a `#:with` or `~parse` pattern causes pattern-matching to backtrack, and though it’s possible to escape to Racket using `#:do` or `~do`, practical uses of `syntax/parse` really do involve quite a lot of programming in its pattern DSL.

But the Racket programmer might not find this DSL wholly satisfying. Why? Well, it isn’t extensible! The pattern directives—`#:declare`, `#:do`, and `#:with`, among others—are essentially the core forms of `syntax/parse`’s pattern-matching language, but new ones cannot be defined. The desire to make this language easy to analyze statically in order to emit optimal pattern-matching code meant its author opted to define the language in terms of a specific grammar rather than a tower of macros.

But what if `syntax/parse` could define its own core forms? What if, instead of `#:do`, `#:declare`, and `#:with` being implemented as keyword options specially recognized by the `syntax-parse` grammar, it defined `do`, `declare`, and `with` as core forms for a new, macro-enabled language? A user of the language could then define a completely ordinary Racket macro and use it with this new language as long as it eventually expanded into the `syntax/parse` core forms. The implementation of `syntax/parse` could then invoke the macroexpander to request each clause be expanded into its core forms, perform its static analysis on the result, and finally emit optimized Racket code.

Now, to be fair, `syntax/parse` is not actually entirely inextensible. While new directives cannot be defined, new patterns can be added through a pattern-expander API that was added to the library after its initial design. However, pattern expanders are still not ideal because they are not ordinary Racket macros—users must explicitly define each pattern expander differently from how they would a macro—and they cannot use existing Racket forms, even ones that would theoretically be compatible with an arbitrary set of core forms.

The technique described in this blog post avoids all those problems. In the following sections, I’ll show that it’s possible to define an embedded language with a custom set of core forms that works well with the rest of the Racket ecosystem and still permits arbitrary static analysis.

# The need for a custom type language in Hackett

In the previous section, I described two use cases for custom core forms. Hackett, in fact, has uses for *both* of them:

  - Hackett can definitely make use of custom core forms to compile to multiple backends. Eventually, it would be nice to compile Hackett to an intermediate language that can target both the Racket runtime and Haskell or GHC Core. This would allow Hackett to take advantage of GHC’s advanced optimizing compiler that already has decades of tuning for a pure, lazy, functional programming language, at the cost of not having access to the rest of Racket’s ecosystem of libraries at runtime.

  - Hackett can *also* make use of custom core forms for an embedded DSL. In this case, that embedded DSL is actually Hackett’s type language.

The second of those two use cases is simpler, and it’s what I ended up implementing first, so it’s what I will focus on in this blog post. Hackett’s type language is fundamentally quite simple, so its set of custom core forms is small as well. Everything in the type language eventually compiles into only seven core forms:

  - <code>(#%type:con <i>id</i>)</code> — Type constructors, like `Integer` or `Maybe`. These are one of the fundamental building blocks of Hackett types.

  - <code>(#%type:app <i>type</i> <i>type</i>)</code> — Type application, such as `(Maybe Integer)`. Types are curried, so type constructors that accept multiple arguments are represented by nested uses of `#%type:app`.

  - <code>(#%type:forall <i>id</i> <i>type</i>)</code> — Universal quantification. This is essentially a binding form, which binds any uses of <code>(#%type:bound-var <i>id</i>)</code> in <code><i>type</i></code>.

  - <code>(#%type:qual <i>type</i> <i>type</i>)</code> — Qualified types, aka types with typeclass constraints. Constraints in Hackett, like in GHC, are represented by types, so typeclass names like `Eq` are bound as type constructors.

  - Finally, Hackett types support three different varieties of type variables:

    - <code>(#%type:bound-var <i>id</i>)</code> — Bound type variables. These are only legal under a corresponding `#%type:forall`.

    - <code>(#%type:wobbly-var <i>id</i>)</code> — Solver variables, which may unify with any other type as part of the typechecking process.

    - <code>(#%type:rigid-var <i>id</i>)</code> — Rigid variables, aka skolem variables, which only unify with themselves. They represent a unique, anonymous type used to ensure types are suitably polymorphic.

To implement our custom core forms in Racket, we need to somehow define them, but how? Intentionally, these should never be expanded, since we want the expander to stop expanding whenever it encounters one of these identifiers. While we can’t encode this directly, we *can* bind them to macros that do nothing but raise an exception if something attempts to expand them:

```racket
(define-syntaxes [#%type:con #%type:app #%type:forall #%type:qual
                  #%type:bound-var #%type:wobbly-var #%type:rigid-var]
  (let ([type-literal (λ (stx) (raise-syntax-error #f "cannot be used as an expression" stx))])
    (values type-literal type-literal type-literal type-literal
            type-literal type-literal type-literal)))
```

This will ensure our core forms are never accidentally expanded, and we’ll instruct the macroexpander to stop whenever it sees one of them via a separate mechanism.

## Expanding types in our type language

We’ve now defined our core forms, but we’ve intentionally left them meaningless. How do we actually inform the expander about how our types ought to be expanded? While it’s true that we don’t want the core forms themselves to be eliminated, we *do* want to expand some of their subforms. For example, in the type `(#%type:app a b)`, we want to recursively expand `a` and `b`.

In order to do this, we’ll use the API made available by the expander for manually invoking macroexpansion from within another macro. This API is called [`local-expand`][local-expand], and it has an option relevant to our needs: the stop list.

Often, `local-expand` is used to force the expander to completely, recursively expand a form. For example, by using `local-expand`, we can produce a fragment of a fully-expanded program from a piece of syntax that still includes macros:

```racket
(local-expand #'(let ([x 1]) (+ x 2)) 'expression '())
; => (let-values ([(x) '1]) (#%plain-app + x '2))
```

The third argument to `local-expand` is the *stop list*, which controls how deep the expander ought to expand a given form. By providing an empty list, we ask for a complete, recursive expansion. In this case, however, we don’t want a complete expansion! We can inform the expander to stop whenever it sees any of our custom core forms by passing a list of our core form identifiers instead of an empty list:

```racket
(begin-for-syntax
  (define type-literal-ids
    (list #'#%type:con #'#%type:app #'#%type:forall #'#%type:qual
          #'#%type:bound-var #'#%type:wobbly-var #'#%type:rigid-var))

  (local-expand #'(#%type:forall x t) 'expression type-literal-ids))
  ; => (#%type:forall x t)
```

Of course, this isn’t very interesting, since it just gives us back exactly what we gave it. It spotted the `#%type:forall` identifier, which is in our stop list, and immediately halted expansion. It didn’t attempt to continue expanding `t` since the expander has no way of knowing which pieces of `(#%type:forall x t)` it should expand! In this case, we want it to recur to expand `t`, since it should be a type, but not `x`, since `#%type:forall` essentially puts `x` in binding position.

Therefore, we have to get more clever. We need to call `local-expand` to produce a type, then we have to pattern-match on it and subsequently call `local-expand` *again* on any of the pieces of syntax we want to keep expanding. Eventually, we’ll run out of things to expand, and our type will be fully-expanded.

One good way to do this is to use `syntax/parse` syntax classes, since they provide a convenient way for other macros to invoke the type expander. To implement our type expander, we’ll use two mutually recursive syntax classes: one to perform the actual expansion using `local-expand` and a second to pattern-match on the resulting expanded type. For example, here’s what these two classes would look like if they only handled `#%type:con` and `#%type:app`:

```racket
(begin-for-syntax
  (define-literal-set type-literals
    [#%type:con #%type:app #%type:forall #%type:qual
     #%type:bound-var #%type:wobbly-var #%type:rigid-var])

  (define-syntax-class type
    #:description "type"
    #:attributes [expansion]
    [pattern _ #:with :expanded-type
                      (local-expand this-syntax 'expression type-literal-ids)])

  (define-syntax-class expanded-type
    #:description #f
    #:attributes [expansion]
    #:commit
    #:literal-sets [type-literals]
    [pattern (#%type:con ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:app ~! a:type b:type)
             #:attr expansion #'(#%type:app a.expansion b.expansion)]))
```

This blog post is definitely *not* a `syntax/parse` tutorial, so I will not explain in detail everything that’s going on here, but the gist of it is that the above code defines two syntax classes, both of which produce a single output attribute named `expansion`. This attribute contains the fully expanded version of the type currently being parsed. In the `#%type:con` case, `expansion` is just `this-syntax`, which holds the current piece of syntax being parsed. This makes sense, since uses of `#%type:con` just expand to themselves—expanding `(#%type:con Maybe)` should not perform any additional expansion on `Maybe`. This is one of Hackett’s atomic types.

In contrast, `#%type:app` *does* recursively expand its arguments. By annotating its two subforms with `:type`, the `type` syntax class will invoke `local-expand` on each subform, which will in turn use `expanded-type` to parse the resulting type. This is what implements the expansion loop that will eventually expand each type completely. Once `a` and `b` have been expanded, `#%type:app` reassembles them into a new syntax object using `#'(#%type:app a.expansion b.expansion)`, which replaces their unexpanded versions with their new, expanded versions.

We can see this behavior by writing a small `expand-type` function that will expand its argument:

```racket
(begin-for-syntax
  (define expand-type (syntax-parser [t:type #'t.expansion])))
```

Now we can use it to observe what happens when we try expanding a type using `#%type:app`:

```racket
(expand-type #'(#%type:app Maybe Integer))
; => #%type:app: expected type
;      at: Maybe
;      in: (#%type:app Maybe Integer)
```

Okay, it failed with an error, which is not ideal, but it makes sense. We haven’t actually defined `Maybe` or `Integer` anywhere. Let’s do so! We can define them as simple macros that expand into uses of `#%type:con`, which can be done easily using [`make-variable-like-transformer`][make-variable-like-transformer] from `syntax/transformer`:

```racket
(define-syntax Maybe (make-variable-like-transformer #'(#%type:con Maybe)))
(define-syntax Integer (make-variable-like-transformer #'(#%type:con Integer)))
```

Now, if we try expanding that same type again:

```racket
(expand-type #'(#%type:app Maybe Integer))
; => (#%type:app (#%type:con Maybe) (#%type:con Integer))
```

…it works! Neat. Now we just need to add the cases for the remaining forms in our type language:

```racket
(begin-for-syntax
  (define-syntax-class expanded-type
    #:description #f
    #:attributes [expansion]
    #:commit
    #:literal-sets [type-literals]
    [pattern (#%type:con ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:app ~! a:type b:type)
             #:attr expansion #'(#%type:app a.expansion b.expansion)]
    [pattern (#%type:forall ~! x:id t:type)
             #:attr expansion #'(#%type:forall x t.expansion)]
    [pattern (#%type:qual ~! a:type b:type)
             #:attr expansion #'(#%type:qual a.expansion b.expansion)]
    [pattern (#%type:bound-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:wobbly-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:rigid-var ~! _:id)
             #:attr expansion this-syntax]))
```

This is pretty good already, and to a first approximation, it’s done! However, it doesn’t actually work as well as we’d really like it to. One of the whole points of doing things this way is to allow other macros like `let-syntax` to work in types. For example, we ought to be able to create a local type binding with `let-syntax` and have it just work. Unfortunately, it doesn’t:

```racket
(expand-type #'(let-syntax ([Bool (make-variable-like-transformer #'(#%type:con Bool))])
                 (#%type:app Maybe Bool)))
; => let-syntax: expected one of these identifiers: `#%type:con', `#%type:app', `#%type:forall', `#%type:qual', `#%type:bound-var', `#%type:wobbly-var', or `#%type:rigid-var'
;     at: letrec-syntaxes+values
;     in: (let-syntax ((Bool (make-variable-like-transformer (syntax Bool)))) (#%type:app Maybe Bool))
```

What went wrong? And why is it complaining about `letrec-syntaxes+values`? Well, if you read the documentation for `local-expand`, you’ll find that its behavior is a little more complicated than you might at first believe:

> If *`stop-ids`* is [a nonempty list containing more than just `module*`], then `begin`, `quote`, `set!`, `#%plain-lambda`, `case-lambda`, `let-values`, `letrec-values`, `if`, `begin0`, `with-continuation-mark`, `letrec-syntaxes+values`, `#%plain-app`, `#%expression`, `#%top`, and `#%variable-reference` are implicitly added to *`stop-ids`*. Expansion stops when the expander encounters any of the forms in *`stop-ids`*, and the result is the partially-expanded form.

That’s a little strange, isn’t it? I am not completely sure why the behavior works quite this way, though I’m sure backwards compatibility plays a significant part, but while some of the behavior seems unnecessary, the issue with `letrec-syntaxes+values` (which `let-syntax` expands to) is a reasonable one. If the expander naïvely expanded `letrec-syntaxes+values` in the presence of a nonempty stop list, it could cause some significant problems!

Allow me to illustrate with an example. Let’s imagine we are the expander, and we are instructed to expand the following program:

```racket
(let-syntax ([Bool (make-variable-like-transformer #'(#%type:con Bool))])
  (#%type:app Maybe Bool))
```

We see `let-syntax`, so we start by evaluating the expression on the right hand side of the `Bool` binding. This produces a transformer expression, so we bind `Bool` to the transformer in the local environment, then move onto expanding the body. At this point, the expander is looking at this:

```racket
; local bindings:
;   Bool -> #<variable-like-transformer>
(#%type:app Maybe Bool)
```

Now, the identifier in application position is `#%type:app`, and `#%type:app` is in the stop list. Therefore, expansion must stop, and it does not attempt to expand any further. But what should the result of expansion be? Well, the `let-syntax` needs to go away when we expand it—local syntax bindings are erased as part of macroexpansion—so the logical thing to expand into is `(#%type:app Maybe Bool)`. But this is a problem, because when we then go to expand `Bool`, `Bool` isn’t in the local binding table anymore! The `let-syntax` was already erased, and `Bool` is unbound!

When expanding recursively, this isn’t a problem, since the entire expression is guaranteed to be expanded while the local binding is still in the expander’s environment. As soon as we introduce partial expansion, however, we run the risk of a binding getting erased too early. So we’re stuck: we can’t recursively expand, or we’ll expand too much, but we can’t partially expand, since we might expand too little.

Confronted with this problem, there is some good news and some bad news. The good news is that, while the macroexpander can’t help us, we can help the macroexpander by doing some of the necessary bookkeeping for it. We can do this using first-class definition contexts, which allow us to manually extend the local environment when we call `local-expand`. The bad news is that first-class definition contexts are *complicated*, and using them properly is a surprisingly subtle problem.

Fortunately, I’ve already spent a lot of time figuring out what needs to be done to properly manipulate the necessary definition contexts in this particular situation. The first step is to parameterize our `type` and `expanded-type` syntax classes so that we may thread a definition context around as we recursively expand:

```racket
(begin-for-syntax
  (define-syntax-class (type [intdef-ctx #f])
    #:description "type"
    #:attributes [expansion]
    [pattern _ #:with {~var || (expanded-type intdef-ctx)}
                      (local-expand this-syntax 'expression type-literal-ids intdef-ctx)])

  (define-syntax-class (expanded-type intdef-ctx)
    #:description #f
    #:attributes [expansion]
    #:commit
    #:literal-sets [type-literals]
    [pattern (#%type:con ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:app ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
             #:attr expansion #'(#%type:app a.expansion b.expansion)]
    [pattern (#%type:forall ~! x:id {~var t (type intdef-ctx)})
             #:attr expansion #'(#%type:forall x t.expansion)]
    [pattern (#%type:qual ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
             #:attr expansion #'(#%type:qual a.expansion b.expansion)]
    [pattern (#%type:bound-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:wobbly-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:rigid-var ~! _:id)
             #:attr expansion this-syntax]))
```

Now, we can add an additional case to `expanded-type` to handle `letrec-syntaxes+values`, which will explicitly create a new definition context, add bindings to it, and use it when parsing the body:

```racket
[pattern (letrec-syntaxes+values ~! ([(id:id ...) e:expr] ...) () t:expr)
         #:do [(define intdef-ctx* (syntax-local-make-definition-context))
               (for ([ids (in-list (attribute id))]
                     [e (in-list (attribute e))])
                 (syntax-local-bind-syntaxes ids e intdef-ctx*))]
         #:with {~var t* (type intdef-ctx*)} #'t
         #:attr expansion #'t*.expansion]
```

But even this isn’t quite right. The problem with this implementation is that it throws away the existing `intdef-ctx` argument to `expanded-type`, which means those bindings will be lost as soon as we introduce a new set. To fix this, we have to do two things: we need to pass the previous definition context as an argument to `syntax-local-make-definition-context` so the bindings will be in scope when we call `syntax-local-bind-syntaxes`, and we will need to pass *both* contexts to the `type` syntax class, since `local-expand` can accept multiple definition contexts if they are provided in a list. Since the previous definition context might be `#f`, a single definition context, or a list of definition contexts, we’ll write some helper functions that handle all the potential cases:

```racket
(begin-for-syntax
  (define (internal-definition-context-extend old-ctx)
    (if (list? old-ctx)
        (syntax-local-make-definition-context (first old-ctx))
        (syntax-local-make-definition-context old-ctx)))

  (define (internal-definition-context-cons new-ctx old-ctx)
    (cond [(not old-ctx) new-ctx]
          [(list? old-ctx) (cons new-ctx old-ctx)]
          [else (list new-ctx old-ctx)])))
```

Now we can use these two functions to ensure we can properly preserve all previous definition contexts when we create a new one:

```racket
[pattern (letrec-syntaxes+values ~! ([(id:id ...) e:expr] ...) () t:expr)
         #:do [(define intdef-ctx* (internal-definition-context-extend intdef-ctx))
               (for ([ids (in-list (attribute id))]
                     [e (in-list (attribute e))])
                 (syntax-local-bind-syntaxes ids e intdef-ctx*))]
         #:with {~var t* (type (internal-definition-context-cons intdef-ctx* intdef-ctx))} #'t
         #:attr expansion #'t*.expansion]
```

With this in place, our example using `let-syntax` actually works!

```racket
(expand-type #'(let-syntax ([Bool (make-variable-like-transformer #'(#%type:con Bool))])
                 (#%type:app Maybe Bool)))
; => (#%type:app (#%type:con Maybe) (#%type:con Bool))
```

Pretty cool, isn’t it?

## Preserving syntax properties and source locations

We’ve now managed to essentially implement an expander for our custom language by periodically yielding to the Racket macroexpander, and for the most part, it works. However, our implementation isn’t perfect. The real Racket macroexpander takes great care to preserve source locations and syntax properties on syntax objects wherever possible, which our implementation does not do. Normally we don’t have to worry so much about such things, since the macroexpander automatically copies properties when expanding macros, but since we’re circumventing the expander, we don’t get that luxury. In order to properly preserve this information, we’ll have to be a little more careful.

To start, we really ought to copy the identifier in application position into the output wherever we can. In addition to preserving source location information and syntax properties, it also preserves the even more visible renamings. For example, if a user imports `#%type:app` under a different name, like `#%type:apply`, we should expand to a piece of syntax that still has `#%type:apply` in application position instead of replacing it with `#%type:app`.

To do this, we just need to bind each of the identifiers in application position, then use that binding when we produce output. For example, we would adjust the `#%type:app` clause to the following:

```racket
[pattern (head:#%type:app ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
         #:attr expansion #'(head a.expansion b.expansion)]
```

But even after doing this, some source locations and syntax properties are lost, since we’re still reconstructing the pair from scratch. To ensure we copy *everything*, we can define two helper macros, `syntax/loc/props` and `quasisyntax/loc/props`, which are like `syntax/loc` and `quasisyntax/loc` but copy properties in addition to source location information:

```racket
(begin-for-syntax
  (define-syntaxes [syntax/loc/props quasisyntax/loc/props]
    (let ()
      (define (make-syntax/loc/props name syntax-id)
        (syntax-parser
          [(_ from-stx-expr:expr {~describe "template" template})
           #`(let ([from-stx from-stx-expr])
               (unless (syntax? from-stx)
                 (raise-argument-error '#,name "syntax?" from-stx))
               (let* ([stx (#,syntax-id template)]
                      [stx* (syntax-disarm stx #f)])
                 (syntax-rearm (datum->syntax stx* (syntax-e stx*) from-stx from-stx) stx)))]))
      (values (make-syntax/loc/props 'syntax/loc/props #'syntax)
              (make-syntax/loc/props 'quasisyntax/loc/props #'quasisyntax)))))
```

Using `syntax/loc/props`, we can be truly thorough about ensuring all properties are preserved:

```racket
[pattern (head:#%type:app ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
         #:attr expansion (syntax/loc/props this-syntax
                            (head a.expansion b.expansion))]
```

Applying this to the other relevant clauses, we get an updated version of the `expanded-type` syntax class:

```racket
(begin-for-syntax
  (define-syntax-class (expanded-type intdef-ctx)
    #:description #f
    #:attributes [expansion]
    #:commit
    #:literal-sets [kernel-literals type-literals]
    [pattern (letrec-syntaxes+values ~! ([(id:id ...) e:expr] ...) () t:expr)
             #:do [(define intdef-ctx* (internal-definition-context-extend intdef-ctx))
                   (for ([ids (in-list (attribute id))]
                         [e (in-list (attribute e))])
                     (syntax-local-bind-syntaxes ids e intdef-ctx*))]
             #:with {~var t* (type (internal-definition-context-cons intdef-ctx* intdef-ctx))} #'t
             #:attr expansion #'t*.expansion]
    [pattern (#%type:con ~! _:id)
             #:attr expansion this-syntax]
    [pattern (head:#%type:app ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
             #:attr expansion (syntax/loc/props this-syntax
                                (head a.expansion b.expansion))]
    [pattern (head:#%type:forall ~! x:id {~var t (type intdef-ctx)})
             #:attr expansion (syntax/loc/props this-syntax
                                (#%type:forall x t.expansion))]
    [pattern (head:#%type:qual ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
             #:attr expansion (syntax/loc/props this-syntax
                                (head:#%type:qual a.expansion b.expansion))]
    [pattern (#%type:bound-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:wobbly-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:rigid-var ~! _:id)
             #:attr expansion this-syntax]))
```

Now we’re getting closer, but if you can believe it, even *this* isn’t good enough. The real expander’s implementation of `letrec-syntaxes+values` does two things our implementation does not: it copies properties and updates the `'origin` property to indicate the syntax came from a use of `letrec-syntaxes+values`, and it adds a `'disappeared-use` property to record the erased bindings for use by tools like DrRacket. We can apply `syntax-track-origin` and `internal-definition-context-track` to the resulting syntax to add the same properties the expander would:

```racket
[pattern (head:letrec-syntaxes+values ~! ([(id:id ...) e:expr] ...) () t:expr)
         #:do [(define intdef-ctx* (internal-definition-context-extend intdef-ctx))
               (for ([ids (in-list (attribute id))]
                     [e (in-list (attribute e))])
                 (syntax-local-bind-syntaxes ids e intdef-ctx*))]
         #:with {~var t* (type (internal-definition-context-cons intdef-ctx* intdef-ctx))} #'t
         #:attr expansion (~> (internal-definition-context-track intdef-ctx* #'t*.expansion)
                              (syntax-track-origin this-syntax #'head))]
```

Now we’ve *finally* dotted all our i’s and crossed our t’s. While it does take a lot to properly emulate what the macroexpander is doing, the important thing is that it’s actually possible! The end result of all this definition context juggling and property copying is that we’ve effectively managed to move some of the macroexpander’s logic into userspace code, which allows us to manipulate it as we see fit.

## Connecting our custom language to Hackett

It took a lot of work, but we finally managed to write a custom type language, and while the code is not exactly simple, it’s not actually very long. The entire implementation of our custom type language is less than 90 lines of code:

```racket
#lang racket/base

(require (for-meta 2 racket/base
                     syntax/parse)
         (for-syntax racket/base
                     racket/list
                     syntax/intdef
                     threading)
         syntax/parse/define)

(begin-for-syntax
  (define (internal-definition-context-extend old-ctx)
    (if (list? old-ctx)
        (syntax-local-make-definition-context (first old-ctx))
        (syntax-local-make-definition-context old-ctx)))

  (define (internal-definition-context-cons new-ctx old-ctx)
    (cond [(not old-ctx) new-ctx]
          [(list? old-ctx) (cons new-ctx old-ctx)]
          [else (list new-ctx old-ctx)]))

  (define-syntaxes [syntax/loc/props quasisyntax/loc/props]
    (let ()
      (define (make-syntax/loc/props name syntax-id)
        (syntax-parser
          [(_ from-stx-expr:expr {~describe "template" template})
           #`(let ([from-stx from-stx-expr])
               (unless (syntax? from-stx)
                 (raise-argument-error '#,name "syntax?" from-stx))
               (let* ([stx (#,syntax-id template)]
                      [stx* (syntax-disarm stx #f)])
                 (syntax-rearm (datum->syntax stx* (syntax-e stx*) from-stx from-stx) stx)))]))
      (values (make-syntax/loc/props 'syntax/loc/props #'syntax)
              (make-syntax/loc/props 'quasisyntax/loc/props #'quasisyntax)))))

(define-syntaxes [#%type:con #%type:app #%type:forall #%type:qual
                  #%type:bound-var #%type:wobbly-var #%type:rigid-var]
  (let ([type-literal (λ (stx) (raise-syntax-error #f "cannot be used as an expression" stx))])
    (values type-literal type-literal type-literal type-literal
            type-literal type-literal type-literal)))

(begin-for-syntax
  (define type-literal-ids
    (list #'#%type:con #'#%type:app #'#%type:forall #'#%type:qual
          #'#%type:bound-var #'#%type:wobbly-var #'#%type:rigid-var))

  (define-literal-set type-literals
    [#%type:con #%type:app #%type:forall #%type:qual
     #%type:bound-var #%type:wobbly-var #%type:rigid-var])

  (define-syntax-class (type [intdef-ctx #f])
    #:description "type"
    #:attributes [expansion]
    [pattern _ #:with {~var || (expanded-type intdef-ctx)}
                      (local-expand this-syntax 'expression type-literal-ids intdef-ctx)])

  (define-syntax-class (expanded-type intdef-ctx)
    #:description #f
    #:attributes [expansion]
    #:commit
    #:literal-sets [kernel-literals type-literals]
    [pattern (head:letrec-syntaxes+values ~! ([(id:id ...) e:expr] ...) () t:expr)
             #:do [(define intdef-ctx* (internal-definition-context-extend intdef-ctx))
                   (for ([ids (in-list (attribute id))]
                         [e (in-list (attribute e))])
                     (syntax-local-bind-syntaxes ids e intdef-ctx*))]
             #:with {~var t* (type (internal-definition-context-cons intdef-ctx* intdef-ctx))} #'t
             #:attr expansion (~> (internal-definition-context-track intdef-ctx* #'t*.expansion)
                                  (syntax-track-origin this-syntax #'head))]
    [pattern (#%type:con ~! _:id)
             #:attr expansion this-syntax]
    [pattern (head:#%type:app ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
             #:attr expansion (syntax/loc/props this-syntax
                                (head a.expansion b.expansion))]
    [pattern (head:#%type:forall ~! x:id {~var t (type intdef-ctx)})
             #:attr expansion (syntax/loc/props this-syntax
                                (#%type:forall x t.expansion))]
    [pattern (head:#%type:qual ~! {~var a (type intdef-ctx)} {~var b (type intdef-ctx)})
             #:attr expansion (syntax/loc/props this-syntax
                                (head:#%type:qual a.expansion b.expansion))]
    [pattern (#%type:bound-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:wobbly-var ~! _:id)
             #:attr expansion this-syntax]
    [pattern (#%type:rigid-var ~! _:id)
             #:attr expansion this-syntax])

  (define expand-type (syntax-parser [t:type #'t.expansion])))
```

But what now? Just as Racket fully-expanded programs are useless without a compiler to turn them into something useful, our custom type language doesn’t do anything at all in isolation. As it happens, in the case of the type language, we don’t have a compiler at all—we have a *typechecker*. The Hackett typechecker consumes fully-expanded types as input and uses them to perform its typechecking process. The actual implementation of Hackett’s typechecker is outside the scope of this blog post, since it’s really an entirely separate problem, but you can probably imagine what such a thing might look like, in an extremely vague, handwavy sense.

But we don’t *just* need a typechecker. Just as the authors of Racket don’t expect users to write programs using the core forms directly, we also don’t expect users to write their types using the fully-expanded syntax. If we did, all this fancy expansion machinery would be pretty pointless! Hackett provides a custom `#%app` binding that converts n-ary type applications to nested uses of `#%type:app`, as well as a nicer `forall` macro that supports specifying multiple type variables and multiple typeclass constraints all at once. The best part, though, is that these macros can be defined in a completely straightforward way, just as any ordinary Racket macro would be written, and the machinery will work precisely as intended. It’s also perfectly okay to have two different versions of `#%app`—one for types and one for values—since [Hackett supports multiple namespaces][hackett-namespaces-blog-post], and each can have its own `#%app` binding.

The real implementation of Hackett’s type language is a little bit longer than the one in this blog post because it includes some extra definitions to provide custom `syntax/parse` pattern expanders for matching types and some template metafunctions for producing them, which are used by the typechecker, but if you’d like to see the whole thing, [it’s available on GitHub here][hackett-type-language-source].

# Evaluation, limitations, and acknowledgements

Reimplementing Hackett’s type language took about a week and a half, about half of which was supplemented by the extra time I had before I started [my new job][tweet-new-job] this past week. A portion of that time was spent deciding what I actually wanted to do, and a lot of it was spent hunting down fiddly bugs. All told, the rewrite resulted in a net addition of 250 lines of code to the Hackett codebase. However, 350 of the added lines reside in a new, self-contained module dedicated to Hackett’s type language, so the change actually resulted in a net *removal* of 100 lines from the rest of the codebase, which I consider an organizational win.

As for whether or not the change will accomplish the goals I had in mind, I think signs currently point to a strong likelihood of the answer being yes. The very same night I finalized and merged the changes to the type language, I dusted off an old prototype of typeclass deriving I had not been able to get working due to insufficiencies of the old type representation. Not only was I [able to get it working][tweet-typeclass-deriving-1] quickly and easily, I was able to do it in [no more than 20 lines of code][tweet-typeclass-deriving-2]. While the implementation is not as robust as it should ideally be, nor is it safe or simple enough yet to be easy for Hackett users to write themselves, making the impossible possible is usually a sign of motion in the right direction.

Unfortunately, the technique outlined in this blog post is not completely flawless. Due to its reliance on the `local-expand` stop list, this technique is incompatible with macros that force recursive expansion using an empty stop list. In the upcoming reimplementation of the Racket macroexpander to be released in Racket 7, this includes `syntax-parameterize`, which unfortunately means syntax parameters don’t work in the type language. This is a problem, and while it’s not a dealbreaker, it is something that will almost certainly have to be fixed at some point. Fortunately, it isn’t intractable, and I’ve been discussing some potential approaches to fixing the problem, whether via changes to the macroexpander or by making macros like `syntax-parameterize` cooperate better with things like Hackett’s type language.

Finally, as seems to be the case more and more with my blog posts, I cannot express enough thanks to [Matthew Flatt][mflatt], without whose help I would probably not have been able to get everything working (not to mention that the Racket macro system would not exist without Matthew inventing and implementing it nearly singlehandedly). Matthew does an almost unfathomable number of things for Racket already without me pestering him with questions, bug reports, and feature requests, but he’s always patient and helpful all the same. Thank you.



[hackett-namespaces-blog-post]: /blog/2017/10/27/a-space-of-their-own-adding-a-type-namespace-to-hackett/
[hackett-type-language-commit]: https://github.com/lexi-lambda/hackett/commit/ba64193da38f63dab2523f42c1b7614cdfa8c935
[hackett-type-language-source]: https://github.com/lexi-lambda/hackett/blob/ba64193da38f63dab2523f42c1b7614cdfa8c935/hackett-lib/hackett/private/type-language.rkt
[local-expand]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._local-expand%29%29
[make-variable-like-transformer]: http://docs.racket-lang.org/syntax/transformer-helpers.html#%28def._%28%28lib._syntax%2Ftransformer..rkt%29._make-variable-like-transformer%29%29
[mflatt]: http://www.cs.utah.edu/~mflatt/
[reference-fully-expanded-programs]: http://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29
[syntax-parse-pattern-directives]: http://docs.racket-lang.org/syntax/stxparse-specifying.html#%28part._.Pattern_.Directives%29
[tweet-new-job]: https://twitter.com/lexi_lambda/status/976533916596097024
[tweet-typeclass-deriving-1]: https://twitter.com/lexi_lambda/status/985051504867446786
[tweet-typeclass-deriving-2]: https://twitter.com/lexi_lambda/status/985052476473856000

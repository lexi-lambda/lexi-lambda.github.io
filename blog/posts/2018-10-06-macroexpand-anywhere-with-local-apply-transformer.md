    Title: Macroexpand anywhere with `local-apply-transformer`!
    Date: 2018-10-06T22:34:46
    Tags: racket, macros

Racket programmers are accustomed to the language’s incredible capacity for extension and customization. Writing useful macros that do complicated things is easy, and it’s simple to add new syntactic forms to meet domain-specific needs. However, it doesn’t take long before many budding macrologists bump into the realization that only *certain positions* in Racket code are subject to macroexpansion.

To illustrate, consider a macro that provides a Clojure-style `let` form:

```racket
(require syntax/parse/define)

(define-simple-macro (clj-let [{~seq x:id e:expr} ...] body:expr ...+)
  (let ([x e] ...) body ...))
```

This can be used anywhere an expression is expected, and it does as one would expect:

```racket
> (clj-let [x 1
            y 2]
    (+ x y))
3
```

However, a novice macro programmer might realize that `clj-let` really only modifies the syntax of *binding pairs* for a `let` form. Therefore, could one define a macro that only adjusts the binding pairs of some existing `let` form instead of expanding to an entire `let`? That is, could one write the above example like this:

```racket
(define-simple-macro (clj-binding-pairs [{~seq x:id e:expr} ...])
  ([x e] ...))

> (let (clj-binding-pairs
        [x 1
         y 2])
    (+ x y))
3
```

The answer is *no*: the binding pairs of a `let` form are not subject to macroexpansion, so the above attempt fails with a syntax error. In this blog post, we will examine the reasons behind this limitation, then explain how to overcome it using a solution that allows macroexpansion *anywhere* in a Racket program.

# Why only some positions are subject to macroexpansion

To understand *why* the macroexpander refuses to touch certain positions in a program, we must first understand how the macro system operates. In Racket, a macro is defined as a compile-time function associated with a particular binding, and macros are given complete control over the syntax trees they are surrounded with. If we define a macro *`mac`*, then we write the expression <code>(*mac* *form*)</code>, *`form`* is provided as-is to *`mac`* as a syntax object. Its structure can be anything at all, since *`mac`* can be an arbitrary Racket function, and that function can use *`form`* however it pleases.

To give a concrete illustration, consider a macro that binds some identifiers to symbols in a local scope:

```racket
(define-simple-macro (let-symbols (x:id ...) body ...+)
  (let ([x 'x] ...) body ...))

> (let-symbols (hello goodbye)
    (list hello goodbye))
'(hello goodbye)
```

It isn’t the most exciting macro in the world, but it illustrates a key point: the first subform to `let-symbols` is a list of identifiers that are eventually put in *binding* position. This means that `hello` and `goodbye` are bindings, not uses, and such bindings shadow any existing bindings that might have been in scope:

```racket
> (let ([foo 42])
    (let-symbols (foo)
      foo))
'foo
```

This might not seem very interesting, but it’s critical to understand, since it means that the expander *can’t know* which sub-pieces of a use of `let-symbols` will eventually be expressions themselves until it expands the macro and discovers it produces a `let` form, so it can’t know where it’s safe to perform macroexpansion. To make this more explicit, imagine we define a macro under some name, then try and use that name with our `let-symbols` macro:

```racket
(define-simple-macro (hello x:id)
  (x:id))

> (let-symbols (hello goodbye)
    hello)
```

What should the above program do? If we treat the first use of `hello` in the `let-symbols` form as a macro application, then `(hello goodbye)` should be transformed into `(goodbye)`, and the use of `hello` in the body should be a syntax error. But if the first use of `hello` was instead intended to be a binder, then it should shadow the `hello` definition above, and the output of the program should be `'hello`.

To avoid the chaos that would ensue if defining a macro could completely break local reasoning about other macros, Racket chooses the second option, and the program produces `'hello`. The macroexpander has no way of knowing *how* each macro will inspect its constituent pieces, so it avoids touching anything until the macro expands. After it discovers the `let` form in the expansion of `let-symbols`, it can safely determine that the body expressions are, indeed, expressions, and it can recursively expand any macros they contain. To put things another way, a macro’s sub-forms are never expanded before the macro itself is expanded, only after.

# Forcing sub-form expansion

The above section explains why the expander must operate as it does, but it’s a little bit unsatisfying. What if we write a macro where we *want* certain sub-forms to be expanded before they are passed to us? Fortunately, the Racket macro system provides an API to handle this use case, too.

It is true that the Racket macro system never *automatically* expands sub-forms before outer forms are expanded, but macro transformers can explicitly op-in to recursive expansion via the [`local-expand`][local-expand] function. This function effectively yields control back to the expander to expand some arbitrary piece of syntax as an expression, and when it returns, the macro transformer can inspect the expanded expression however it wishes. In theory, this can be used to implement extensible macros that allow macroexpansion in locations other than expression position.

To give an example of such a macro, consider the Racket `match` form, which implements an expressive pattern-matcher as a macro. One of the most interesting qualities of Racket’s `match` macro is that its pattern language is user-extensible, essentially allowing pattern-level macros. For example, a user might find they frequently match against natural numbers, and they wish to be able to write `(nat n)` as a shorthand for `(? exact-nonnegative-integer? n)`. Fortunately, this is easy using `define-match-expander`:

```racket
(define-match-expander nat
  (syntax-parser
    [(_ pat)
     #'(? exact-nonnegative-integer? pat)]))

> (match '(-5 -2 4 -7)
    [(list _ ... (nat n) _ ...)
     n])
4
```

Clearly, `match` is somehow expanding the `nat` match expander as a part of its expansion. Is it using `local-expand`?

Well, no. While [a previous blog post of mine][custom-core-forms-1] has illustrated that it is possible to do such a thing with `local-expand` via some clever trickery, `local-expand` is really designed to expand *expressions*. This is a problem, since `(nat n)` is not an expression, it’s a pattern: it will expand into `(? exact-nonnegative-integer? n)`, which will lead to a syntax error, since `?` is not bound in the world of expressions. Instead, for a long while, `match` and forms like it have emulated how the expander performs macroexpansion in ad-hoc ways. Fortunately, as of Racket v7.0, the new [`local-apply-transformer`][local-apply-transformer] API provides a way to invoke recursive macroexpansion in a consistent way, and it doesn’t assume that what’s being expanded is an expression.

## A closer look at `local-apply-transformer`

If `local-apply-transformer` is the answer, what does it actually do? Well, `local-apply-transformer` allows explicitly invoking a transformer function on some piece of syntax and retrieving the result. In other words, `local-apply-transformer` allows expanding an arbitrary macro, but since it doesn’t make any assumptions about what the output will be, it only expands it *once*: just a single step of macro transformation.

To illustrate, we can write a macro that uses `local-apply-transformer` to invoke a transformer function and preserve the result using `quote-syntax`:

```racket
(require (for-syntax syntax/apply-transformer))

(define-for-syntax flip
  (syntax-parser
    [(a b more ...)
     #'(b a more ...)]))

(define-simple-macro (mac)
  #:with result (local-apply-transformer flip #'(([x 1]) let x) 'expression)
  (quote-syntax result))
```

When we use `mac`, our `flip` function will be applied, as a macro, to the syntax object we provide:

```racket
> (mac)
#<syntax (let ((x 1)) x)>
```

Alright, so this works, but it raises some questions. Why is `flip` defined as a function at phase 1 (using `define-for-syntax`) instead of as a macro (using `define-syntax`)? What’s the deal with the `'expression` argument to `local-apply-transformer` given that `local-apply-transformer` is supposedly decoupled from expression expansion? And finally, how is this any different from just calling our `flip` function on the syntax object directly by writing `(flip #'(([x 1]) let x))`?

Let’s start with the first of those questions: why is `flip` defined as a function rather than as a macro? Well, `local-apply-transformer` is a fairly low-level operation: remember, it doesn’t assume *anything* about the argument it’s given! Therefore, it doesn’t take an expression containing a macro and expand it based on its structure, it needs to be explicitly provided the macro transformer function to apply. In practice, this might not seem very useful, since presumably we want to write our macros as macros, not as phase 1 functions. Fortunately, it’s possible to look up the function associated with a macro binding using the [`syntax-local-value`][syntax-local-value] function, so if we use that, we can define `flip` using `define-syntax` as usual:

```racket
(define-syntax flip
  (syntax-parser
    [(a b more ...)
     #'(b a more ...)]))

(define-simple-macro (mac)
  #:with result (local-apply-transformer (syntax-local-value #'flip)
                                         #'(([x 1]) let x)
                                         'expression)
  (quote-syntax result))
```

Now for the next question: what is the meaning of the `'expression` argument? This one is more of a historical artifact than anything else: when the expander applies a macro transformer, it does it in a “context”, which is accessible using the [`syntax-local-context`][syntax-local-context] function. This context can be one of a predefined enumeration of cases, including `'expression`, `'top-level`, `'module`, `'module-begin`, or a list representing a definition context. Whether or not any of those actually apply to our use case, we still have to pick one, but aside from how they affect the value returned by `syntax-local-context` (which some macros inspect), the value we choose is largely irrelevant. Using `'expression` will do, even if it’s a bit of a lie.

Finally, how does any of this differ from just applying the function we get directly? Well, the critical answer is all about *hygiene*. Racket’s macro system is hygienic, which, among other things, ensures bindings defined with the same name in different places do not unintentionally conflict. Racket’s hygiene mechanism is implemented in the macroexpander, when macro transformers are applied. If we just applied the `flip` transformer procedure to a syntax object directly, we would circumvent this hygiene mechanism, potentially causing all sorts of problems. By using `local-apply-transformer`, we ensure hygiene is preserved.

There is one small problem left with our program, however. Can you spot it? The key is to consider what would happen if we used `flip` as an ordinary macro, without using `local-apply-transformer`:

```racket
> (flip (([x 1]) let x))
let: bad syntax
  in: let
```

What happened? Well, remember that when a macro in Racket is used, it receives the whole use site as a syntax object: in this case, `#'(flip (([x 1]) let x))`. This means that `flip` ought to be written to parse its argument slightly differently:

```racket
(define-syntax flip
  (syntax-parser
    [(_ (a b more ...))
     #'(b a more ...)]))
```

Indeed, now that we’ve properly restructured the macro, we can easily switch to using the convenient `define-simple-macro` shorthand:

```racket
(define-simple-macro (flip (a b more ...))
  (b a more ...))
```

This means we also need to update our definition of `mac` to provide the full syntax object the expander would:

```racket
(define-simple-macro (mac)
  #:with result (local-apply-transformer (syntax-local-value #'flip)
                                         #'(flip (([x 1]) let x))
                                         'expression)
  (quote-syntax result))
```

This might seem redundant, but remember, `local-apply-transformer` is very low-level! While the convention that <code>(*mac* . \_)</code> is the syntax for a macro transformation might seem obvious, `local-apply-transformer` makes no assumptions. It just does what we tell it to do.

## Applying `local-apply-transformer`

So what does `local-apply-transformer` have to do with the problem at the beginning of this blog post? Well, as it happens, we can use `local-apply-transformer` to implement a macro that allows expansion *anywhere* using some simple tricks. While it’s true that we cannot magically divine which locations ought to be expanded, what we *can* do is explicitly annotate which places to expand.

To do this, we will implement a macro, `expand-inside`, that looks for subforms annotated with a special `$expand` identifier and performs macro transformation on those locations before proceeding with ordinary macroexpansion. Using the `clj-binding-pairs` example from the beginning of this blog post, our solution to that problem will look like this:

```racket
(define-simple-macro (clj-binding-pairs [{~seq x:id e:expr} ...])
  ([x e] ...))

> (expand-inside
   (let ($expand
         (clj-binding-pairs
          [x 1
           y 2]))
     (+ x y)))
3
```

Put another way, `expand-inside` will force eager expansion on any subform surrounded with an `$expand` annotation.

We’ll start by defining the `$expand` binding itself. This binding won’t mean anything at all outside of `expand-inside`, but we’d like it to be a unique binding so that users can rename it (using, `rename-in`, for example) if they wish. To do this, we’ll use the usual trick of defining it as a macro that always produces an error if it’s ever used:

```racket
(define-syntax ($expand stx)
  (raise-syntax-error #f "illegal outside an ‘expand-inside’ form" stx))
```

Next, we’ll implement a syntax class that will form the bulk of our implementation of `expand-inside`. Since we need to find uses of `$expand` that might be deeply-nested inside the syntax object provided to `expand-inside`, we need to recursively look through the syntax object, find any instances of `$expand`, and put it all back together once we’re done. This can be done relatively cleanly using a recursive syntax class:

```racket
(begin-for-syntax
  (define-syntax-class do-expand-inside
    #:literals [$expand]
    #:attributes [expansion]
    [pattern {~or $expand ($expand . _)}
             #:with :do-expand-inside (do-$expand this-syntax)]
    [pattern (a:do-expand-inside . b:do-expand-inside)
             #:attr expansion
             (let ([reassembled (cons (attribute a.expansion)
                                      (attribute b.expansion))])
               (if (syntax? this-syntax)
                   (datum->syntax this-syntax reassembled
                                  this-syntax this-syntax)
                   reassembled))]
    [pattern _ #:attr expansion this-syntax]))
```

There are some tricky details to get right in the reassembly of pairs, since syntax lists are actually composed of ordinary pairs rather than syntax pairs, but ultimately, the code for walking a syntax object is small. The key case of this syntax class is the call to `do-$expand` in the first clause, which we have not yet defined. This function will actually handle performing the expansion by invoking `local-apply-transformer`:

```racket
(begin-for-syntax
  (define (do-$expand stx)
    (syntax-parse stx
      [(_ {~and form {~or trans (trans . _)}})
       #:declare trans (static (disjoin procedure? set!-transformer?)
                               "syntax transformer")
       (local-apply-transformer (attribute trans.value)
                                #'form
                                'expression)])))
```

This uses the handy `static` syntax class that comes with `syntax/parse`, which implicitly handles the call to `syntax-local-value` and produces a nice error message if the value returned does not match a predicate. All we have to do is apply the transformer value bound to the `trans.value` attribute using `local-apply-transformer`, and now the `expand-macro` can be written in just a couple lines of code:

```racket
(define-syntax-parser expand-inside
  #:track-literals
  [(_ form:do-expand-inside) #'form.expansion])
```

(Using the `#:track-literals` option, also new in Racket v7.0, ensures that Check Syntax will be able to recognize the uses of `$expand` that disappear from after `expand-inside` is expanded.)

Putting everything together, our example from above really works:

```racket
(define-simple-macro (clj-binding-pairs [{~seq x:id e:expr} ...])
  ([x e] ...))

> (expand-inside
   (let ($expand
         (clj-binding-pairs
          [x 1
           y 2]))
     (+ x y)))
3
```

That’s it. All told, the entire implementation is only about 30 lines of code. For a full, compilable, working example, see [this gist](https://gist.github.com/lexi-lambda/65d69043023b519694f50dfca2dc7d33).

[custom-core-forms-1]: /blog/2018/04/15/reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket/
[local-apply-transformer]: http://docs.racket-lang.org/syntax/transformer-helpers.html#%28def._%28%28lib._syntax%2Fapply-transformer..rkt%29._local-apply-transformer%29%29
[local-expand]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._local-expand%29%29
[syntax-local-context]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-context%29%29
[syntax-local-value]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-value%29%29

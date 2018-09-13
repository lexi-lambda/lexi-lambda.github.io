    Title: Custom core forms in Racket, part II: generalizing to arbitrary expressions and internal definitions
    Date: 2018-09-13T00:00:00
    Tags: racket, macros

In my [previous blog post][custom-core-forms-1], I covered the process involved in creating a small language with a custom set of core forms. Specifically, it discussed what was necessary to create Hackett’s type language, which involved expanding to custom expressions. While somewhat involved, Hackett’s type language was actually a relatively simple example to use, since it only made use of a subset of the linguistic features Racket supports. In this blog post, I’ll demonstrate how that same technique can be generalized to support runtime bindings and internal definitions, two key concepts useful if intending to develop a more featureful language than Hackett’s intentionally-restrictive type system.

<!-- more -->

# What are internal definitions?

This blog post is going to be largely focused on how to properly implement a form that handles the expansion of *internal definitions* in Racket. This is a tricky topic to get right, but before we can discuss internal definitions, we have to establish what definitions themselves are and how they relate to other binding forms.

In a traditional Lisp, there are two kinds of bindings: top-level bindings and local bindings. In Scheme and its descendants, this distinction is characterized by two different binding forms, `define` and `let`. To a first approximation, `define` is used for defining top-level, global bindings, and it resembles variable definitions in many mainstream languages in the sense that definitions using `define` are not really expressions. They don’t produce a value, they define a new binding. Definitions written with `define` look like this:

```racket
(define x 42)
(define y "hello")
```

Each definition is made up of two parts: the *binding identifier*, in this case `x` and `y`, and the *right hand side*, or RHS for short. Each RHS is a single expression that will be evaluated and used as the value for the introduced binding.

In Scheme and Racket, `define` also supports a shorthand form for defining functions in a natural syntax without the explicit need to write `lambda`, which looks like this:

```racket
(define (double x)
  (* x 2))
```

However, this is just syntactic sugar. The above form is really just a macro for the following equivalent, expanded version:

```racket
(define double
  (lambda (x) (* x 2)))
```

Since we only really care about fully-expanded programs, we’ll focus exclusively on the expanded version of `define` in this blog post, since if we handle that, we’ll also handle the function shorthand’s expansion.

In contrast to `define`, there is also `let`, which has a rather different shape. A `let` form *is* an expression, and it creates local bindings in a delimited scope:

```racket
(let ([x 2]
      [y 3])
  (+ x y))
```

The binding clauses of a `let` expression are known as the *binding pairs*, and the sequence of expressions afterwards are known as the *body* of the `let`. Each binding pair consists of a binding identifier and a RHS, just like a top-level definition created with `define`, but while `define` is a standalone form, the binding pairs cannot meaningfully exist outside of a `let`—they are recognized as part of the grammar of the `let` form itself.

Like other Lisps, Racket distinguishes between top-level—or, more precisely, *module-level*—bindings and local bindings. A module-level binding can be exported using `provide`, which will allow other modules to access the binding by importing the module with `require`. Such definitions are treated specially by the macroexpander, compiler, and runtime system alike. There is a pervasive, meaningful difference between module-level definitions and local definitions besides simply scope.

I am making an effort to make this as clear as possible before discussing internal definitions because without it, the following point can be rather confusing: internal definitions are written using `define`, but they are local bindings, *not* module-level ones! In Racket, `define` is allowed to appear in the body of virtually all block forms like `let`, so the following is a legal program:

```racket
(let ()
  (define x 2)
  (define y 3)
  (+ x y))
```

This program is equivalent to the one expressed using `let`. In fact, when the Racket macroexpander expands these local uses of `define`, it actually translates them into uses of `letrec`. After expanding the above expression, it would look closer to the following:

```racket
(let ()
  (letrec ([x 2]
           [y 3])
    (+ x y)))
```

In this sense, `define` is a form with a double life in Racket. When used at the module level, it creates module-level definitions, which remain in a fully-expanded program and can be imported by other modules. When used inside local blocks, it creates internal definitions, which do not remain in fully expanded programs, since they are translated into recursive local binding forms.

In this blog post, we will ignore module-level definitions. Like in the previous blog post, we will focus exclusively on expanding expressions, not whole modules. However, we will extend our language to allow internal definitions inside local binding forms, and we will translate them into `letrec` forms in the same way as the Racket macroexpander.

# Revisiting and generalizing the expression expander

In the previous blog post, our expander expanded types, which were essentially expressions from the perspective of the Racket macroexpander. We wrote a syntax class that handled the parsing of a restricted type grammar that disallowed most Racket-level expression forms, like `begin`, `if`, `#%plain-lambda`, and `quote`. After all, Hackett is not dependently-typed, and it disallows explicit type abstraction to preserve type inference, so it would be a very bad thing if we allowed `if` or explicit lambda abstraction to appear in our types. For this blog post, however, we will restructure the type expander to handle the full grammar of expressions permitted by Racket.

While the syntax class approach used in the previous blog post was cute, this blog post will use ordinary functions defined at phase 1 instead of syntax classes. In practice, this provides superior error reporting, since it reports syntax errors in terms of the form that went wrong, not the form prior to expansion. Since we can still use `syntax-parse` to parse the arguments to these functions, we don’t lose any expressive power in the expression of our pattern language.

To start, we’ll extract the call to `local-expand` into its own function. This corresponds to the `type` syntax class from the previous blog post, but we’ll use phase 1 parameters to avoid threading so many explicit function arguments around:

```racket
(begin-for-syntax
  (define current-context (make-parameter #f))
  (define current-stop-list (make-parameter (list #'define-values #'define-syntaxes)))
  (define current-intdef-ctx (make-parameter #f))

  (define (current-expand stx)
    (local-expand stx
                  (current-context)
                  (current-stop-list)
                  (current-intdef-ctx))))
```

Due to the way `local-expand` implicitly extends the stop list, as discussed in the previous blog post, we can initialize the stop list to a list containing just `define-values` and `define-syntaxes`, and the other forms we care about will be included automatically.

Next, we’ll use this function to implement a `expand-expression` function, which will emulate the way the expander expands a single expression, as the name implies. We’ll ignore any custom core forms for now, so we’ll just focus exclusively on the Racket core forms.

A few of Racket’s core forms are not actually subject to any expansion at all, and they expand to themselves. These forms are `quote`, `quote-syntax`, and `#%variable-reference`. Additionally, `#%top` is not something useful to handle ourselves, since it involves no recursive expansion, so we’ll treat it as if it expands to itself as well and allow the expander to raise any unbound identifier errors it produces. Here’s what the `expand-expression` function looks like when exclusively handling these things:

```racket
(define (expand-expression stx)
    (syntax-parse (parameterize ([current-context 'expression])
                    (current-expand stx))
      #:literal-sets [kernel-literals]
      [({~or quote quote-syntax #%top #%variable-reference} ~! . _)
       this-syntax]))
```

Another set of Racket core forms are simple expressions which contain subforms, all of which are themselves expressions. These forms include things like `#%expression`, `begin`, and `if`, and they can be expanded recursively. We’ll add another clause to handle these, which can be written with a straightforward recursive call to `expand-expression`:

```racket
[({~and head {~or #%expression #%plain-app begin begin0 if with-continuation-mark}} ~! form ...)
 #:with [form* ...] (map expand-expression (attribute form))
 (syntax/loc/props this-syntax
   (head form* ...))]
```

Another easy form to handle is `set!`, since it also requires simple recursive expansion, but it can’t be handled in the same way as the above forms since one of its subforms (the variable to mutate) should not be expanded. It needs another small clause:

```racket
[(head:set! ~! x:id rhs)
 (quasisyntax/loc/props this-syntax
   (head x #,(expand-expression #'rhs)))]
```

The other expressions are harder, since they’re all the binding forms. Fully-expanded Racket code has four local binding forms: `#%plain-lambda`, `case-lambda`, `let-values`, and `letrec-values`. Additionally, as discussed in the previous blog post, `local-expand` can also produce `letrec-syntaxes+values` forms produced by local syntax bindings. In the type expander, we completely disallowed runtime bindings from appearing in the resulting program, so we completely removed `letrec-syntaxes+values` in our expansion, but in the case of handling arbitrary Racket programs, we actually want to leave a `letrec-values` form behind to hold any runtime bindings (i.e. the `values` part of `letrec-syntaxes+values`).

We’ll start with `#%plain-lambda`, which is the simplest of all the five aforementioned binding forms. It binds a sequence of identifiers at runtime, and they are in scope within the body of the lambda expression. Just as we created and used an internal-definition context to hold the bindings of a `letrec-syntax+values` form in the previous blog post, we’ll do the same for Racket’s other binding forms as well:

```racket
[(head:#%plain-lambda ~! [x:id ...] body ...)
 #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
       (syntax-local-bind-syntaxes (attribute x) #f intdef-ctx)]
 #:with [x* ...] (internal-definition-context-introduce intdef-ctx #'[x ...])
 #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                      (map expand-expression (attribute body)))
 (syntax/loc/props this-syntax
   (head [x* ...] body* ...))]
```

However, the above handling of `#%plain-lambda` isn’t *quite* right, since the argument list can also include a “rest argument” binding in addition to a sequence of positional arguments. To accommodate this, we can introduce a simple syntax class that handles the different permutations:

```racket
(begin-for-syntax
  (define-syntax-class plain-formals
    #:description "formals"
    #:attributes [[id 1]]
    #:commit
    [pattern (id:id ...)]
    [pattern (id*:id ... . id**:id) #:with [id ...] #'[id* ... id**]]))
```

Now we can use this to adjust `#%plain-lambda` to handle rest arguments:

```racket
[(head:#%plain-lambda ~! formals:plain-formals body ...)
 #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
       (syntax-local-bind-syntaxes (attribute formals.id) #f intdef-ctx)]
 #:with formals* (internal-definition-context-introduce intdef-ctx #'formals)
 #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                      (map expand-expression (attribute body)))
 (syntax/loc/props this-syntax
   (head formals* body* ...))]
```

Next, we’ll handle `case-lambda`. As it turns out, expanding `case-lambda` is almost exactly the same as expanding `#%plain-lambda`, except that it has multiple clauses. Since each clause is expanded identically to the body of a `#%plain-lambda`, and it even has the same shape, the clauses can be extracted into a separate syntax class to share code between the two forms:

```racket
(begin-for-syntax
  (define-syntax-class lambda-clause
    #:description #f
    #:attributes [expansion]
    #:commit
    [pattern [formals:plain-formals body ...]
             #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
                   (syntax-local-bind-syntaxes (attribute formals.id) #f intdef-ctx)]
             #:with formals* (internal-definition-context-introduce intdef-ctx #'formals)
             #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                                  (map expand-expression (attribute body)))
             #:attr expansion #'[formals* body* ...]]))
```

Now, both `#%plain-lambda` and `case-lambda` can be handled in a few lines of code each:

```racket
[(head:#%plain-lambda ~! . clause:lambda-clause)
 (syntax/loc/props this-syntax
   (head . clause.expansion))]

[(head:case-lambda ~! clause:lambda-clause ...)
 (syntax/loc/props this-syntax
   (head clause.expansion ...))]
```

Finally, we need to tackle the three `let` forms. None of these really involve any more fundamental ideas than what we’ve already covered, but they are a little bit more involved than the variants of lambda due to the need to handle the RHSs. Each variant is slightly different, but not dramatically so: the bindings aren’t in scope when expanding the RHSs of `let-values`, but they are for `letrec-values` and `letrec-syntaxes+values`, and `letrec-syntaxes+values` creates transformer bindings and must evaluate some RHSs in phase 1 while `let-values` and `letrec-values` exclusively bind runtime bindings. It would be possible to implement these three forms in separate clauses, but since we’d ideally like to duplicate as little code as possible, we can write a rather elaborate `syntax/parse` pattern to handle all three binding forms all at once.

We’ll start by handling `let-values` alone to keep things simple:

```racket
[(head:let-values ~! ([(x:id ...) rhs] ...) body ...)
 #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
       (syntax-local-bind-syntaxes (append* (attribute x)) #f intdef-ctx)]
 #:with [[x* ...] ...] (internal-definition-context-introduce intdef-ctx #'[[x ...] ...])
 #:with [rhs* ...] (map expand-expression (attribute rhs))
 #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                      (map expand-expression (attribute body)))
 (syntax/loc/props this-syntax
   (head ([(x* ...) rhs*] ...) body* ...))]
```

This isn’t dramatically different from the implementation of `#%plain-lambda`. The only difference is that we have to recursively invoke `expand-expression` on the RHSs in addition to expanding the body expressions. To handle `letrec-values` in the same clause, however, we’ll have to get a little more creative.

So far, we haven’t actually tapped very far into `syntax/parse`’s pattern language over the course of these two blog posts. The full language available to patterns is rather extensive, and we can take advantage of that to write a modification of the above clause that handles both `let-values` and `letrec-values` at once:

```racket
[({~or {~and head:let-values {~bind [rec? #f]}}
       {~and head:letrec-values {~bind [rec? #f]}}}
  ~! ([(x:id ...) rhs] ...) body ...)
 #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
       (syntax-local-bind-syntaxes (append* (attribute x)) #f intdef-ctx)]
 #:with [[x* ...] ...] (internal-definition-context-introduce intdef-ctx #'[[x ...] ...])
 #:with [rhs* ...] (if (attribute rec?)
                       (parameterize ([current-intdef-ctx intdef-ctx])
                         (map expand-expression (attribute rhs)))
                       (map expand-expression (attribute rhs)))
 #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                      (map expand-expression (attribute body)))
 (syntax/loc/props this-syntax
   (head ([(x* ...) rhs*] ...) body* ...))]
```

The `~bind` pattern allows us to explicitly control how attributes are bound as part of the pattern-matching process, which allows us to track when we want to enable the recursive binding behavior of `letrec-values` in our handler code. Since the vast majority of the logic is otherwise identical, this is a significant improvement over duplicating the clause.

Adding support for `letrec-syntaxes+values` is done in the same general way, but the pattern is even more involved. In addition to tracking whether or not the bindings are recursive, we have to track if any syntax bindings were present at all, and if they were, bind them with `syntax-local-bind-syntaxes`:

```racket
[({~or {~or {~and head:let-values ~! {~bind [rec? #f] [stxs? #f]}}
            {~and head:letrec-values ~! {~bind [rec? #t] [stxs? #f]}}}
       {~seq head:letrec-syntaxes+values {~bind [rec? #t] [stxs? #t]}
             ~! ([(x/s:id ...) rhs/s] ...)}}
  ([(x:id ...) rhs] ...) body ...)
 #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
       (syntax-local-bind-syntaxes (append* (attribute x)) #f intdef-ctx)
       (when (attribute stxs?)
         (for ([xs/s (in-list (attribute x/s))]
               [rhs/s (in-list (attribute rhs/s))])
           (syntax-local-bind-syntaxes xs/s rhs/s intdef-ctx)))]
 #:with [[x* ...] ...] (internal-definition-context-introduce intdef-ctx #'[[x ...] ...])
 #:with [rhs* ...] (if (attribute rec?)
                       (parameterize ([current-intdef-ctx intdef-ctx])
                         (map expand-expression (attribute rhs)))
                       (map expand-expression (attribute rhs)))
 #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                      (map expand-expression (attribute body)))
 (if (attribute stxs?)
     (~> (syntax/loc this-syntax
           (letrec-values ([(x* ...) rhs*] ...) body* ...))
         (syntax-track-origin this-syntax #'head))
     (syntax/loc/props this-syntax
       (head ([(x* ...) rhs*] ...) body* ...)))]
```

This behemoth clause handles all three varieties of `let` forms that can appear in the result of `local-expand`. Notably, in the `letrec-syntaxes+values` case, we expand into `letrec-values`, since the transformer bindings are effectively erased, and we use `syntax-track-origin` to record that the result originally came from a use of `letrec-syntaxes+values`.

With these five clauses, we’ve handled all the special forms that can appear in expression position in Racket’s kernel language. To tie things off, we just need to handle the cases of a variable reference, which is represented by a bare identifier not bound to syntax, or literal data, like numbers or strings. We can add one more clause at the end to handle those:

```racket
[_
 this-syntax]
```

Putting them all together, our `expand-expression` function looks as follows:

```racket
(begin-for-syntax
  (define (expand-expression stx)
    (syntax-parse (parameterize ([current-context 'expression])
                    (current-expand stx))
      #:literal-sets [kernel-literals]
      [({~or quote quote-syntax #%top #%variable-reference} ~! . _)
       this-syntax]

      [({~and head {~or #%expression #%plain-app begin begin0 if with-continuation-mark}} ~! form ...)
       #:with [form* ...] (map expand-expression (attribute form))
       (syntax/loc/props this-syntax
         (head form* ...))]

      [(head:#%plain-lambda ~! . clause:lambda-clause)
       (syntax/loc/props this-syntax
         (head . clause.expansion))]

      [(head:case-lambda ~! clause:lambda-clause ...)
       (syntax/loc/props this-syntax
         (head clause.expansion ...))]

      [({~or {~or {~and head:let-values ~! {~bind [rec? #f] [stxs? #f]}}
                  {~and head:letrec-values ~! {~bind [rec? #t] [stxs? #f]}}}
             {~seq head:letrec-syntaxes+values {~bind [rec? #t] [stxs? #t]}
                   ~! ([(x/s:id ...) rhs/s] ...)}}
        ([(x:id ...) rhs] ...) body ...)
       #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
             (syntax-local-bind-syntaxes (append* (attribute x)) #f intdef-ctx)
             (when (attribute stxs?)
               (for ([xs/s (in-list (attribute x/s))]
                     [rhs/s (in-list (attribute rhs/s))])
                 (syntax-local-bind-syntaxes xs/s rhs/s intdef-ctx)))]
       #:with [[x* ...] ...] (internal-definition-context-introduce intdef-ctx #'[[x ...] ...])
       #:with [rhs* ...] (if (attribute rec?)
                             (parameterize ([current-intdef-ctx intdef-ctx])
                               (map expand-expression (attribute rhs)))
                             (map expand-expression (attribute rhs)))
       #:with [body* ...] (parameterize ([current-intdef-ctx intdef-ctx])
                            (map expand-expression (attribute body)))
       (if (attribute stxs?)
           (~> (syntax/loc this-syntax
                 (letrec-values ([(x* ...) rhs*] ...) body* ...))
               (syntax-track-origin this-syntax #'head))
           (syntax/loc/props this-syntax
             (head ([(x* ...) rhs*] ...) body* ...)))]

      [_
       this-syntax])))
```

If we try it out, we’ll see that it really does work! Even complicated local binding forms are handled properly by our expander:

```racket
> (expand-expression
   #'(let ([x 42])
       (letrec-syntax ([y (make-rename-transformer #'z)]
                       [z (make-rename-transformer #'x)])
         (+ y 3))))
#<syntax (let-values (((x) '42))
           (letrec-values ()
             (#%plain-app + x '3)))>
```

We are now able to expand arbitrary Racket expressions in the same way that the expander does. While this might not seem immediately useful—after all, we haven’t actually gained anything here over just calling `local-expand` with an empty stop list—we can use this as the basis of an expander that can extensibly handle custom core forms, which I may cover in a future blog post.

# Adding support for internal definitions

In the previous section, we defined an expander that could expand arbitrary Racket expressions, but our expander is still imperfect: we still do not support internal definitions. For all forms that have bodies, including `#%plain-lambda`, `case-lambda`, `let-values`, `letrec-values`, and `letrec-syntaxes+values`, Racket permits the use of internal definitions.

In practice, internal-definition contexts allow for an increased degree of modularity compared to traditional local binding forms, since they provide an *extensible* binding language. Users may mix many different binding forms within a single definition context, such as `define`, `define-syntax`, `match-define`, and even `struct`. However, this means the rewriting process described earlier in this blog post is not as simple as detecting the definitions and lifting them into a local binding form, since it’s not immediately apparent which forms are binding forms and which are expressions!

For this reason, expanding internal-definition contexts happens to be a nontrivial problem in itself. It involves a little more care than expanding expressions does, since it requires using partial expansion to discover which forms are definitions and which forms are expressions. We must take care to never expand too much, but also to expand enough that we reveal all uses of `define-values` and `define-syntaxes` (which all definition forms eventually expand into). We also must handle the splicing behavior of `begin`, which is necessary to allow single forms to expand into multiple definitions.

We’ll start by writing an `expand-body` function, which operates similarly to our previous `expand-expression` function. Unlike `expand-expression`, `expand-body` will accept a *list* of syntax objects, which represents the sequence of forms that make up the body. Logically, each body will create a first-class definition context with `syntax-local-make-definition-context` to represent the sequence of definitions:

```racket
(begin-for-syntax
  (define (expand-body stxs)
    (define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
    (parameterize ([current-context (list (gensym))]
                   [current-intdef-ctx intdef-ctx])
      )))
```

The bulk of our `expand-body` function will be a loop that partially expands body forms, adds definitions to the definition context as it discovers them, and returns the expressions and runtime definitions to be rewritten into binding pairs for a `letrec-values` form. Additionally, the loop will also track so-called *disappeared uses* and *disappeared bindings*, which are attached to the expansion using syntax properties to allow tools like DrRacket to learn about the binding structure of phase 1 definitions that are erased as part of macroexpansion.

The skeleton of this loop is relatively straightforward to write. We will iterate over the syntax objects that make up the body, expand them, and process the expansion using `syntax-parse`:

```racket
(begin-for-syntax
  (define (expand-body stxs)
    (define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
    (parameterize ([current-context (list (gensym))]
                   [current-intdef-ctx intdef-ctx])
      (define-values [binding-clauses exprs disappeared-uses disappeared-bindings]
        (let loop ([stxs stxs]
                   [binding-clauses '()]
                   [exprs '()]
                   [disappeared-uses '()]
                   [disappeared-bindings '()])
          (if (empty? stxs)
              (values (reverse binding-clauses) (reverse exprs) disappeared-uses disappeared-bindings)
              (syntax-parse (current-expand (first stxs))
                #:literal-sets [kernel-literals]
                )))))))
```

The hard part, of course, is actually handling the potential results of that expansion. We need to handle three forms specially: `begin`, `define-values`, and `define-syntaxes`. All other results of partial expansion will be treated as expressions. We’ll start by handling `begin`, since it’s the simplest case; we only need to prepend the subforms to the list of body forms to be processed, then continue looping:

```racket
[(head:begin ~! form ...)
 (loop (append (attribute form) stxs) binding-clauses exprs
       disappeared-uses disappeared-bindings)]
```

However, as is often the case, this isn’t quite perfect, since the information that these forms came from a surrounding `begin` is lost, which tools like DrRacket want to know. To solve this problem, the expander adjusts the `origin` property for all spliced forms, which we can mimic using `syntax-track-origin`:

```racket
[(head:begin ~! form ...)
 (loop (append (for/list ([form (in-list (attribute form))])
                 (syntax-track-origin form this-syntax #'head))
               stxs)
       binding-clauses exprs disappeared-uses disappeared-bindings)]
```

This is sufficient for `begin`, so we can move onto the actual definitions themselves. This actually isn’t too hard, since we just need to add the bindings we discover to the first-class definition context and preserve `define-values` bindings as binding pairs:

```racket
[(head:define-values ~! [x:id ...] rhs)
 #:do [(syntax-local-bind-syntaxes (attribute x) #f intdef-ctx)]
 (loop (rest stxs) (cons #'[(x ...) rhs] binding-clauses) exprs
       disappeared-uses disappeared-bindings)]
```

This solution is missing one thing, however, which is the use of `syntax-local-identifier-as-binding` to any use-site scopes that were added to the binding identifier while expanding the binding form in the definition context. Explaining precisely why this is necessary is outside the scope of this blog post, and is best understood by reading [the section on use-site scopes][use-site-scopes] in the paper that describes the theory behind Racket’s current macro system, Bindings as Sets of Scopes. In any case, the impact on our implementation is small:

```racket
[(head:define-values ~! [x:id ...] rhs)
 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
 #:do [(syntax-local-bind-syntaxes (attribute x*) #f intdef-ctx)]
 (loop (rest stxs) (cons #'[(x* ...) rhs] binding-clauses) exprs
       disappeared-uses disappeared-bindings)]
```

Finally, as with `begin`, we want to track that the binding pairs we generate actually came from a use of `define-values` (which in turn likely came from a use of some other definition form). Therefore, we’ll add another use of `syntax-track-origin` to copy and extend the necessary properties:

```racket
[(head:define-values ~! [x:id ...] rhs)
 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
 #:do [(syntax-local-bind-syntaxes (attribute x*) #f intdef-ctx)]
 (loop
  (rest stxs)
  (cons (syntax-track-origin #'[(x* ...) rhs] this-syntax #'head) binding-clauses)
  exprs disappeared-uses disappeared-bindings)]
```

That’s it for `define-values`. All that’s left is to handle `define-syntaxes`, which is quite similar, but instead of storing the definition in a binding pair, its RHS is immediately evaluated and added to the definition context using `syntax-local-bind-syntaxes`:

```racket
[(head:define-syntaxes ~! [x:id ...] rhs)
 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
 #:do [(syntax-local-bind-syntaxes (attribute x*) #'rhs intdef-ctx)]
 (loop (rest stxs) binding-clauses exprs
       (cons #'head disappeared-uses) (cons (attribute x*) disappeared-bindings))]
```

As the above snippet indicates, this is also where the disappeared uses and disappeared bindings come in. In previous cases, we’ve used `syntax-track-origin` to indicate that a piece of syntax was the result of expanding a different piece of syntax, but in this case, `define-syntaxes` doesn’t expand into anything at all; it’s simply removed from the expansion entirely. Therefore, we need to resort to tracking the information in syntax properties on the resulting `letrec-values` form, so we’ll save them for later.

Finally, to finish things up, we can add a catchall clause that handles all other forms, which are now guaranteed to be expressions:

```racket
[_
 (loop (rest stxs) binding-clauses (cons this-syntax exprs)
       disappeared-uses disappeared-bindings)]
```

This completes our loop that processes definition forms, so all that’s left to do is handle the results. The only significant remaining work is to actually expand the RHSs of the binding pairs we collected and the body expressions, which can be done by calling our own `expand-expression` function directly:

```racket
(define expanded-binding-clauses
  (for/list ([binding-clause (in-list binding-clauses)])
    (syntax-parse binding-clause
      [[(x ...) rhs]
       (quasisyntax/loc/props this-syntax
         [(x ...) #,(expand-expression #'rhs)])])))
(define expanded-exprs (map expand-expression exprs))
```

Finally, we can assemble all the pieces together into a single local binding form with the appropriate syntax properties:

```racket
(~> #`(letrec-values #,expanded-binding-clauses #,@expanded-exprs)
    (syntax-property 'disappeared-uses disappeared-uses)
    (syntax-property 'disappeared-bindings disappeared-bindings))
```

That’s it. We’ve now written an `expand-body` function that can process internal definition contexts in the same way that the macroexpander does. Overall, the whole function is just under 45 lines of code:

```racket
(begin-for-syntax
  (define (expand-body stxs)
    (define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
    (parameterize ([current-context (list (gensym))]
                   [current-intdef-ctx intdef-ctx])
      (define-values [binding-clauses exprs disappeared-uses disappeared-bindings]
        (let loop ([stxs stxs]
                   [binding-clauses '()]
                   [exprs '()]
                   [disappeared-uses '()]
                   [disappeared-bindings '()])
          (if (empty? stxs)
              (values (reverse binding-clauses) (reverse exprs) disappeared-uses disappeared-bindings)
              (syntax-parse (current-expand (first stxs))
                #:literal-sets [kernel-literals]
                [(head:begin ~! form ...)
                 (loop (append (for/list ([form (in-list (attribute form))])
                                 (syntax-track-origin form this-syntax #'head))
                               stxs)
                       binding-clauses exprs disappeared-uses disappeared-bindings)]
                [(head:define-values ~! [x:id ...] rhs)
                 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
                 #:do [(syntax-local-bind-syntaxes (attribute x*) #f intdef-ctx)]
                 (loop
                  (rest stxs)
                  (cons (syntax-track-origin #'[(x* ...) rhs] this-syntax #'head) binding-clauses)
                  exprs disappeared-uses disappeared-bindings)]
                [(head:define-syntaxes ~! [x:id ...] rhs)
                 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
                 #:do [(syntax-local-bind-syntaxes (attribute x*) #'rhs intdef-ctx)]
                 (loop (rest stxs) binding-clauses exprs
                       (cons #'head disappeared-uses) (cons (attribute x*) disappeared-bindings))]
                [_
                 (loop (rest stxs) binding-clauses (cons this-syntax exprs)
                       disappeared-uses disappeared-bindings)]))))
      (define expanded-binding-clauses
        (for/list ([binding-clause (in-list binding-clauses)])
          (syntax-parse binding-clause
            [[(x ...) rhs]
             (quasisyntax/loc/props this-syntax
               [(x ...) #,(expand-expression #'rhs)])])))
      (define expanded-exprs (map expand-expression exprs))
      (~> #`(letrec-values #,expanded-binding-clauses #,@expanded-exprs)
          (syntax-property 'disappeared-uses disappeared-uses)
          (syntax-property 'disappeared-bindings disappeared-bindings)))))
```

The next step is to actually use this function. We need to replace certain recursive calls to `expand-expression` with calls to `expand-body`, but if we do this naïvely, we’ll have some problems. Currently, when we expand body forms, they’re always immediately inside another definition context (i.e. the bindings introduced by lambda formals or by `let` binding pairs), but they haven’t actually been expanded in that context yet. When we call `expand-body`, we create a nested context, which will inherit the bindings, but won’t automatically add the parent context’s scope. Therefore, we need to manually call `internal-definition-context-introduce` on the body syntax objects before calling `expand-body`. We can write a small helper function to make this easier:

```racket
(begin-for-syntax
  (define (expand-body/in-ctx stxs ctx)
    (define (add-ctx-scope stx)
      (internal-definition-context-introduce ctx stx 'add))
    (parameterize ([current-intdef-ctx ctx])
      (add-ctx-scope (expand-body (map add-ctx-scope stxs))))))
```

Now we just need to replace the relevant calls to `expand-expression` with calls to `expand-body/in-ctx`, starting with a minor adjustment to our `lambda-clause` syntax class from earlier:

```racket
(begin-for-syntax
  (define-syntax-class lambda-clause
    #:description #f
    #:attributes [expansion]
    #:commit
    [pattern [formals:plain-formals body ...]
             #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
                   (syntax-local-bind-syntaxes (attribute formals.id) #f intdef-ctx)]
             #:with formals* (internal-definition-context-introduce intdef-ctx #'formals)
             #:with body* (expand-body/in-ctx (attribute body) intdef-ctx)
             #:attr expansion #'[formals* body*]]))
```

The only other change must occur in the handling of the various `let` forms, which similarly replaces `expand-expression` with `expand-body/in-ctx`:

```racket
[({~or {~or {~and head:let-values ~! {~bind [rec? #f] [stxs? #f]}}
            {~and head:letrec-values ~! {~bind [rec? #t] [stxs? #f]}}}
       {~seq head:letrec-syntaxes+values {~bind [rec? #t] [stxs? #t]}
             ~! ([(x/s:id ...) rhs/s] ...)}}
  ([(x:id ...) rhs] ...) body ...)
 #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
       (syntax-local-bind-syntaxes (append* (attribute x)) #f intdef-ctx)
       (when (attribute stxs?)
         (for ([xs/s (in-list (attribute x/s))]
               [rhs/s (in-list (attribute rhs/s))])
           (syntax-local-bind-syntaxes xs/s rhs/s intdef-ctx)))]
 #:with [[x* ...] ...] (internal-definition-context-introduce intdef-ctx #'[[x ...] ...])
 #:with [rhs* ...] (if (attribute rec?)
                       (parameterize ([current-intdef-ctx intdef-ctx])
                         (map expand-expression (attribute rhs)))
                       (map expand-expression (attribute rhs)))
 #:with body* (expand-body/in-ctx (attribute body) intdef-ctx)
 (if (attribute stxs?)
     (~> (syntax/loc this-syntax
           (letrec-values ([(x* ...) rhs*] ...) body*))
         (syntax-track-origin this-syntax #'head))
     (syntax/loc/props this-syntax
       (head ([(x* ...) rhs*] ...) body*)))]
```

With these changes, we’ve now extended our expression expander with the ability to expand internal definitions. We can see this in action on a simple example:

```racket
> (expand-expression
   #'(let ()
       (define x 42)
       (define-syntax y (make-rename-transformer #'z))
       (define-syntax z (make-rename-transformer #'x))
       (+ y 3)))
#<syntax (let-values ()
           (letrec-values ([(x) '42])
             (#%app + x '3)))>
```

Just as we’d like, the transformer bindings were expanded and subsequently eliminated, and the runtime binding was collected into a `letrec-values` form. The outer `let-values` is left over from the outer `let`, which is needed only to create an internal-definition context to hold our internal definitions.

# Putting the expression expander to work

So far, we’ve done a lot of work to emulate the behavior of Racket’s macroexpander, and as the above example demonstrates, we’ve been fairly successful in that goal. However, you might be wondering *why* we did any of this, as replicating the behavior of `local-expand` is not very useful on its own. As mentioned above, this can be used as the foundation of an expander for custom core forms that extends, rather than replaces, the built-in Racket core forms, It can also be used to “cheat” and expand through the behavior of the `local-expand` stop list, which implicitly adds the Racket core forms to any non-empty stop list. Hopefully, I’ll have a chance to cover some of these things more deeply in the future, but for now, I’ll just give a small taste of the latter.

By using the power of our `expand-expression` function, it’s actually possible to use this kind of expression expander to do genuinely nefarious things, such as hijack the behavior of arbitrary macros! For example, we could do something evil like make `for` loops run in reverse order by adding `for` to `current-stop-list`, then adding an additional special case to `expand-expression` for `for`:

```racket
(begin-for-syntax
  (define current-stop-list (make-parameter (list #'define-values #'define-syntaxes #'for)))

  (define (expand-expression stx)
    (syntax-parse (parameterize ([current-context 'expression])
                    (current-expand stx))
      #:literal-sets [kernel-literals]
      #:literals [for]
      ; ...
      [(head:for ([x:id seq:expr] ...) body ...+)
       (syntax/loc/props this-syntax
         (head ([x (in-list (reverse (sequence->list seq)))] ...)
           body ...))]
      ; ...
    )))
```

Amazingly, due to the fact that we’ve taken complete control of the expansion process, this will rewrite uses of `for` *even if they are introduced by macroexpansion*. For example, we could write a small macro that expands into a use of `for`:

```racket
(define-simple-macro (print-up-to n)
  (for ([i (in-range n)])
    (println i)))

> (print-up-to 5)
0
1
2
3
4
```

If we write a wrapper macro that applies our evil version of `expand-expression` to its body, then wrap a use of our `print-up-to` macro with it, it will really execute the loop in reverse order:

```racket
(define-syntax-parser hijack-for-loops
  [(_ form:expr) (expand-expression #'form)])

> (hijack-for-loops
   (print-up-to 5))
4
3
2
1
0
```

On its own, this is not that impressive, since we could have just used `local-expand` on the body directly to achieve this. However, what’s remarkable about `hijack-for-loops` is that it will work even if the `for` loop is buried deep inside some arbitrary expression:

```racket
> (define foo
    (hijack-for-loops
     (lambda (x)
       (define n (* x 2))
       (print-up-to n))))
> (foo 3)
5
4
3
2
1
0
```

Of course, this example is rather contrived—mucking with `for` loops like this isn’t useful at all, and nobody would really write `print-up-to` as a macro, anyway—but there is potential for using this technique to do more interesting things.

# Closing thoughts

The system outlined in this blog post is not something I would recommend using in any real macro. It is enormously complicated, requires knowledge well above that of your average working macrologist, and it involves doing rather horrible things to the macro system, things it was undoubtably never designed to do. Still, I believe this blog post is useful, for a few different reasons:

  1. The technology outlined in this post, while perhaps not directly applicable to existing real-world problems, provides a framework for implementing various new kinds of syntax transformations in Racket *without* extending the macro system. It demonstrates the expressive power of the macro system, and it hopefully lays the foundation for a better, more high-level interface for users who wish to define their own languages with custom core forms.

  2. This system provides insight in to the way the Racket macroexpander operates, *in terms of the userspace syntax API*. The canonical existing model of hygienic macroexpansion, in the aforementioned [Bindings as Sets of Scopes][scope-sets] paper, does not explain the workings of internal definition contexts in detail, and it certainly doesn’t explain them in terms that a Racket programmer would already be familiar with. By reencoding those ideas within the macro system itself, an advanced macro writer may be able to more easily connect concepts in the macro system’s implementation to concepts they have already been exposed to.

  3. The capability of the proof-of-concept outlined here demonstrates that the limitation imposed by the existing implementation of the stop list (namely, the way it is implicitly extended with additional identifiers) is essentially artificial, and it can be hacked around with sufficient (albeit significant) effort. This isn’t enormously important, but it is somewhat relevant to a recent debate in [a GitHub issue][gh-issue-2154] about the handling of the `local-expand` stop list.

  4. Finally, for myself as much as anyone else, this implementation records in a concise way (perhaps overly concise at times) the collection of very subtle details I’ve learned over the past six months about how information is preserved and propagated during the expansion process.

This blog post is not for everybody. If you made it to the end, give yourself a pat on the back. If you made it to the end *and* understood everything you read: congratulations, you are a certified expert in Racket macro programming. If not, do not fear, and do not lose hope—I plan for something significantly more mellow next time.

As always, I’d like to give thanks to the people who contributed significantly, if indirectly, to the contents of this blog post, namely [Matthew Flatt][mflatt], [Michael Ballantyne][mballantyne], and [Ryan Culpepper][ryanc]. And finally, for those interested, all of the code in this blog post can be found in a runnable form [in this GitHub gist][expression-expander-source].


[custom-core-forms-1]: /blog/2018/04/15/reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket/
[expression-expander-source]: https://gist.github.com/lexi-lambda/c4f4b91ac9c0a555447d72d02e18be7b
[gh-issue-2154]: https://github.com/racket/racket/issues/2154
[mflatt]: http://www.cs.utah.edu/~mflatt/
[mballantyne]: http://mballantyne.net
[ryanc]: http://www.ccs.neu.edu/home/ryanc/
[scope-sets]: http://www.cs.utah.edu/plt/scope-sets/
[use-site-scopes]: http://www.cs.utah.edu/plt/scope-sets/pattern-macros.html#%28part._use-site%29

    Title: Simple, safe multimethods in Racket
    Date: 2016-02-18T18:48:32
    Tags: racket, macros

Racket ships with `racket/generic`, a system for defining *generic methods*, functions that work differently depending on what sort of value they are supplied. I have made heavy use of this feature in my collections library, and it has worked well for my needs, but that system does have a bit of a limitation: it only supports *single dispatch*. Method implementations may only be chosen based on a single argument, so multiple dispatch is impossible.

<!-- more -->

# Motivating multiple dispatch

What is multiple dispatch and why is it necessary? Well, in most cases, it *isn’t* necessary at all. [It has been shown that multiple dispatch is much rarer than single dispatch in practice.][multiple-dispatch-in-practice] However, when actually needed, having multiple dispatch in the toolbox is a valuable asset.

A classic example of multiple dispatch is multiplication over both scalars and vectors. Ideally, all of the following operations should work:

```
2 × 3 = 6
2 × ⟨3, 4⟩ = ⟨6, 8⟩
⟨3, 4⟩ × 2 = ⟨6, 8⟩
```

In practice, most languages do not support such flexible dispatch rules without fairly complicated branching constructs to handle each permutation of input types. Furthermore, since most languages only support single dispatch (such as most object-oriented languages), it is nearly impossible to add support for a new combination of types to an existing method.

To illustrate the above, even if a language supported operator overloading *and* it included a `Vector` class that overloaded multiplication to properly work with numbers and vectors, it might not implement matrix multiplication. If a user defines a `Matrix` class, they may overload *its* multiplication to support numbers, vectors, and matrices, but it is impossible to extend the multiplication implementation for the `Vector` class. That method is now completely set in stone, unless it is edited directly (and the programmer may not have access to `Vector`’s implementation).

Multiple dispatch solves all of these problems. Rather than specify implementations of functions for singular types, it is possible to specify implementations for sets of types. In the above example, a programmer would be able to define a new function that operates on `Vector` and `Matrix` arguments. Since each definition does not “belong” to any given type, extending this set of operations is trivial.

# Multiple dispatch in Racket

This blog post is somewhat long and technical, so before proceeding any further, I want to show some real code that actually works so you can get a feel for what I’m talking about. As a proof-of-concept, I have created [a very simple implementation of multiple dispatch in Racket][racket-multimethod]. The above example would look like this in Racket using my module:

```racket
#lang racket

(require multimethod)

(provide mul
         (struct-out num)
         (struct-out vec))

(struct num (val))
(struct vec (vals))

(define-generic (mul a b))

(define-instance ((mul num num) x y)
  (num (* (num-val x) (num-val y))))

(define-instance ((mul num vec) n v)
  (vec (map (curry * (num-val n)) (vec-vals v))))

(define-instance ((mul vec num) v n)
  (mul n v))
```

Pardon the somewhat clunky syntax, but the functionality is there. Using the above code works as expected:

```
> (mul (num 2) (num 3))
(num 6)
> (mul (num 2) (vec '(3 4)))
(vec '(6 8))
> (mul (vec '(3 4)) (num 2))
(vec '(6 8))
```

Making the above snippet work is not particularly hard. In fact, it’s likely that most competent Racketeers could do it without much thought. However, there’s a tiny bit more going on behind the scenes than it may seem.

# The problem with multiple dispatch

The single-dispatch design limitation of `racket/generic` comes directly from a desire to avoid what has been described as “spooky action at a distance”, a problem that is prevalent in many systems that support methods with multiple dispatch (aka *multimethods*). Specifically, the issue arises when new method implementations are defined for existing datatypes, which can have far-reaching effects throughout a program because the method table is global state. Both CLOS and Clojure suffer from this shortcoming.

Interestingly, Haskell with multi-parameter typeclasses (a nonstandard but highly useful extension) makes it quite trivial to create constructs similar to multiple dispatch (though the overload resolution is done at compile-time). The similarities are significant: Haskell *also* suffers from the possibility of a certain sort of “spooky action”. However, Haskell’s static typing and resolution allows the compiler to catch these potential issues, known as “orphan instances”, at compile time. Even though Racket does not support the same sort of static typing, the same idea can be used to keep multiple dispatch safe using the macro system.

# Safe, dynamically-typed multiple dispatch

In order to make multiple dispatch safe, we first need to determine exactly what is unsafe. Haskell has rules for determining what constitutes an “orphan instance”, and these rules are equally applicable for determining dangerous multimethod implementations. Specifically, a definition can be considered unsafe if *both* of the following conditions are true:

  1. The multimethod that is being implemented was declared in a different module from the implementation.
  2. *All* of the types used for dispatch in the multimethod instance were declared in a different module from the implementation.

Conversely, a multimethod implementation is safe if *either* of the following conditions are true:

  1. The multimethod that is being implemented is declared in the same module as the implementation.
  2. *Any* of the types used for dispatch in the multimethod instance are declared in the same module as the implementation.

Why do these two rules provide a strong enough guarantee to eliminate the dangers created by global state? Well, to understand that, we need to understand what can go wrong if these rules are ignored.

## Multimethods and dangerous instances

What exactly is this dangerous-sounding “spooky action”, and what causes it? Well, the trouble stems from the side-effectful nature of multimethod instance definitions. Consider the Racket module from earlier, which defines multiplication instances for scalars and vectors:

```racket
(provide mul
         (struct-out num)
         (struct-out vec))

(struct num (val))
(struct vec (vals))

(define-generic (mul a b))

(define-instance ((mul num num) x y)
  (num (* (num-val x) (num-val y))))

(define-instance ((mul num vec) n v)
  (vec (map (curry * (num-val n)) (vec-vals v))))

(define-instance ((mul vec num) v n)
  (mul n v))
```

Note that there is not actually a `(mul vec vec)` implementation. This is intentional: there are *two* ways to take the product of two vectors, so no default implementation is provided. However, it is possible that another module might desire an instance for `mul` that takes the dot product, and the programmer might write the following definition:

```racket
(define-instance ((mul vec vec) x y)
  (num (foldl + 0 (map * (vec-vals x) (vec-vals y)))))
```

However, there is something fishy about the above definition: it doesn’t need to be exported with `provide` to work! Since instances don’t create new bindings, they only add dispatch options, they don’t ever need to `provide` anything. This is problematic, though: it means that a program could continue to happily compile *even if* the module containing the dot product instance was never loaded with `require`, but an attempt to multiply two vectors would fail at runtime, claiming that there was no `(mul vec vec)` implementation. This drastic change of behavior violates Racket programmers’ assumptions about the guarantees made by modules (`require` should not cause any side-effects if the module’s bindings are not used).

Of course, while this seems potentially unexpected, it is workable: just be careful to `require` modules containing instances. Unfortunately, it gets much worse—what if a different library defines *its own* `(mul vec vec)` instance? What if that instance takes the cross product instead? That library may function entirely properly on its own, but when loaded alongside the program that defines a dot product instance, it is impossible to determine which instance should be used where. Because `define-instance` operates by modifying the aforementioned global state, the implementations clash, and the two systems *cannot* continue to operate together as written.

This is pretty bad. Defining extra instances is a reasonable use-case for multiple dispatch, but if these instances can break *third-party code*, how can they be trusted? This sort of problem can make multiple dispatch difficult to reason about and even more difficult to trust.

## What determines safety?

With those problems in mind, we can turn back to the two rules for *safe* multiple dispatch. How do they prevent the above issues? Well, let’s take them one at a time.

Remember that an instance can be unequivocally determined to be safe if either of the two conditions are true, so we can consider them entirely independently. The first one is simple—an instance is safe if the following condition holds:

> The multimethod that is being implemented is declared in the same module as the implementation.

This one is pretty obvious. It is impossible to create a “bad” instance of a method declared in the same module because it is impossible to import the method without also bringing in the instance. Furthermore, a conflicting instance cannot be defined at the place where the types themselves are defined because that would require a circular module dependency, which Racket does not permit.

With the above explanation in mind, the second condition should make sense, too:

> *Any* of the types used for dispatch in the multimethod instance are declared in the same module as the implementation.

The same argument for the first point holds for the second, but with the parties swapped. Again, it is impossible to use the instance without somehow requiring the module that defines the datatype itself, so the instance would always be required, anyway. The most interesting aspect of this condition is that it demonstrates that instances can be defined for existing datatypes (that are defined in other modules) just so long as *at least one* of the datatypes is defined in the same module. This continues to permit the important use-case of extending the interfaces of existing types.

## Encoding the safety rules into Racket’s macro system

In order to keep track of which methods and instances are defined where, I leveraged a technique based on the one [used by Typed Racket to keep track of whether or not a typed identifier is used in a typed or untyped context][typed-racket-scheme2007]. However, instead of using a simple mutable boolean flag, I used a mutable [free identifier set][free-id-set], which keeps track of the identifiers within a given module that should be considered “privileged”.

```racket
#lang racket/base

(require syntax/id-set)

(provide mark-id-as-privileged!
         id-privileged?)

(define privileged-ids (mutable-free-id-set))

(define (mark-id-as-privileged! id)
  (free-id-set-add! privileged-ids id))

(define (id-privileged? id)
  (free-id-set-member? privileged-ids id))
```

Making this work with `define-generic` is obvious: just invoke `mark-id-as-privileged!` on the method name to note that the method is “privileged” in the scope of the current module. Keeping track of privileged structs is similarly straightforward, though it is a little more devious: the `multimethod` module provides a custom `struct` macro that just expands to `struct` from `racket/base`, but adds privilege information.

The `define-instance` macro does all the heavy lifting to ensure that only privileged identifiers can be used in instance definitions. A simple check for the identifier annotations is performed before proceeding with macro expansion:

```racket
(unless (or privileged? (ormap id-privileged? types))
  (assert-privileged-struct! (first types)))
```

When the privilege checks fail, an error is raised:

```racket
(define (assert-privileged-struct! id)
  (unless (id-privileged? id)
    (raise-syntax-error 'define-instance
                        "expected name of struct defined in current module"
                        id)))
```

With the above safeguards in place, the dangerous dot product implementation from above **would not be allowed**. The checks manage to encode both of the safety rules into the macro system such that invalid instances will fail *at compile time*, preventing dangerous uses of multimethods from ever slipping by unnoticed.

## Actually implementing multiple dispatch

The rest of the multimethod implementation is relatively straightforward and is not even particularly robust. If anything, it is the bare minimum of what would be needed to allow the safety mechanisms above to work. Lots of features that would likely be needed in a real implementation are not included, and graceful error handling is largely ignored.

Multimethods themselves are implemented as Racket [transformer bindings][transformer-binding] containing custom data, including a reference to the multimethod’s arity and dispatch table. The custom datatype includes a `prop:procedure` structure type property, which allows such bindings to also function as macros. The macro procedure expands to an operation that looks up the proper instance to use in the multimethod’s dispatch table and invokes it with the supplied arguments.

The relevant code for defining multimethods is reproduced below:

```racket
(begin-for-syntax
  (struct multimethod (arity dispatch-table)
    #:property prop:procedure
    (λ (method stx)
      (syntax-parse stx
        [(method arg ...)
         #'(apply-multimethod method (list arg ...))]
        [method
         #'(λ args (apply-multimethod method args))]))))

(define-syntax define-generic
  (syntax-parser
    [(_ (method:id arg:id ...+))
     (with-syntax ([arity (length (attribute arg))]
                   [dispatch-table (generate-temporary #'method)])
       (mark-id-as-privileged! #'method)
       #'(begin
           (define dispatch-table (make-hash))
           (define-syntax method (multimethod arity #'dispatch-table))))]))
```

The dispatch tables are implemented entirely in terms of Racket’s structure types, so while they can be defined on arbitrary structure types (including ones defined in the Racket standard library), they *cannot* be defined on primitives such as pairs or vectors. Implementations are registered in the dispatch table using the compile-time information associated with structs’ transformer bindings, and the same information is retrieved from struct instances at runtime to look up the proper implementation to call. Notably, this only works if the struct is `#:transparent`, or more generally and accurately, if the calling code has access to the struct’s inspector. All structs defined by the `struct` form from the `multimethod` module are automatically marked as `#:transparent`.

The following code implements defining multimethod instances:

```racket
(begin-for-syntax
  (define (assert-privileged-struct! id)
    (unless (id-privileged? id)
      (raise-syntax-error 'define-instance
                          "expected name of struct defined in current module"
                          id))))

(define-syntax define-instance
  (syntax-parser
    ; standard (define (proc ...) ...) shorthand
    [(_ ((method type:id ...+) . args) body:expr ...+)
     #'(define-instance (method type ...) (λ args body ...))]
    ; full (define proc lambda-expr) notation
    [(_ (method type:id ...+) proc:expr)
     (let* ([multimethod (syntax-local-value #'method)]
            [privileged? (id-privileged? #'method)])
       (unless (or privileged? (ormap id-privileged? (attribute type)))
         (assert-privileged-struct! (first (attribute type))))
       (with-syntax ([dispatch-table (multimethod-dispatch-table multimethod)]
                     [(struct-type-id ...) (map (compose1 first extract-struct-info syntax-local-value)
                                                (attribute type))])
         #'(let ([struct-types (list struct-type-id ...)])
             (hash-set! dispatch-table struct-types proc))))]))
```

The resulting implementation is a useful, if certainly incomplete implementation of multimethods in Racket that does not sacrifice the safety provided by `racket/generic`’s single-dispatch approach.

# Related work, advantages and disadvantages, and areas for future improvement

As previously mentioned, this implementation of multiple dispatch was inspired by the types of APIs offered by CLOS and Clojure while also maintaining the safety of `racket/generic`. The inspiration for the safety rules came from GHC’s detection of orphan instances. Although most of the ideas presented above exist in other places, I am unsure if the concept of safety checking has been used before in any dynamically-typed programming languages.

The primary advantage offered over Racket’s existing generics system is obvious: multiple dispatch. Furthermore, this system can supersede many uses of `racket/generic` simply by dispatching on a single type. However, the current implementation does *not* support all of the features of `racket/generic`, such as supporting non-structure types and allowing fallback implementations. While those are well within the realm of possibility, other things like attaching structure type properties are probably not possible with this approach, so it is unlikely that the existing system could be subsumed by one like this one.

Additionally, this implementation would almost certainly need numerous improvements before being useful to most programmers:

  - **Good error reporting for failure cases.** Right now, even something obvious like calling a method on values that do not implement it simply fails with an error produced by `hash-ref`. In a more interesting sense, using the arity to generate compile-time error messages for `define-instance` would be a nice improvement.

  - **Support for Racket primitive data types.** This might require some cooperation from Racket itself to permit an elegant implementation, but they could also just be special-cased. So long as lookup for primitives was done *after* consulting the main dispatch table, there wouldn’t be any performance hit for non-primitive types.

  - **Option to supply fallback implementations.** This wouldn’t be too hard at all, though it’s questionable whether or not it would be useful without method groupings like `define/generic` provides. There would likely also need to be some sort of way to check if a set of values implements a particular method.

  - **Better cooperation with structure inspectors to alleviate the need for all structures to be transparent.** It’s currently unclear to me how exactly this works and how it *should* work. There might be a better way to do this without mucking with inspectors.

  - **Much more flexible argument lists, including the ability to specify arguments that are not used for dispatch.** This is really a pretty fundamental requirement, but the parsing required was significant enough for me to put it off for this initial prototype.

  - **Scribble forms to document generic methods and their instances.** This is something `racket/generic` *doesn’t* have, and it has suffered for it. It would be very nice to have easy documentation forms for multimethods.

  - **Proper consideration of struct subtyping.** Racket structs support subtyping, which I have not given much thought for this prototype. It is possible that subtyping violates constraints I had assumed would hold, so reviewing the existing code with that context would be useful.

I’m not sure how much effort is involved in most of the above ideas, and in fact I’m not even completely sure how useful this system is to begin with. I have not found myself reaching much for multiple dispatch in my time as a Racket programmer, but that could simply be because it was previously unavailable. It will be interesting to see if that changes now that I have built this system, even if it is a bit rough around the edges.

# Conclusion

Despite the lack of need for multiple dispatch to solve most problems, as indicated by its general lack of support in mainstream programming languages, it’s a nice tool to have in the toolbox, and it *is* asked for in the Racket community from time to time (perhaps due to its familiarity in other parts of the Lisp world). Time will tell if pointing people to something like this will create or stifle interest in multiple dispatch for Racket.

The source for the [`multimethod` package can be found here][racket-multimethod] if you are at all interested in playing with it yourself.

[free-id-set]: http://docs.racket-lang.org/syntax/syntax-helpers.html#%28tech._identifier._set%29
[multiple-dispatch-in-practice]: http://dl.acm.org/citation.cfm?doid=1449764.1449808
[racket-multimethod]: https://github.com/lexi-lambda/racket-multimethod
[transformer-binding]: http://docs.racket-lang.org/guide/proc-macros.html#%28tech._transformer._binding%29
[typed-racket-scheme2007]: http://www.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf

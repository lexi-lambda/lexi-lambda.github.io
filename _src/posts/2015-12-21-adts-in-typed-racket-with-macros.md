    Title: ADTs in Typed Racket with macros
    Date: 2015-12-21T17:57:07
    Tags: racket, typed racket, macros

Macros are one of Racket's flagship features, and its macro system really is state of the art. Of course, it can sometimes be difficult to demonstrate *why* macros are so highly esteemed, in part because it can be hard to find self-contained examples of using macros in practice. Of course, one thing that macros are perfect for is filling a "hole" in the language by introducing a feature a language lacks, and one of those features in Typed Racket is **ADTs**.

<!-- more -->

# Warning: this is not a macro tutorial

First, a disclaimer: this post assumes at least some knowledge of Scheme/Racket macros. Ideally, you would be familiar with Racket itself. But if you aren't, fear not: if you get lost, don't worry. Hold on to the bigger picture, and you'll likely learn more than someone who knows enough to follow all the way through. If you *are* interested in learning about macros, I must recommend Greg Hendershott's [Fear of Macros][fear-of-macros]. It is good. This is not that.

Now, with that out of the way, let's get started.

# What we're building

[Algebraic data types][adts], or *ADTs*, are a staple of the ML family of functional programming languages. I won't go into detail here—I want to focus on the implementation—but they're a very descriptive way of modeling data that encourages designing functions in terms of pattern-matching, something that Racket is already good at.

Racket also already has a facility for creating custom data structures in the form of *structs*, which are extremely flexible, but also a little verbose. Racket structs are more powerful than we need, but that means we can implement our ADTs in terms of Racket's struct system.

With that in mind, what should our syntax look like? Well, let's consider a quintessential example of ADTs: modeling a simple tree. For now, let's just consider a tree of integers. For reference, the Haskell syntax for such a data structure would look like this:

```haskell
data Tree = Empty
          | Leaf Int
          | Node Tree Tree
```

This already demonstrates a few of the core things we'll need to build:

  1. Each ADT has a *data type*, in this case `Tree`. This name only exists in the world of types, it isn't a value.
  2. Each ADT has various *data constructors*, in this case `Leaf` and `Node`.
  3. Each data constructor may accept any number of arguments, each of which have a specific type.
  4. The types that data constructors may accept include the ADT's datatype itself—that is, definitions can be recursive.

Of course, there's one more important feature we're missing: polymorphism. Our definition of a tree is overly-specific, and really, it should be able to hold any kind of data, not just integers. In Haskell, we can do that by adding a type parameter:

```haskell
data Tree a = Empty
            | Leaf a
            | Node (Tree a) (Tree a)
```

With this in mind, we can add a fifth and final point to our list:

  <ol start="5"><li>ADTs must be able to be parametrically polymorphic.</li></ol>

That covers all of our requirements for basic ADTs. Now we're ready to port this idea to Racket.

## Describing ADTs in Racket

How should we take the Haskell syntax for an ADT definition and adapt it to Racket's parenthetical s-expressions? By taking some cues from the Haskell implementation, Typed Racket's type syntax, and Racket's naming conventions, a fairly logical syntax emerges:

```racket
(define-datatype (Tree a)
  Empty
  (Leaf a)
  (Node (Tree a) (Tree a)))
```

This looks pretty good. Just like with the Haskell implementation, `Tree` should only exist at the type level, and `Empty`, `Leaf`, and `Node` should be constructor functions. Our syntax mirrors Racket function application, too—the proper way to create a leaf would be `(Leaf 7)`.

Now that we can create ADT values, how should we extract the values from them? Well, just like in ML-likes, we can use pattern-matching. We don't need to reinvent the wheel for this one; we should be able to just use Racket's `match`[racket] with our datatypes. For example, a function that sums all the values in a tree might look like this:

```racket
(: tree-sum ((Tree Integer) -> Integer))
(define (tree-sum tree)
  (match tree
    [(Empty)    0               ]
    [(Leaf n)   n               ]
    [(Node l r) (+ (tree-sum l)
                   (tree-sum r))]))
```

Given that Racket's `struct` form automatically produces identifiers that cooperate with `match`, this shouldn't be hard at all. And with our syntax settled, we're ready to begin implementation.

# Implementing ADTs as syntax

Now for the fun part. To implement our ADT syntax, we'll employ Racket's industrial-strength macro DSL, [`syntax/parse`][syntax-parse]. The `syntax/parse` library works like the traditional Scheme `syntax-case` on steroids, and one of the most useful features is the ability to define "syntax classes" that encapsulate reusable parsing rules into declarative components.

Since this is not a macro tutorial, the following implementation assumes you already know how to use `syntax/parse`. However, all of the concepts here are well within the reaches of any intermediate macrologist, so don't be intimidated by some of the more complex topics at play.

## Parsing types with a syntax class

To implement ADTs, we're going to want to define exactly one syntax class, a class that describes the grammar for a type. As we've seen, types can be bare identifiers, like `Tree`, or they can be identifiers with parameters, like `(Tree a)`. We'll want to cover both cases.

```racket
(begin-for-syntax
  (define-syntax-class type
    (pattern name:id #:attr [param 1] '())
    (pattern (name:id param ...+))))
```

This syntax class has two rules, one that's a bare identifier, and one that's a list. The ellipsis followed by a plus (`...+`) in the second example means "one or more", so parsing those parameters will automatically be handled for us. In the bare identifier example, we use `#:attr` to give the `param` attribute the default value of an empty list, so this syntax class will actually *normalize* the input we get in addition to actually parsing it.

## A first attempt at `define-datatype`

Now we can move on to actually implementing `define-datatype`. The rules are simple: we need to generate a structure type for each one of the data constructors, and we need to generate a type definition for the parent type itself. This is pretty simple to implement using `syntax-parser`, which actually does the parsing for our macro.

```racket
(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)
     ]))
```

This definition will do all the parsing we need. It parses the entire macro "invocation", ignoring the first datum with `_` (which will just be the identifier `define-datatype`), then expecting a `type-name`, which uses the `type` syntax class we defined above. Next, we expect zero or more `data-constructor`s, which also use the `type` syntax class. That's all we have to do for parsing. We now have all the information we need to actually output the expansion for the macro.

Of course, it won't be that easy: this is the difficult part. The first step is to generate a Racket struct for each data constructor. We can do this pretty easily with some simple use of Racket's syntax templating facility. A naïve attempt would look like this:

```racket
(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)
     #'(begin
         (struct data-constructor.name ([f : data-constructor.param] ...)
         ...))]))
```

This is actually really close to being correct. This will generate a struct definition for each `data-constructor`, where each struct has the name of the data constructor and the same number of fields as arguments provided. The trouble is that in Racket structs, all of the fields have *names*, but in our ADTs, all the fields are anonymous and by-position. Currently, we're just using the same name for *all* the fields, `f`, so if any data constructor has two or more fields, we'll get an error.

Since we don't care about the field names, what we want to do is just generate random names for every field. To do this, we can use a Racket function called `generate-temporary`, which generates random identifiers. Our next attempt might look like this:

```racket
#`(begin
    (struct data-constructor.name
      ([#,(generate-temporary) : data-constructor.param] ...)
    ...))
```

The `#,` lets us "escape" from the template to execute `(generate-temporary)` and interpolate its result into the syntax. Unfortunately, this doesn't work. We *do* generate a random field name, but the ellipsis will re-use the same generated value when it repeats the fields, rendering our whole effort pointless. Instead of relying on the ellipsis magic, we need to generate the struct definitions one at a time, ourselves.

## Procedural macro techniques

Our macro is just a little too complex to shove into a single syntax template, but we don't need to do too much restructuring to fix the issue. What we will do, though, is iterate through our values *manually*, which will allow us to create an entirely new random identifier for each and every field specified. To do this, we'll use a combination of `define/with-syntax` and Racket's list comprehensions, `for/list`.

The `define/with-syntax` form binds values to pattern identifiers, which can be used within syntax patterns just like the ones bound by `syntax-parser`. This will allow us to break up our result into multiple steps. Technically, `define/with-syntax` is not strictly necessary—we could just use `` #` `` and `#,`—but it's cleaner to work with.

We'll start by defining a set of constructor definitions, one per `data-constructor`:

```racket
(define/with-syntax [constructor-definition ...]
  (for/list ([name (in-syntax #'(data-constructor.name ...))]
             [params (in-syntax #'((data-constructor.params ...) ...))])
    ))
```

To fill in the body, we'll need to generate a set of constructor fields, then assemble them into a resulting structure definition. We can do that with a nested loop and `define/with-syntax`:

```racket
(define/with-syntax [constructor-definition ...]
  (for/list ([name (in-syntax #'(data-constructor.name ...))]
             [params (in-syntax #'((data-constructor.params ...) ...))])

    (define/with-syntax [constructor-field ...]
      (for/list ([param (in-syntax params)])
        #`[#,(generate-temporary) : #,param]))

    #`(struct #,name (constructor-field ...))))
```

This may look complicated, but it isn't too bad: we're using `generate-temporary` within our nested loop to generate a new identifier for each field within the struct. Finally, we're assembling them into a struct declaration. (It can be helpful to use the identifier-binding arrows in DrRacket to trace how values flow through the program.) With this in hand, we can implement our overall macro definition:

```racket
(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)

     (define/with-syntax [constructor-definition ...]
       (for/list ([name (in-syntax #'(data-constructor.name ...))]
                  [params (in-syntax #'((data-constructor.param ...) ...))])

         (define/with-syntax [constructor-field ...]
           (for/list ([param (in-syntax params)])
             #`[#,(generate-temporary) : #,param]))

         #`(struct #,name (constructor-field ...))))

     #'(begin
         constructor-definition ...)]))
```

We're almost done—now we just need to implement our overall type, the one defined by `type-name`. This is implemented as a trivial type alias, but we need to ensure that polymorphic types are properly handled. For example, a non-polymorphic type would need to be handled like this:

```racket
(define-type Tree (U Empty Leaf Node))
```

However, a polymorphic type alias would need to include the type parameters in each subtype, like this:

```racket
(define-type (Tree a) (U (Empty a) (Leaf a) (Node a)))
```

Once again, we can handle this with some very simple logic before including it in our template:

```racket
(define/with-syntax [data-type ...]
  (for/list ([name (in-syntax #'(data-constructor.name ...))])
    (if (stx-null? #'(type-name.param ...))
        name
        #`(#,name type-name.param ...))))

#'(define-type type-name (U data-type ...))
```

There's just one more thing to do before we can call this macro finished: we need to ensure that all the type parameters defined by `type-name` are in scope for each data constructor's structure definition. We can do this by making use of `type-name.param` within each produced struct definition, resulting in this:

```racket
#`(struct (type-name.param ...) #,name (constructor-field ...))
```

And we're done! The final macro, now completed, looks like this:

```racket
(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)

     (define/with-syntax [constructor-definition ...]
       (for/list ([name (in-syntax #'(data-constructor.name ...))]
                  [params (in-syntax #'((data-constructor.param ...) ...))])

         (define/with-syntax [constructor-field ...]
           (for/list ([param (in-syntax params)])
             #`[#,(generate-temporary) : #,param]))

         #`(struct (type-name.param ...) #,name (constructor-field ...))))

     (define/with-syntax [data-type ...]
       (for/list ([name (in-syntax #'(data-constructor.name ...))])
         (if (stx-null? #'(type-name.param ...))
             name
             #`(#,name type-name.param ...))))

     #'(begin
         constructor-definition ...
         (define-type type-name (U data-type ...)))]))
```

It's dense, certainly, but it is not as complicated or scary as it might seem. Granted, it might be helpful to add some explanatory comments, or even to pull certain aspects of it apart into separate functions, but I will leave it as a single definition for the sake of this post.

# Using our ADTs

With the macro built, we can now actually use our ADTs using the syntax we described! The following is now *valid code*:

```racket
(define-datatype (Tree a)
  Empty
  (Leaf a)
  (Node (Tree a) (Tree a)))

> (Node (Leaf 3) (Node (Empty) (Leaf 7)))
- : (Node Positive-Byte)
(Node (Leaf 3) (Node (Empty) (Leaf 7)))
```

We can use this to define common data types, such as Haskell's `Maybe`:

```racket
(define-datatype (Maybe a)
  (Just a)
  Nothing)

(: maybe-default (All [a] (Maybe a) a -> a))
(define (maybe-default m v)
  (match m
    [(Just a)  a]
    [(Nothing) v]))

(: maybe-then (All [a] (Maybe a) (a -> (Maybe a)) -> (Maybe a)))
(define (maybe-then m f)
  (match m
    [(Just a)  (f a)]
    [(Nothing) (Nothing)]))
```

And of course, we can also use it to define ADTs that use concrete types rather that type parameters, if we so desire. This implements a small mathematical language, along with a trivial interpreter:

```racket
(define-datatype Expr
  (Value Number)
  (Add Expr Expr)
  (Subtract Expr Expr)
  (Multiply Expr Expr)
  (Divide Expr Expr))

(: evaluate (Expr -> Number))
(define (evaluate e)
  (match e
    [(Value x)      x                            ]
    [(Add a b)      (+ (evaluate a) (evaluate b))]
    [(Subtract a b) (- (evaluate a) (evaluate b))]
    [(Multiply a b) (* (evaluate a) (evaluate b))]
    [(Divide a b)   (/ (evaluate a) (evaluate b))]))

> (evaluate (Add (Value 1)
                 (Multiply (Divide (Value 1) (Value 2))
                           (Value 7))))
4 1/2
```

There's all the power of ADTs, right in Racket, all implemented in 30 lines of code.

# Conclusions and credit

This isn't the simplest macro to create, nor is it the most complex. The code examples might not even make much sense until you try it out yourself. Macros, like any difficult concept, are not always easy to pick up, but they certainly *are* powerful. The ability to extend the language in such a way, in the matter of minutes, is unparalleled in languages other than Lisp.

This is, of course, a blessing and a curse. Lisps reject some of the syntactic landmarks that often aid in readability for the power to abstract programs into their bare components. In the end, is this uniform conciseness more or less readable? That's an incredibly subjective question, one that has prompted powerfully impassioned discussions, and I will not attempt to argue one way or the other here.

That said, I think it's pretty cool.

Finally, I must give credit where credit is due. Thanks to [Andrew M. Kent][andmkent] for the creation of the [datatype][racket-datatype] package, which served as the inspiration for this blog post. Also thanks to [Ryan Culpepper][ryanc] and [Matthias Felleisen][matthias] for their work on creating `syntax/parse`, which is truly a marvelous tool for exploring the world of macros. And, of course, a big thanks to [Matthew Flatt][mflatt] for his implementation of hygiene in Racket, as well as much of the rest of Racket itself. Not to mention the entire legacy of those who formulated the foundations of the Scheme macro system and created the framework for all of this to be possible so many decades later.

Truly, working in Racket feels like standing on the shoulders of giants. If you're intrigued, give it a shot. It's a fun feeling.

[adts]: https://en.wikipedia.org/wiki/Algebraic_data_type
[andmkent]: http://andmkent.com
[fear-of-macros]: http://www.greghendershott.com/fear-of-macros/
[matthias]: http://www.ccs.neu.edu/home/matthias/
[mflatt]: http://www.cs.utah.edu/~mflatt/
[racket-datatype]: https://github.com/andmkent/datatype
[ryanc]: http://www.ccs.neu.edu/home/ryanc/
[syntax-parse]: http://docs.racket-lang.org/syntax/stxparse.html

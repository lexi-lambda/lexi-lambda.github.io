    Title: Types as axioms, or: playing god with static types
    Date: 2020-08-13T13:51:57
    Tags: types, functional programming, haskell, typescript

Just what exactly *is* a type?

A common perspective is that types are *restrictions*. Static types restrict the set of values a variable may contain, capturing some subset of the space of “all possible values.” Under this worldview, a typechecker is sort of like an oracle, predicting which values will end up where when the program runs and making sure they satisfy the constraints the programmer wrote down in the type annotations. Of course, the typechecker can’t *really* predict the future, so when the typechecker gets it wrong—it can’t “figure out” what a value will be—static types can feel like self-inflicted shackles.

But that is not the *only* perspective. There is another way—a way that puts you, the programmer, back in the driver’s seat. You make the rules, you call the shots, you set the objectives. You need not be limited any longer by what the designers of your programming language decided the typechecker can and cannot prove. You do not serve the typechecker; the typechecker serves *you.*

…no, I’m not trying to sell you a dubious self-help book for programmers who feel like they’ve lost control of their lives. If the above sounds too good to be true, well… I won’t pretend it’s all actually as easy as I make it sound. Nevertheless, it’s well within the reach of the working programmer, and most remarkably, all it takes is a change in perspective.

# Seeing the types half-empty

Let’s talk a little about TypeScript.

TypeScript is a *gradually-typed* language, which means it’s possible to mix statically- and dynamically-typed code. The original intended use case of gradual typing was to *gradually* add static types to an existing dynamically-typed codebase, which imposes some interesting design constraints. For one, a valid JavaScript program must also be a valid TypeScript program; for another, TypeScript must be accommodating of traditional JavaScript idioms.

Gradually typed languages like TypeScript are particularly good illustrations of the way type annotations can be viewed as constraints. A function with no explicit type declarations[^1] can accept *any* JavaScript value, so adding a type annotation fundamentally restricts the set of legal values.

Furthermore, languages like TypeScript tend to have subtyping. This makes it easy to classify certain types as “more restrictive” than others. For example, a type like `string | number` clearly includes more values than just `number`, so `number` is a more restrictive type—a *subtype*.

An exceptionally concrete way to illustrate this “types are restrictions” mentality is to write a function with an unnecessarily specific type. Here’s a TypeScript function that returns the first element in an array of numbers:

```typescript
function getFirst(arr: number[]): number | undefined {
  return arr[0];
}
```

If we ignore the type annotations and consider only the dynamic semantics of JavaScript, this function would work perfectly well given a list of strings. However, if we write `getFirst(["hello", "world"])`, the typechecker will complain. In this example, the restriction is thoroughly self-imposed—it would be easy to give this function a more generic type—but it’s not always that easy. For example, suppose we wrote a function where the return type depends upon the type of the argument:

```typescript
function emptyLike(val: number | string): number | string {
  if (typeof val === "number") {
    return 0;
  } else {
    return "";
  }
}
```

Now if we write `emptyLike(42) * 10`, the typechecker will once again complain, claiming the result might be a string—it can’t “figure out” that when we pass a number, we always get a number back.

When type systems are approached from this perspective, the result is often frustration. The programmer knows that the equivalent untyped JavaScript is perfectly well-behaved, so the typechecker comes off as being the highly unfortunate combination of stubborn yet dim-witted. What’s more, the programmer likely has little mental model of the typechecker’s internal operation, so when types like the above are inferred (not explicitly written), it can be unclear what solutions exist to make the error go away.

At this point, the programmer may give up. “Stupid typechecker,” they grumble, changing the return type of `emptyLike` to `any`. “If it can’t even figure this out, can it *really* be all that useful?”

Sadly, this relationship with the typechecker is all too common, and gradually-typed languages in particular tend to create a vicious cycle of frustration:

  * Gradual type systems are intentionally designed to “just work” on idiomatic code as much as possible, so programmers may not think much about the types except when they get type errors.

  * Furthermore, many programmers using gradually-typed languages are already adept at programming in the underlying dynamically-typed language, so they have working mental models of program operation in terms of the dynamic semantics alone. They are much less likely to develop a rich mental model of the static semantics of the type system because they are used to reasoning without one.

  * Gradually typed languages must support idioms from their dynamically-typed heritage, so they often include ad-hoc special cases (such as, for example, special treatment of `typeof` checks) that obscure the rules the typechecker follows and make them seem semi-magical.

  * Builtin types are deeply blessed in the type system, strongly encouraging programmers to embrace their full flexibility, but leaving little recourse when they run up against their limits.

  * All this frustration breeds a readiness to override the typechecker using casts or `any`, which ultimately creates a self-fulfilling prophecy in which the typechecker rarely catches any interesting mistakes because it has been so routinely disabled.

The end result of all of this is a defeatist attitude that views the typechecker as a minor tooling convenience at best (i.e. a fancy autocomplete provider) or an active impediment at worst. Who can really blame them? The type system has (unintentionally of course) been designed in such a way so as to lead them into this dead end. The public perception of type systems settles into that of a strikingly literal nitpicker we endure rather than as a tool we actively leverage.

# Taking back types

After everything I said above, it may be hard to imagine seeing types any other way. Indeed, through the lens of TypeScript, the “types are restrictions” mentality is incredibly natural, so much so that it seems self-evident. But let’s move away from TypeScript for a moment and focus on a different language, Haskell, which encourages a somewhat different perspective. If you aren’t familiar with Haskell, that’s alright—I’m going to try to keep the examples in this blog post as accessible as possible whether you’ve written any Haskell or not.

Though Haskell and TypeScript are both statically-typed—and both of their type systems are fairly sophisticated—Haskell’s type system is almost completely different philosophically:

  * Haskell does not have subtyping,[^2] which means that every value belongs to exactly one type.

  * While JavaScript is built around a small handful of flexible builtin datatypes (booleans, numbers, strings, arrays, and objects), Haskell has essentially no blessed, built-in datatypes other than numbers. Key types such as booleans, lists, and tuples are ordinary datatypes defined in the standard library, no different from types users could define.[^3]

  * In particular, Haskell is built around the idea that datatypes can be defined with multiple *cases*, and branching is done via pattern-matching (more on this shortly).

Let’s look at a basic Haskell datatype declaration. Suppose we want to define a type that represents a season:

```haskell
data Season = Spring | Summer | Fall | Winter
```

If you are familiar with TypeScript, this may look rather similar to a union type; if you’re familiar with a C-family language, this may remind you more of an enum. Both are on the right track: this defines a new type named `Season` with four possible values, `Spring`, `Summer`, `Fall`, and `Winter`.

But what exactly *are* those values?

  * In TypeScript, we’d represent this type with a union of strings, like this:

    ```ts
    type Season = "spring" | "summer" | "fall" | "winter";
    ```

    Here, `Season` is a type that can be one of those four strings, but nothing else.

  * In C, we’d represent this type with an enum, like this:

    ```c
    enum season { SPRING, SUMMER, FALL, WINTER };
    ```

    Here, `SPRING`, `SUMMER`, `FALL`, and `WINTER` are essentially defined to be global aliases for the integers `0`, `1`, `2`, and `3`, and the type `enum season` is essentially an alias for `int`.

So in TypeScript, the values are strings, and in C, the values are numbers. What are they in Haskell? Well… they simply *are*.

The Haskell declaration invents four completely new constants out of thin air, `Spring`, `Summer`, `Fall`, and `Winter`. They aren’t aliases for numbers, nor are they symbols or strings. The compiler doesn’t expose anything about how it chooses to represent these values at runtime; that’s an implementation detail. In Haskell, `Spring` is now a value *distinct from all other values*, even if someone in a different module were to also use the name `Spring`. Haskell type declarations let us play god, creating something from nothing.

Since these values are totally unique, abstract constants, what can we actually do with them? The answer is one thing and *exactly* one thing: we can branch on them. For example, we can write a function that takes a `Season` as an argument and returns whether or not Christmas occurs during it:

```haskell
containsChristmas :: Season -> Bool
containsChristmas season = case season of
  Spring -> False
  Summer -> True  -- southern hemisphere
  Fall   -> False
  Winter -> True  -- northern hemisphere
```

`case` expressions are, to a first approximation, a lot like C-style `switch` statements (though they can do a lot more than this simple example suggests). Using `case`, we can also define conversions from our totally unique `Season` constants to other types, if we want:

```haskell
seasonToString :: Season -> String
seasonToString season = case season of
  Spring -> "spring"
  Summer -> "summer"
  Fall   -> "fall"
  Winter -> "winter"
```

We can also go the other way around, converting a `String` to a `Season`, but if we try, we run into a problem: what do we return for a string like, say, `"cheesecake"`? In other languages, we might throw an error or return `null`, but Haskell does not have `null`, and errors are generally reserved for truly catastrophic failures. What can we do instead?

A particularly naïve solution would be to create a type called `MaybeASeason` that has two cases—it can be a valid `Season`, or it can be `NotASeason`:

```haskell
data MaybeASeason = IsASeason Season | NotASeason

stringToSeason :: String -> MaybeASeason
stringToSeason seasonString = case seasonString of
  "spring" -> IsASeason Spring
  "summer" -> IsASeason Summer
  "fall"   -> IsASeason Fall
  "winter" -> IsASeason Winter
  _        -> NotASeason
```

This shows a feature of Haskell datatypes that C-style enums do *not* have: they aren’t just constants, they can contain other values. A `MaybeASeason` can be one of five different values: `IsASeason Spring`, `IsASeason Summer`, `IsASeason Fall`, `IsASeason Winter`, or `NotASeason`.

In TypeScript, we’d write `MaybeASeason` more like this:

```ts
type MaybeASeason = Season | "not-a-season";
```

This is kind of nice, because we don’t have to wrap all our `Season` values with `IsASeason` like we have to do in Haskell. But remember that Haskell doesn’t have subtyping—every value must belong to exactly one type—so the Haskell code needs the `IsASeason` wrapper to distinguish the value as a `MaybeASeason` rather than a `Season`.

Now, you may rightly point out that having to invent a type like `MaybeASeason` every time we need to create a variant of a type with a failure case is absurd, so fortunately we can define a type like `MaybeASeason` that works for *any* underlying type. In Haskell, it looks like this:

```haskell
data Maybe a = Just a | Nothing
```

This defines a generic type, where the `a` in `Maybe a` is a stand-in for some other type, much like the `T` in `Array<T>` in other languages. We can change our `stringToSeason` function to use `Maybe`:

```haskell
stringToSeason :: String -> Maybe Season
stringToSeason seasonString = case seasonString of
  "spring" -> Just Spring
  "summer" -> Just Summer
  "fall"   -> Just Fall
  "winter" -> Just Winter
  _        -> Nothing
```

`Maybe` gets us something a lot like nullable types, but it isn’t built into the type system, it’s just an ordinary type defined in the standard library.

## Positive versus negative space

At this point, you may be wondering to yourself why I am talking about all of this, seeing as everything in the previous section is information you could find in a basic Haskell tutorial. But the point of this blog post is not to teach you Haskell, it’s to focus on a particular philosophical approach to modeling data.

In TypeScript, when we write a type declaration like

```ts
type Season = "summer" | "spring" | "fall" | "winter";
```

we are defining a type that can be one of those four strings *and nothing else*. All the other strings that *aren’t* one of those four make up `Season`’s “negative space”—values that exist, but that we have intentionally excluded. In contrast, the Haskell type does not really have any “negative space” because we pulled four new values out of thin air.

Of course, I suspect you don’t really buy this argument. What makes a string like `"cheesecake"` “negative space” in TypeScript but not in Haskell? Well… nothing, really. The distinction I’m drawing here doesn’t really exist, it’s just a different perspective, and arguably a totally contrived and arbitrary one. But now that I’ve explained the premise and set up some context, let me provide a more compelling example.

Suppose you are writing a TypeScript program, and you want a function that only accepts *non-empty* arrays. What can you do? Your first instinct is that you need a way to somehow further restrict the function’s input type to exclude empty arrays. And indeed, there *is* a trick for doing that:

```ts
type NonEmptyArray<T> = [T, ...T[]];
```

Great! But what if the constraint was more complicated: what if you needed an array containing an even number of elements? Unfortunately, there isn’t really a trick for that one. At this point, you might start wishing the type system had support for something really fancy, like refinement types, so you could write something like this:

```ts
type EvenArray<T> = T[] satisfies (arr => arr.length % 2 === 0);
```

But TypeScript doesn’t support anything like that, so for now you’re stuck. You need a way to restrict the function’s domain in a way the type system does not have any special support for, so your conclusion might be “I guess the type system just can’t do this.” People tend to call this “running up against the limits of the type system.”

But what if we took a different perspective? Recall that in Haskell, lists aren’t built-in datatypes, they’re just ordinary datatypes defined in the standard library:[^4]

```haskell
data List a = Nil | Cons a (List a)
```

This type might be a bit confusing at first if you have not written any Haskell, since it’s *recursive*. All of these are valid values of type `List Int`:

  * `Nil`
  * `Cons 1 Nil`
  * `Cons 1 (Cons 2 Nil)`
  * `Cons 1 (Cons 2 (Cons 3 Nil))`

The recursive nature of `Cons` is what gives our user-defined datatype the ability to hold any number of values: we can have any number of nested `Cons`es we want before we terminate the list with a final `Nil`.

If we wanted to define an `EvenList` type in Haskell, we might end up thinking along the same lines we did before, that we need some fancy type system extension so we can restrict `List` to exclude lists with odd numbers of elements. But that’s focusing on the negative space of things we want to exclude… what if instead, we focused on the *positive* space of things we want to *include?*

What do I mean by that? Well, we could define an entirely new type that’s just like `List`, but we make it *impossible* to ever include an odd number of elements:

```haskell
data EvenList a = EvenNil | EvenCons a a (EvenList a)
```

Here are some valid values of type `EvenList Int`:

  * `EvenNil`
  * `EvenCons 1 2 EvenNil`
  * `EvenCons 1 2 (EvenCons 3 4 EvenNil)`

Lo and behold, a datatype that can only ever include even numbers of elements!

Now, at this point you might realize that this is kind of silly. We don’t need to invent an entirely new datatype for this! We could just create a list of pairs:

```haskell
type EvenList a = List (a, a)
```

Now values like `Cons (1, 2) (Cons (3, 4) Nil)` would be valid values of type `EvenList Int`, and we wouldn’t have to reinvent lists. But again, this is an approach based on thinking not on which values we want to exclude, but rather how to structure our data such that those illegal values aren’t even *constructible.*

**This is the essence of the Haskeller’s mantra, “Make illegal states unrepresentable,”** and sadly it is often misinterpreted. It’s much easier to think “hm, I want to make these states illegal, how can I add some post-hoc restrictions to rule them out?” And indeed, this is why refinement types really *are* awesome, and when they’re available, by all means use them! But checking totally arbitrary properties at the type level is not tractable in general, and sometimes you need to think a little more outside the box.

## Types as axiom schemas

So far in this blog post, I’ve repeatedly touched upon a handful of different ideas in a few different ways:

  * Instead of thinking about how to *restrict*, it can be useful to think about how to *correctly construct*.

  * In Haskell, datatype declarations invent new values out of thin air.

  * We can represent a *lot* of different data structures using the incredibly simple framework of “datatypes with several possibilities.”

Independently, those ideas might not seem deeply related, but in fact, they’re all essential to the Haskell school of data modeling. I want to now explore how we can unify them into a single framework that makes this seem less magical and more like an iterative design process.

In Haskell, when you define a datatype, you’re really defining a new, self-contained set of *axioms* and *inference rules.* That is rather abstract, so let’s make it more concrete. Consider the `List` type again:

```haskell
data List a = Nil | Cons a (List a)
```

Viewed as an axiom schema, this type has one axiom and one inference rule:

  * The empty list is a list.

  * If you have a list, and you add an element to the beginning, the result is also a list.

The axiom is `Nil`, and the inference rule is `Cons`. Every list[^5] is constructed by starting with the axiom, `Nil`, followed by some number of applications of the inference rule, `Cons`.

We can take a similar approach when designing the `EvenList` type. The axiom is the same:

  * The empty list is a list with an even number of elements.

But our inference rule must preserve the invariant that the list always contains an even number of elements. We can do this by always adding two elements at a time:

  * If you have a list with an even number of elements, and you add two elements to the beginning, the result is also a list with an even number of elements.

This corresponds precisely to our `EvenList` declaration:

```haskell
data EvenList a = EvenNil | EvenCons a a (EvenList a)
```

We can also go through this same reasoning process to come up with a type that represents non-empty lists. That type has just one inference rule:

  * If you have a list, and you add an element to the beginning, the result is a non-empty list.

That inference rule corresponds to the following datatype:

```haskell
data NonEmptyList a = NonEmptyCons a (List a)
```

Of course, it’s possible to do this with much more than just lists. A particularly classic example is the constructive definition of natural numbers:

  * Zero is a natural number.
  * If you have a natural number, its successor (i.e. that number plus one) is also a natural number.

These are two of the [Peano axioms][wiki:peano], which can be represented in Haskell as the following datatype:

```haskell
data Natural = Zero | Succ Natural
```

Using this type, `Zero` represents 0, `Succ Zero` represents 1, `Succ (Succ Zero)` represents 2, and so on. Just as `EvenList` allowed us to represent any list with an even number of elements but made other values impossible to even express, this `Natural` type allows us to represent all natural numbers, while other numbers (such as, for example, negative integers) are impossible to express.

Now, of course, all this hinges on our interpretation of the values we’ve invented! We have chosen to interpret `Zero` as `0` and `Succ n` as `n + 1`, but that interpretation is not inherent to `Natural`’s definition—it’s all in our heads! We could choose to interpret `Succ n` as `n - 1` instead, in which case we would only be able to represent non-positive integers, or we could interpret `Zero` as `1` and `Succ n` as `n * 2`, in which case we could only represent powers of two.

I find that people sometimes find this approach troubling, or at least counterintuitive. Is `Succ (Succ Zero)` *really* 2? It certainly doesn’t look like a number we’re used to writing. When someone thinks “I need a datatype for a number greater than or equal to zero,” they’re going to reach for the type in their programming language called `number` or `int`, not think to invent a recursive datatype. And admittedly, the `Natural` type defined here is not very practical: it’s an incredibly inefficient representation of natural numbers.

But in less contrived situations, this approach *is* practical, and in fact it’s highly useful! The quibble that an `EvenList Int` isn’t “really” a `List Int` is rather meaningless, seeing as our definition of `List` was just as arbitrary. A great deal of our jobs as programmers is imbuing arbitrary symbols with meaning; at some point someone decided that the number 65 would correspond to the capital letter A, and it was no less arbitrary then.

So when you have a property you want to capture in your types, take a step back and think about it for a little bit. Is there a way you can structure your data so that, no matter how you build it, the result is always a valid value? In other words, don’t try to add post-hoc restrictions to exclude bad values, **make your datatypes correct by construction**.

# “But what if I don’t write Haskell?” And other closing thoughts

I write Haskell for a living, and I wrote this blog post with both my coworkers and the broader Haskell community in mind, but if I had *only* written it with those people in mind, it wouldn’t make sense to have spent so much time explaining basic Haskell. These techniques can be used in almost any statically typed programming language, though it’s certainly easier in some than others.

I don’t want people to come away from this blog post with an impression that I think TypeScript is a bad language, or that I’m claiming Haskell can do things TypeScript can’t. In fact, TypeScript *can* do all the things I’ve talked about in this blog post! As proof, here are TypeScript definitions of both `EvenList` and `Natural`:

```typescript
type EvenList<T> = [] | [T, T, EvenList<T>];
type Natural = "zero" | { succ: Natural };
```

If anything, **the real point of this blog post is that a type system does not have a well-defined list of things it “can prove” and “can’t prove.”** Languages like TypeScript don’t really encourage this approach to data modeling, where you restructure your values in a certain way so as to guarantee certain properties. Rather, they prefer to add increasingly sophisticated constraints and type system features that can capture the properties people want to capture without having to change their data representation.

And in general, *that’s great!*

Being able to reuse the same data representation is *hugely* beneficial. Functions like `map` and `filter` already exist for ordinary lists/arrays, but a home-grown `EvenList` type needs its own versions. Passing an `EvenList` to a function that expects a list requires explicitly converting between the two. All these things have both code complexity and performance costs, and type system features that make these issues just invisibly disappear are *obviously* a good thing.

But the danger of treating the type system this way is that it means you may find yourself unsure what to do when suddenly you have a new requirement that the type system doesn’t provide built-in support for. What then? Do you start punching holes through your type system? The more you do that, the less useful the type system becomes: type systems are great at detecting how changes in one part of a codebase can impact seemingly-unrelated areas in surprising ways, but every unsafe cast or use of `any` is a hard stop, a point past which the typechecker cannot propagate information. Do that once or twice in a leaf function, it’s okay, but do that even just a half dozen times in your application’s connective tissue, and your type system might not be able to catch those things anymore.

Even if it isn’t a technique you use every day, it’s worth getting comfortable tweaking your data representation to preserve those guarantees. It’s a magical experience having the typechecker teach you things about your domain you hadn’t even considered simply because you got a type error and started thinking through why. Yes, it’s extra work, but trust me: it’s a lot more pleasant to work for your typechecker when you know exactly how much your typechecker is working for you.


[^1]: Sort of. TypeScript will try to infer type annotations based on how variables and functions are used, but by default, it falls back on the dynamic, unchecked `any` type if it can’t find a solution that makes the program typecheck. That behavior can be changed via a configuration option, but that isn’t relevant here: I’m just trying to illustrate a perspective, not make any kind of value judgment about TypeScript specifically.

[^2]: Sort of. Haskell does have a limited notion of subtyping when polymorphism is involved; for example, the type `forall a. a -> a` is a subtype of the type `Int -> Int`. But Haskell does not have anything resembling inheritance (e.g. there is no common `Number` supertype that includes both `Int` and `Double`) nor does it have untagged unions (e.g. the argument to a function cannot be something like `Int | String`, you must define a wrapper type like `data IntOrString = AnInt Int | AString String`).

[^3]: Lists, tuples, and strings do technically have special *syntax*, which is built into the compiler, but there is truly nothing special about their semantics. They would work exactly the same way without the syntax, the code would just look less pretty.

[^4]: Haskell programmers will notice that this is not actually the definition of the list type, since the real list type uses special syntax, but I wanted to keep things as simple as possible for this blog post.

[^5]: Ignoring infinite lists, but the fact that infinite lists are representable in Haskell is outside the scope of this blog post.


[wiki:peano]: https://en.wikipedia.org/wiki/Peano_axioms

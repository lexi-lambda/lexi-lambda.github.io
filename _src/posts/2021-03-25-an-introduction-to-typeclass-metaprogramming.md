    Title: An introduction to typeclass metaprogramming
    Date: 2021-03-25T00:00:00
    Tags: haskell, types, functional programming

*Typeclass metaprogramming* is a powerful technique available to Haskell programmers to automatically generate term-level code from static type information. It has been used to great effect in several popular Haskell libraries (such as the [servant][hackage:servant] ecosystem), and it is the core mechanism used to implement generic programming via [GHC generics][hackage:base:GHC.Generics]. Despite this, remarkably little material exists that explains the technique, relegating it to folk knowledge known only to advanced Haskell programmers.

This blog post attempts to remedy that by providing an overview of the foundational concepts behind typeclass metaprogramming. It does *not* attempt to be a complete guide to type-level programming in Haskell—such a task could easily fill a book—but it does provide explanations and illustrations of the most essential components. This is also *not* a blog post for Haskell beginners—familiarity with the essentials of the Haskell type system and several common GHC extensions is assumed—but it does not assume any prior knowledge of type-level programming.

<!-- more -->

# Part 1: Basic building blocks

Typeclass metaprogramming is a big subject, which makes covering it in a blog post tricky. To break it into more manageable chunks, this post is divided into several parts, each of which introduces new type system features or type-level programming techniques, then presents an example of how they can be applied.

To start, we’ll cover the absolute foundations of typeclass metaprogramming.

## Typeclasses as functions from types to terms

As its name implies, typeclass metaprogramming (henceforth TMP[^1]) centers around Haskell’s typeclass construct. Traditionally, typeclasses are viewed as a mechanism for principled operator overloading; for example, they underpin Haskell’s polymorphic `==` operator via the `Eq` class. Though that is often the most useful way to think about typeclasses, TMP encourages a different perspective: **typeclasses are functions from types to (runtime) terms**.

What does that mean? Let’s illustrate with an example. Suppose we define a typeclass called `TypeOf`:

```haskell
class TypeOf a where
  typeOf :: a -> String
```

The idea is that this typeclass will accept some value and return the name of its type as a string. To illustrate, here are a couple potential instances:

```haskell
instance TypeOf Bool where
  typeOf _ = "Bool"

instance TypeOf Char where
  typeOf _ = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf (a, b) = "(" ++ typeOf a ++ ", " ++ typeOf b ++ ")"
```

Given these instances, we can observe that they do what we expect in GHCi:

```haskell
ghci> typeOf (True, 'a')
"(Bool, Char)"
```

Note that both the `TypeOf Bool` and `TypeOf Char` instances ignore the argument to `typeOf` altogether. This makes sense, as the whole point of the `TypeOf` class is to get access to *type* information, which is the same regardless of which value is provided. To make this more explicit, we can take advantage of some GHC extensions to eliminate the value-level argument altogether:

```haskell
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

class TypeOf a where
  typeOf :: String
```

This typeclass definition is a little unusual, as the type parameter `a` doesn’t appear anywhere in the body. To understand what it means, recall that the type of each method of a typeclass is implicitly extended with the typeclass’s constraint. For example, in the definition

```haskell
class Show a where
  show :: a -> String
```

the full type of the `show` method is implicitly extended with a `Show a` constraint to yield:

```haskell
show :: Show a => a -> String
```

Furthermore, if we write `forall`s explicitly, each typeclass method is also implicitly quantified over the class’s type parameters, which makes the following the *full* type of `show`:

```haskell
show :: forall a. Show a => a -> String
```

In the same vein, we can write out the full type of `typeOf`, as given by our new definition of `TypeOf`:

```haskell
typeOf :: forall a. TypeOf a => String
```

This type is still unusual, as the `a` type parameter doesn’t appear anywhere to the right of the `=>` arrow. This makes the type parameter trivially *ambiguous*, which is to say it’s impossible for GHC to infer what `a` should be at any call site. Fortunately, [we can use `TypeApplications`][ghc:type-applications] to pass a type for `a` directly, as we can see in the updated definition of `TypeOf (a, b)`:

```haskell
instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"
```

Once again, we can test out our new definitions in GHCi:

```haskell
ghci> typeOf @Bool
"Bool"
ghci> typeOf @(Bool, Char)
"(Bool, Char)"
```

This illustrates very succinctly how typeclasses can be seen as functions from types to terms. Our `typeOf` function is, quite literally, a function that accepts a single type as an argument and returns a term-level `String`. Of course, the `TypeOf` typeclass is not a particularly *useful* example of such a function, but it demonstrates how easy it is to construct.

## Type-level interpreters

One important consequence of eliminating the value-level argument of `typeOf` is that there is no need for its argument type to actually be *inhabited*. For example, consider the `TypeOf` instance on `Void` from `Data.Void`:

```haskell
instance TypeOf Void where
  typeOf = "Void"
```

This above instance is no different from the ones on `Bool` and `Char` even though `Void` is a completely uninhabited type. This is an important point: as we delve into type-level programming, it’s important to keep in mind that the language of types is mostly blind to the term-level meaning of those types. Although we usually write typeclasses that operate on values, this is not at all essential. This turns out to be quite important in practice, even in something as simple as the definition of `TypeOf` on lists:

```haskell
instance TypeOf a => TypeOf [a] where
  typeOf = "[" ++ typeOf @a ++ "]"
```

If `typeOf` required a value-level argument, not just a type, our instance above would be in a pickle when given the empty list, since it would have no value of type `a` to recursively apply `typeOf` to. But since `typeOf` only accepts a type-level argument, the term-level meaning of the list type poses no obstacle.

A perhaps unintuitive consequence of this property is that we can use typeclasses to write interesting functions on types even if none of the types are inhabited at all. For example, consider the following pair of type definitions:

```haskell
data Z
data S a
```

It is impossible to construct any values of these types, but we can nevertheless use them to construct natural numbers at the type level:

  * `Z` is a type that represents 0.

  * `S Z` is a type that represents 1.

  * `S (S Z)` is a type that represents 2.

And so on. These types might not seem very useful, since they aren’t inhabited by any values, but remarkably, we can still use a typeclass to distinguish them and convert them to term-level values:

```haskell
import Numeric.Natural

class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + reifyNat @a
```

As its name implies, `reifyNat` reifies a type-level natural number encoded using our datatypes above into a term-level `Natural` value:

```haskell
ghci> reifyNat @Z
0
ghci> reifyNat @(S Z)
1
ghci> reifyNat @(S (S Z))
2
```

One way to think about `reifyNat` is as an *interpreter* of a type-level language. In this case, the type-level language is very simple, only capturing natural numbers, but in general, it could be arbitrarily complex—and typeclasses can be used to give it a useful meaning, even if it has no term-level representation.

## Overlapping instances

Generally, typeclass instances aren’t supposed to overlap. That is, if you write an instance for `Show (Maybe a)`, you aren’t supposed to *also* write an instance for `Show (Maybe Bool)`, since it isn’t clear whether `show (Just True)` should use the first instance or the second. For that reason, by default, GHC rejects any form of instance overlap as soon as it detects it.

Usually, this is the right behavior. Due to the way Haskell’s typeclass system is designed to preserve coherency—that is, the same combination of type arguments always selects the same instance—overlapping instances can be unintuitive or even cause nonsensical behavior if orphan instances are defined. However, when doing TMP, it’s useful to make exceptions to that rule of thumb, so GHC provides the option to explicitly opt-in to overlapping instances.

As a simple example, suppose we wanted to write a typeclass that checks whether a given type is `()` or not:

```haskell
class IsUnit a where
  isUnit :: Bool
```

If we were to write an ordinary, value-level function, we could write something like this pseudo-Haskell:

```haskell
-- not actually valid Haskell, just an example
isUnit :: * -> Bool
isUnit () = True
isUnit _  = False
```

But if we try to translate this to typeclass instances, we’ll get a problem:

```haskell
instance IsUnit () where
  isUnit = True

instance IsUnit a where
  isUnit = False
```

The problem is that a function definition has a closed set of clauses matched from top to bottom, but typeclass instances are open and unordered.[^2] This means GHC will complain about instance overlap if we try to evaluate `isUnit @()`:

```
ghci> isUnit @()

error:
    • Overlapping instances for IsUnit ()
        arising from a use of ‘isUnit’
      Matching instances:
        instance IsUnit a
        instance IsUnit ()
```

To fix this, we have to explicitly mark `IsUnit ()` as overlapping:

```haskell
instance {-# OVERLAPPING #-} IsUnit () where
  isUnit = True
```

Now GHC accepts the expression without complaint:

```haskell
ghci> isUnit @()
True
```

What does the `{-# OVERLAPPING #-}` pragma do, exactly? The gory details are [spelled out in the GHC User’s Guide][ghc:overlapping-instances], but the simple explanation is that `{-# OVERLAPPING #-}` relaxes the overlap checker as long as the instance is *strictly more specific* than the instance(s) it overlaps with. In this case, that is true: `IsUnit ()` is trivially more specific than `IsUnit a`, since the former only matches `()` while the latter matches anything at all. That means our overlap is well-formed, and instance resolution should behave the way we’d like.

Overlapping instances are a useful tool when performing TMP, as they make it possible to write piecewise functions on types in the same way it’s possible to write piecewise functions on terms. However, they must still be used with care, as without understanding how they work, they can produce unintuitive results. For an example of how things can go wrong, consider the following definition:

```haskell
guardUnit :: forall a. a -> Either String a
guardUnit x = case isUnit @a of
  True  -> Left "unit is not allowed"
  False -> Right x
```

The idea is that this function uses `isUnit` to detect if its argument is of type `()`, and if it is, it returns an error. However, even though we marked `IsUnit ()` overlapping, we still get an overlapping instance error:

```
error:
    • Overlapping instances for IsUnit a arising from a use of ‘isUnit’
      Matching instances:
        instance IsUnit a
        instance [overlapping] IsUnit ()
    • In the expression: isUnit @a
```

What gives? The problem is that GHC simply doesn’t know what type `a` is when compiling `guardUnit`. It *could* be instantiated to `()` where it’s called, but it might not be. Therefore, GHC doesn’t know which instance to pick, and an overlapping instance error is still reported.

This behavior is actually a very, very good thing. If GHC were to blindly pick the `IsUnit a` instance in this case, then `guardUnit` would always take the `False` branch, even when passed a value of type `()`! That would certainly not be what was intended, so it’s better to reject this program than to silently do the wrong thing. However, in more complicated situations, it can be quite surprising that GHC is complaining about instance overlap even when `{-# OVERLAPPING #-}` annotations are used, so it’s important to keep their limitations in mind.

As it happens, in this particular case, the error is easily remedied. We simply have to add an `IsUnit` constraint to the type signature of `guardUnit`:

```haskell
guardUnit :: forall a. IsUnit a => a -> Either String a
guardUnit x = case isUnit @a of
  True  -> Left "unit is not allowed"
  False -> Right x
```

Now picking the right `IsUnit` instance is deferred to the place where `guardUnit` is used, and the definition is accepted.[^3]

## Type families are functions from types to types

In the previous section, we discussed how typeclasses are functions from types to terms, but what about functions from types to types? For example, suppose we wanted to sum two type-level natural numbers and get a new type-level natural number as a result? For that, we can use a type family:

```haskell
{-# LANGUAGE TypeFamilies #-}

type family Sum a b where
  Sum Z     b = b
  Sum (S a) b = S (Sum a b)
```

The above is a [closed type family][ghc:closed-type-families], which works quite a lot like an ordinary Haskell function definition, just at the type level instead of at the value level. For comparison, the equivalent value-level definition of `Sum` would look like this:

```haskell
data Nat = Z | S Nat

sum :: Nat -> Nat -> Nat
sum Z     b = b
sum (S a) b = S (sum a b)
```

As you can see, the two are quite similar. Both are defined via a pair of pattern-matching clauses, and though it doesn’t matter here, both closed type families and ordinary functions evaluate their clauses top to bottom.

To test our definition of `Sum` in GHCi, we can use [the `:kind!` command][ghc:ghci-kind], which prints out a type and its kind after reducing it as much as possible:

```haskell
ghci> :kind! Sum (S Z) (S (S Z))
Sum (S Z) (S (S Z)) :: *
= S (S (S Z))
```

We can also combine `Sum` with our `ReifyNat` class from earlier:

```haskell
ghci> reifyNat @(Sum (S Z) (S (S Z)))
3
```

Type families are a useful complement to typeclasses when performing type-level programming. They allow computation to occur entirely at the type-level, which is necessarily computation that occurs entirely at compile-time, and the result can then be passed to a typeclass method to produce a term-level value from the result.

## Example 1: Generalized `concat`

Finally, using what we’ve discussed so far, we can do our first bit of practical TMP. Specifically, we’re going to define a `flatten` function similar to like-named functions provided by many dynamically-typed languages. In those languages, `flatten` is like `concat`, but it works on a list of arbitrary depth. For example, we might use it like this:

```haskell
> flatten [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
[1, 2, 3, 4, 5, 6, 7, 8]
```

In Haskell, lists of different depths have different types, so multiple levels of `concat` have to be applied explicitly. But using TMP, we can write a generic `flatten` function that operates on lists of any depth!

Since this is *typeclass* metaprogramming, we’ll unsurprisingly begin with a typeclass:

```haskell
class Flatten a where
  flatten :: a -> [???]
```

Our first challenge is writing the return type of `flatten`. Since the argument could be a list of any depth, there’s no direct way to obtain its element type. Fortunately, we can define a type family that does precisely that:

```haskell
type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a]   = a

class Flatten a where
  flatten :: a -> [ElementOf a]
```

Now we can write our `Flatten` instances. The base case is when the type is a list of depth 1, in which case we don’t have any flattening to do:

```haskell
instance Flatten [a] where
  flatten x = x
```

The inductive case is when the type is a nested list, in which case we want to apply `concat` and recur:

```haskell
instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten x = flatten (concat x)
```

Sadly, if we try to compile these definitions, GHC will reject our `Flatten [a]` instance:

```
error:
    • Couldn't match type ‘a’ with ‘ElementOf [a]’
      ‘a’ is a rigid type variable bound by
        the instance declaration
      Expected type: [ElementOf [a]]
        Actual type: [a]
    • In the expression: x
      In an equation for ‘flatten’: flatten x = x
      In the instance declaration for ‘Flatten [a]’
   |
   |   flatten x = x
   |               ^
```

At first blush, this error looks very confusing. Why doesn’t GHC think `a` and `ElementOf [a]` are the same type? Well, consider what would happen if we picked a type like `[Int]` for `a`. Then `[a]` would be `[[Int]]`, a nested list, so the first case of `ElementOf` would apply. Therefore, GHC refuses to pick the second equation of `ElementOf` so hastily.

In this particular case, we might think that’s rather silly. After all, if `a` were `[Int]`, then GHC wouldn’t have picked the `Flatten [a]` instance to begin with, it would pick the more specific `Flatten [[a]]` instance defined below. Therefore, the hypothetical situation above could never happen. Unfortunately, GHC does not realize this, so we find ourselves at an impasse.

Fortunately, we can soothe GHC’s anxiety by adding an extra constraint to our `Flatten [a]` instance:

```haskell
instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten x = x
```

This is a *type equality constraint*. Type equality constraints are written with the syntax `a ~ b`, and they state that `a` must be the same type as `b`. Type equality constraints are mostly useful when type families are involved, since they can be used (as in this case) to require a type family reduce to a certain type. In this case, we’re asserting that `ElementOf [a]` must always be `a`, which allows the instance to typecheck.

Note that this doesn’t let us completely wriggle out of our obligation, as the type equality constraint must *eventually* be checked when the instance is actually used, so initially this might seem like we’ve only deferred the problem to later. But in this case, that’s exactly what we need: by the time the `Flatten [a]` instance is selected, GHC will know that `a` is *not* a list type, and it will be able to reduce `ElementOf [a]` to `a` without difficulty. Indeed, we can see this for ourselves by using `flatten` in GHCi:

```haskell
ghci> flatten [[[1 :: Integer, 2], [3, 4]], [[5, 6], [7, 8]]]
[1,2,3,4,5,6,7,8]
```

It works! But why do we need the type annotation on `1`? If we leave it out, we get a rather hairy type error:

```
error:
    • Couldn't match type ‘ElementOf [a0]’ with ‘ElementOf [a]’
      Expected type: [ElementOf [a]]
        Actual type: [ElementOf [a0]]
      NB: ‘ElementOf’ is a non-injective type family
      The type variable ‘a0’ is ambiguous
```

The issue here stems from the polymorphic nature of Haskell number literals. Theoretically, someone could define a `Num [a]` instance, in which case `1` could actually have a list type, and either case of `ElementOf` could match depending on the choice of `Num` instance. Of course, no such `Num` instance exists, nor should it, but the possibility of it being defined means GHC can’t be certain of the depth of the argument list.

This issue happens to come up a lot in simple examples of TMP, since polymorphic number literals introduce a level of ambiguity. In real programs, this is much less of an issue, since there’s no reason to call `flatten` on a completely hardcoded list! However, it’s still important to understand what these type errors mean and why they occur.

That wrinkle aside, `flatten` is a functioning example of what useful TMP can look like. We’ve written a single, generic definition that flattens lists of any depth, taking advantage of static type information to choose what to do at runtime.

### Typeclasses as compile-time code generation

Presented with the above definition of `Flatten`, it might not be immediately obvious how to think about `Flatten` as a function from types to terms. After all, it looks a lot more like an “ordinary” typeclass (like, say, `Eq` or `Show`) than the `TypeOf` and `ReifyNat` classes we defined above.

One useful way to shift our perspective is to consider equivalent `Flatten` instances written using point-free style:

```haskell
instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat
```

These definitions of `flatten` no longer (syntactically) depend on term-level arguments, just like our definitions of `typeOf` and `reifyNat` didn’t accept any term-level arguments above. This allows us to consider what `flatten` might “expand to” given a type argument alone:

  * `flatten @[Int]` is just `id`, since the `Flatten [a]` instance is selected.

  * `flatten @[[Int]]` is `flatten @[Int] . concat`, since the `Flatten [[a]]` instance is selected. That then becomes `id . concat`, which can be further simplified to just `concat`.

  * `flatten @[[[Int]]]` is `flatten @[[Int]] . concat`, which simplifies to `concat . concat` by the same reasoning above.

  * `flatten @[[[[Int]]]]` is then `concat . concat . concat`, and so on.

This meshes quite naturally with our intuition of typeclasses as functions from types to terms. Each application of `flatten` takes a type as an argument and produces some number of composed `concat`s as a result. From this perspective, `Flatten` is performing a kind of compile-time code generation, synthesizing an expression to do the concatenation on the fly by inspecting the type information.

This framing is one of the key ideas that makes TMP so powerful, and indeed, it explains how it’s worthy of the name *metaprogramming*. As we continue to more sophisticated examples of TMP, try to keep this perspective in mind.

# Part 2: Generic programming

Part 1 of this blog post established the foundational techniques used in TMP, all of which are useful on their own. If you’ve read up to this point, you now know enough to start applying TMP yourself, and the remainder of this blog post will simply continue to build upon what you already know.

In the previous section, we discussed how to use TMP to write a generic `flatten` operation. In this section, we’ll aim a bit higher: totally generic functions that operate on *arbitrary* datatypes.

## Open type families and associated types

Before we can dive into examples, we need to revisit type families. In the previous sections, we discussed closed type families, but we did not cover their counterpart, *open type families*. Like closed type families, open type families are effectively functions from types to types, but unlike closed type families, they are not defined with a predefined set of equations. Instead, new equations are added separately using `type instance` declarations. For example, we could define our `Sum` family from above like this:

```haskell
type family Sum a b
type instance Sum Z b = b
type instance Sum (S a) b = S (Sum a b)
```

In the case of `Sum`, this would not be very useful, and indeed, `Sum` is much better expressed as a closed type family than an open one. But the advantage of open type families is similar to the advantage of typeclasses: new equations can be added at any time, even in modules other than the one that declares the open type family.

This extensibility means open type families are used less for type-level computation and more for type-level maps that associate types with other types. For example, one might define a `Key` open type family that relates types to the types used to index them:

```haskell
type family Key a
type instance Key (Vector a) = Int
type instance Key (Map k v) = k
type instance Key (Trie a) = ByteString
```

This can be combined with a typeclass to provide a generic way to see if a data structure contains a given key:

```haskell
class HasKey a where
  hasKey :: Key a -> a -> Bool

instance HasKey (Vector a) where
  hasKey i vec = i >= 0 && i < Data.Vector.length vec

instance HasKey (Map k v) where
  hasKey = Data.Map.member

instance HasKey (Trie a) where
  hasKey = Data.Trie.member
```

In this case, anyone could define their own data structure, define instances of `Key` and `HasKey` for their data structure, and use `hasKey` to see if it contains a given key, regardless of the structure of those keys. In fact, it’s so common for open type families and typeclasses to cooperate in this way that GHC provides the option to make the connection explicit by defining them together:

```haskell
class HasKey a where
  type Key a
  hasKey :: Key a -> a -> Bool

instance HasKey (Vector a) where
  type Key (Vector a) = Int
  hasKey i vec = i >= 0 && i < Data.Vector.length vec

instance HasKey (Map k v) where
  type Key (Map k v) = k
  hasKey = Data.Map.member

instance HasKey (Trie a) where
  type Key (Trie a) = ByteString
  hasKey = Data.Trie.member
```

An open family declared inside a typeclass like this is called an *associated type*. It works exactly the same way as the separate definitions of `Key` and `HasKey`, it just uses a different syntax. Note that although the `family` and `instance` keywords have disappeared from the declarations, that is only an abbreviation; the keywords are simply implicitly added (and explicitly writing them is still allowed, though most people do not).

Open type families and associated types are extremely useful for abstracting over similar types with slightly different structure, and libraries like [`mono-traversable`][hackage:mono-traversable] are examples of how they can be used to that end for their full effect. However, those use cases can’t really be classified as TMP, just using typeclasses for their traditional purpose of operation overloading.

However, that doesn’t mean open type families aren’t useful for TMP. In fact, one use case of TMP makes *heavy* use of open type families: datatype-generic programming.

## Example 2: Datatype-generic programming

*Datatype-generic programming* refers to a class of techniques for writing generic functions that operate on arbitrary data structures. Some useful applications of datatype-generic programming include

  * equality, comparison, and hashing,

  * recursive traversal of self-similar data structures, and

  * serialization and deserialization,

among other things. The idea is that by exploiting the structure of datatype definitions themselves, it’s possible for a datatype-generic function to provide implementations of functionality like the above on *any* datatype.

In Haskell, the most popular approach to datatype-generic programming leverages GHC generics, which is quite sophisticated. The [module documentation for `GHC.Generics`][hackage:base:GHC.Generics] already includes a fairly lengthy explanation of how it works, so I will not regurgitate it here (that could fill a blog post of its own!), but I will show how to construct a simplified version of the system that highlights the key role of TMP.

### Generic datatype representations

At the heart of the `Generic` class is a simple concept: all non-GADT Haskell datatypes can be represented as sums of products. For example, if we have

```haskell
data Authentication
  = AuthBasic Username Password
  | AuthSSH PublicKey
```

then we have a type that is essentially equivalent to this one:

```haskell
type Authentication = Either (Username, Password) PublicKey
```

If we know how to define a function on a nested tree built out of `Either`s and pairs, then we know how to define it on *any* such datatype! This is where TMP comes in: recall the way we viewed `Flatten` as a mechanism for compile-time code generation based on type information. Could we use the same technique to generate implementations of equality, comparison, hashing, etc. from statically-known information about the structure of a datatype?

The answer to that question is *yes*. To start, let’s consider a particularly simple example: suppose we want to write a generic function that counts the number of fields stored in an arbitrary constructor. For example, `numFields (AuthBasic "alyssa" "pass1234")` would return `2`, while `numFields (AuthSSH "<key>")` would return `1`. Not a very useful function, admittedly, but it’s a simple example of what generic programming can do.

We’ll start by using TMP to implement a “generic” version of `numFields` that operates on trees of `Either`s and pairs as described above:

```haskell
class GNumFields a where
  gnumFields :: a -> Natural

-- base case: leaf value
instance GNumFields a where
  gnumFields _ = 1

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b
```

Just like our `Flatten` class from earlier, `GNumFields` uses the type-level structure of its argument to choose what to do:

  * If we find a pair, that corresponds to a product, so we recur into both sides and sum the results.

  * If we find `Left` or `Right`, that corresponds to the “spine” differentiating different constructors, so we simply recur into the contained value.

  * In the case of any other value, we’re at a “leaf” in the tree of `Either`s and pairs, which corresponds to a single field, so we just return `1`.

Now if we call `gnumFields (Left ("alyssa", "pass1234"))`, we’ll get `2`, and if we call `gnumFields (Right "<key>")`, we’ll get `1`. All that’s left to do is write a bit of code that converts our `Authentication` type to a tree of `Either`s and pairs:

```haskell
genericizeAuthentication :: Authentication -> Either (Username, Password) PublicKey
genericizeAuthentication (AuthBasic user pass) = Left (user, pass)
genericizeAuthentication (AuthSSH key)         = Right key

numFieldsAuthentication :: Authentication -> Natural
numFieldsAuthentication = gnumFields . genericizeAuthentication
```

Now we get the results we want on our `Authentication` type using `numFieldsAuthentication`, but we’re not done yet, since it only works on `Authentication` values. Is there a way to define a generic `numFields` function that works on arbitrary datatypes that implement this conversion to sums-of-products? Yes, with another typeclass:

```haskell
class Generic a where
  type Rep a
  genericize :: a -> Rep a

instance Generic Authentication where
  type Rep Authentication = Either (Username, Password) PublicKey
  genericize (AuthBasic user pass) = Left (user, pass)
  genericize (AuthSSH key)         = Right key

numFields :: (Generic a, GNumFields (Rep a)) => a -> Natural
numFields = gnumFields . genericize
```

Now `numFields (AuthBasic "alyssa" "pass1234")` returns `2`, as desired, and it will *also* work with any datatype that provides a `Generic` instance. If the above code makes your head spin, don’t worry: this is by far the most complicated piece of code in this blog post up to this point. Let’s break down how it works piece by piece:

  * First, we define the `Generic` class, comprised of two parts:

    1. The `Rep a` associated type maps a type `a` onto its generic, sums-of-products representation, i.e. one built out of combinations of `Either` and pairs.

    2. The `genericize` method converts an actual *value* of type `a` to the equivalent value using the sums-of-products representation.

  * Next, we define a `Generic` instance for `Authentication`. `Rep Authentication` is the sums-of-products representation we described above, and `genericize` is likewise `genericizeAuthentication` from above.

  * Finally, we define `numFields` as a function with a `GNumFields (Rep a)` constraint. This is where all the magic happens:

    * When we apply `numFields` to a datatype, `Rep` retrieves its generic, sums-of-products representation type.

    * The `GNumFields` class then uses various TMP techniques we’ve already described so far in this blog post to generate a `numFields` implementation on the fly from the structure of `Rep a`.

    * Finally, that generated `numFields` implementation is applied to the genericized term-level value, and the result is produced.

After all that, I suspect you might think this seems like a very convoluted way to define the (rather unhelpful) `numFields` operation. Surely just defining `numFields` on each type directly would be far easier? Indeed, if we were just considering `numFields`, you’d be right, but in fact we get much more than that. Using the same machinery, we can continue to define other generic operations—equality, comparison, etc.—the same way we defined `numFields`, and all of them would automatically work on `Authentication` because they all leverage the same `Generic` instance!

This is the basic value proposition of generic programming: we can do a little work up front to normalize our datatype to a generic representation *once*, then get a whole buffet of generic operations on it for free. In Haskell, the code generation capabilities of TMP is a key piece of that puzzle.

### Improving our definition of `Generic`

You may note that the definition of `Generic` provided above does not match the one in `GHC.Generic`. Indeed, our naïve approach suffers from several flaws that the real version does not. This is not a `GHC.Generics` tutorial, so I will not discuss every detail of the full implementation, but I will highlight a few improvements relevant to the broader theme of TMP.

#### Distinguishing leaves from the spine

One problem with our version of `Generic` is that it provides no way to distinguish an `Either` or pair that should be considered a “leaf”, as in a type like this:

```haskell
data Foo = A (Either Int String) | B (Char, Bool)
```

Given this type, `Rep Foo` should be `Either (Either Int String) (Char, Bool)`, and `numFields (Right ('a', True))` will erroneously return `2` rather than `1`. To fix this, we can introduce a simple wrapper newtype that distinguishes leaves specifically:

```haskell
newtype Leaf a = Leaf { getLeaf :: a }
```

Now our `Generic` instances look like this:

```haskell
instance Generic Authentication where
  type Rep Authentication = Either (Leaf Username, Leaf Password) (Leaf PublicKey)
  genericize (AuthBasic user pass) = Left (Leaf user, Leaf pass)
  genericize (AuthSSH key)         = Right (Leaf key)

instance Generic Foo where
  type Rep Foo = Either (Leaf (Either Int String)) (Leaf (Char, Bool))
  genericize (A x) = Left (Leaf x)
  genericize (B x) = Right (Leaf x)
```

Since the `Leaf` constructor now distinguishes a leaf, rather than the absence of an `Either` or `(,)` constructor, we’ll have to update our `GNumFields` instances as well. However, this has the additional pleasant effect of eliminating the need for overlapping instances:

```haskell
instance GNumFields (Leaf a) where  
  gnumFields _ = 1

instance (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b
```

This is a good example of why overlapping instances can be so seductive, but they often have unintended consequences. Even when doing TMP, explicit tags are almost always preferable.

#### Handling empty constructors

Suppose we have a type with nullary data constructors, like the standard `Bool` type:

```haskell
data Bool = False | True
```

How do we write a `Generic` instance for `Bool`? Using just `Either`, `(,)`, and `Leaf`, we can’t, but if we are willing to add a case for `()`, we can use it to denote nullary constructors:

```haskell
instance GNumFields () where
  gnumFields _ = 0

instance Generic Bool where
  type Rep Bool = Either () ()
  genericize False = Left ()
  genericize True  = Right ()
```

In a similar vein, we could use `Void` to represent datatypes that don’t have any constructors at all.

### Continuing from here

The full version of `Generic` has a variety of further improvements useful for generic programming, including:

  * Support for converting from `Rep a` to `a`.

  * Special indication of self-recursive datatypes, making generic tree traversals possible.

  * Type-level information about datatype constructor and record accessor names, allowing them to be used in serialization.

  * Fully automatic generation of `Generic` instances via [the `DeriveGeneric` extension][ghc:derive-generic], which reduces the per-type boilerplate to essentially nothing.

The [module documentation for `GHC.Generics`][hackage:base:GHC.Generics] discusses the full system in detail, and it provides an additional example that uses the same essential TMP techniques discussed here.

# Part 3: Dependent typing

It’s time for the third and final part of this blog post: an introduction to dependently typed programming in Haskell. A full treatment of dependently typed programming is far, far too vast to be contained in a single blog post, so I will not attempt to do so here. Rather, I will cover some basic idioms for doing dependent programming and highlight how TMP can be valuable when doing so.

## Datatype promotion

In part 1, we used uninhabited datatypes like `Z` and `S a` to define new type-level constants. This works, but it is awkward. Imagine for a moment that we wanted to work with type-level booleans. Using our previous approach, we could define two empty datatypes, `True` and `False`:

```haskell
data True
data False
```

Now we could define type families to provide operations on these types, such as `Not`:

```haskell
type family Not a where
  Not True  = False
  Not False = True
```

However, this has some frustrating downsides:

  * First, it’s simply inconvenient that we have to define these new `True` and `False` “dummy” types, which are completely distinct from the `Bool` type provided by the prelude.

  * More significantly, it means `Not` has a very unhelpful kind:

    ```haskell
    ghci> :kind Not
    Not :: * -> *
    ```

    Even though `Not` is only *supposed* to be applied to `True` or `False`, its kind allows it to be applied to any type at all. You can see this in practice if you try to evaluate something like `Not Char`:

    ```haskell
    ghci> :kind! Not Char
    Not Char :: *
    = Not Char
    ```

    Rather than getting an error, GHC simply spits `Not Char` back at us. This is a somewhat unintuitive property of closed type families: if none of the clauses match, the type family just gets “stuck,” not reducing any further. This can lead to very confusing type errors later in the typechecking process.

One way to think about `Not` is that it is largely *dynamically kinded* in the same way some languages are dynamically typed. That isn’t entirely true, as we technically *will* get a kind error if we try to apply `Not` to a type constructor rather than a type, such as `Maybe`:

```
ghci> :kind! Not Maybe

<interactive>:1:5: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
```

…but `*` is still a very big kind, much bigger than we would like to permit for `Not`.

To help with both these problems, GHC provides *datatype promotion* via [the `DataKinds` language extension][ghc:data-kinds]. The idea is that for each normal, non-GADT type definition like

```haskell
data Bool = False | True
```

then in addition to the normal type constructor and value constructors, GHC also defines several *promoted* constructors:

  * `Bool` is allowed as both a type and a kind.

  * `'True` and `'False` are defined as new types of kind `Bool`.

We can see this in action if we remove our `data True` and `data False` declarations and adjust our definition of `Not` to use promoted constructors:

```haskell
{-# LANGUAGE DataKinds #-}

type family Not a where
  Not 'True  = 'False
  Not 'False = 'True
```

Now the inferred kind of `Not` is no longer `* -> *`:

```haskell
ghci> :kind Not
Not :: Bool -> Bool
```

Consequently, we will now get a kind error if we attempt to apply `Not` to anything other than `'True` or `'False`:

```
ghci> :kind! Not Char

<interactive>:1:5: error:
    • Expected kind ‘Bool’, but ‘Char’ has kind ‘*’
```

This is a nice improvement. We can make a similar change to our definitions involving type-level natural numbers:

```haskell
data Nat = Z | S Nat

class ReifyNat (a :: Nat) where
  reifyNat :: Natural

instance ReifyNat 'Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat ('S a) where
  reifyNat = 1 + reifyNat @a
```

Note that we need to add an explicit kind signature on the definition of the `ReifyNat` typeclass, since otherwise GHC will assume `a` has kind `*`, since nothing in the types of the typeclass methods suggests otherwise. In addition to making it clearer that `Z` and `S` are related, this prevents someone from coming along and defining a nonsensical instance like `ReifyNat Char`, which previously would have been allowed but will now be rejected with a kind error.

Datatype promotion is not strictly required to do TMP, but makes the process significantly less painful. It makes Haskell’s kind language extensible in the same way its type language is, which allows type-level programming to enjoy static typechecking (or more accurately, static kindchecking) in the same way term-level programming does.

## GADTs and proof terms

So far in this blog post, we have discussed several different function-like things:

  * Ordinary Haskell functions are functions from terms to terms.

  * Type families are functions from types to types.

  * Typeclasses are functions from types to terms.

A curious reader may wonder about the existence of a fourth class of function:

  * *???* are functions from terms to types.

To reason about what could go in the *???* above, we must consider what “a function from terms to types” would even mean. Functions from terms to terms and types to types are straightforward enough. Functions from types to terms are a little trickier, but they make intuitive sense: we use information known at compile-time to generate runtime behavior. But how could information possibly flow in the other direction? How could we possibly turn runtime information into compile-time information without being able to predict the future?

In general, we cannot. However, one feature of Haskell allows a restricted form of seemingly doing the impossible—turning runtime information into compile-time information—and that’s GADTs.

GADTs[^4] are [described in detail in the GHC User’s Guide][ghc:gadts], but the key idea for our purposes is that *pattern-matching on a GADT constructor can refine type information*. Here’s a simple, silly example:

```haskell
data WhatIsIt a where
  ABool :: WhatIsIt Bool
  AnInt :: WhatIsIt Int

doSomething :: WhatIsIt a -> a -> a
doSomething ABool x = not x
doSomething AnInt x = x + 1
```

Here, `WhatIsIt` is a datatype with two nullary constructors, `ABool` and `AnInt`, similar to a normal, non-GADT datatype like this one:

```haskell
data WhatIsIt a = ABool | AnInt
```

What’s special about GADTs is that each constructor is given an explicit type signature. With the plain ADT definition above, `ABool` and `AnInt` would both have the type `forall a. WhatIsIt a`, but in the GADT definition, we explicitly fix `a` to `Bool` in the type of `ABool` and to `Int` in the type of `AnInt`.

This simple feature allows us to do very interesting things. The `doSomething` function is polymorphic in `a`, but on the right-hand side of the first equation, `x` has type `Bool`, while on the right-hand side of the second equation, `x` has type `Int`. This is because the `WhatIsIt a` argument effectively constrains the type of `a`, as we can see by experimenting with `doSomething` in GHCi:

```
ghci> doSomething ABool True
False
ghci> doSomething AnInt 10
11
ghci> doSomething AnInt True

error:
    • Couldn't match expected type ‘Int’ with actual type ‘Bool’
    • In the second argument of ‘doSomething’, namely ‘True’
      In the expression: doSomething AnInt True
      In an equation for ‘it’: it = doSomething AnInt True
```

One way to think about GADTs is as “proofs” or “witnesses” of type equalities. The `ABool` constructor is a proof of `a ~ Bool`, while the `AnInt` constructor is a proof of `a ~ Int`. When you construct `ABool` or `AnInt`, you must be able to satisfy the equality, and it is in a sense “packed into” the constructor value. When code pattern-matches on the constructor, the equality is “unpacked from” the value, and the equality becomes available on the right-hand side of the pattern match.

GADTs can be much more sophisticated than our simple `WhatIsIt` type above. Just like normal ADTs, GADT constructors can have parameters, which makes it possible to write inductive datatypes that carry type equality proofs with them:

```haskell
infixr 5 `HCons`

data HList as where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)
```

This type is a *heterogenous list*, a list that can contain elements of different types:

```haskell
ghci> :t True `HCons` "hello" `HCons` 42 `HCons` HNil
True `HCons` "hello" `HCons` 42 `HCons` HNil
  :: Num a => HList '[Bool, [Char], a]
```

An `HList` is parameterized by a type-level list that keeps track of the types of its elements, which allows us to highlight another interesting property of GADTs: if we restrict that type information, the GHC pattern exhaustiveness checker will take the restriction into account. For example, we can write a completely total `head` function on `HList`s like this:

```haskell
head :: HList (a ': as) -> a
head (x `HCons` _) = x
```

Remarkably, GHC does not complain that this definition of `head` is non-exhaustive. Since we specified that the argument must be of type `HList (a ': as)` in the type signature for `head`, GHC knows that the argument *cannot* be `HNil` (which would have the type `HList '[]`), so it doesn’t ask us to handle that case.

These examples illustrate the way GADTs serve as a general-purpose construct for relating type- and term-level information. Information flows bidirectionally: type information refines the set of type constructors that can be matched on, and matching on type constructors exposes new type equalities.

### Proofs that work together

This interplay is wonderfully compositional. Suppose we wanted to write a function that accepts an `HList` of exactly 1, 2, or 3 elements. There’s no easy way to express that in the type signature the way we did with `head`, so it might seem like all we can do is write an entirely new container datatype that has three constructors, one for each case.

However, a more interesting solution exists that takes advantage of the bidirectional nature of GADTs. We can start by writing a *proof term* that contains no values, it just encapsulates type equalities on a type-level list:

```haskell
data OneToThree a b c as where
  One   :: OneToThree a b c '[a]
  Two   :: OneToThree a b c '[a, b]
  Three :: OneToThree a b c '[a, b, c]
```

We call it a proof term because a value of type `OneToThree a b c as` constitutes a *proof* that `as` has exactly 1, 2, or 3 elements. Using `OneToThree`, we can write a function that accepts an `HList` accompanied by a proof term:

```haskell
sumUpToThree :: OneToThree Int Int Int as -> HList as -> Int
sumUpToThree One   (x `HCons` HNil)                     = x
sumUpToThree Two   (x `HCons` y `HCons` HNil)           = x + y
sumUpToThree Three (x `HCons` y `HCons` z `HCons` HNil) = x + y + z
```

As with `head`, this function is completely exhaustive, in this case because we take full advantage of the bidirectional nature of GADTs:

  * When we match on the `OneToThree` proof term, information flows from the term level to the type level, refining the type of `as` in that branch.

  * The refined type of `as` then flows back down to the term level, restricting the shape the `HList` can take and refinine the set of patterns we have to match.

Of course, this example is not especially useful, but in general proof terms can encode any number of useful properties. For example, we can write a proof term that ensures an `HList` has an even number of elements:

```haskell
data Even as where
  EvenNil  :: Even '[]
  EvenCons :: Even as -> Even (a ': b ': as)
```

This is a proof which itself has inductive structure: `EvenCons` takes a proof that `as` has an even number of elements and produces a proof that adding two more elements preserves the evenness. We can combine this with a type family to write a function that “pairs up” elements in an `HList`:

```haskell
type family PairUp as where
  PairUp '[]            = '[]
  PairUp (a ': b ': as) = (a, b) ': PairUp as

pairUp :: Even as -> HList as -> HList (PairUp as)
pairUp EvenNil         HNil                     = HNil
pairUp (EvenCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp even xs
```

Once again, this definition is completely exhaustive, and we can show that it works in GHCi:

```haskell
ghci> pairUp (EvenCons $ EvenCons EvenNil)
             (True `HCons` 'a' `HCons` () `HCons` "foo" `HCons` HNil)
(True,'a') `HCons` ((),"foo") `HCons` HNil
```

This ability to capture properties of a type using auxiliary proof terms, rather than having to define an entirely new type, is one of the things that makes dependently typed programming so powerful.

### Proof inference

While our definition of `pairUp` is interesting, you may be skeptical of its practical utility. It’s fiddly and inconvenient to have to pass the `Even` proof term explicitly, since it must be updated every time the length of the list changes. Fortunately, this is where TMP comes in.

Remember that typeclasses are functions from types to terms. As its happens, a value of type `Even as` can be mechanically produced from the structure of the type `as`. This suggests that we could use TMP to automatically generate `Even` proofs, and indeed, we can. In fact, it’s not at all complicated:

```haskell
class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenCons evenProof
```

We can now adjust our `pairUp` function to use `IsEven` instead of an explicit `Even` argument:

```haskell
pairUp :: IsEven as => HList as -> HList (PairUp as)
pairUp = go evenProof where
  go :: Even as -> HList as -> HList (PairUp as)
  go EvenNil         HNil                     = HNil
  go (EvenCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` go even xs
```

This is essentially identical to its old definition, but by acquiring the proof via `IsEven` rather than passing it explicitly, we can call `pairUp` without having to construct a proof manually:

```haskell
ghci> pairUp (True `HCons` 'a' `HCons` () `HCons` "foo" `HCons` HNil)
(True,'a') `HCons` ((),"foo") `HCons` HNil
```

This is rather remarkable. Using TMP, we are able to get GHC to *automatically construct a proof that a list is even*, with no programmer guidance beyond writing the `IsEven` typeclass. This relies once more on the perspective that typeclasses are functions that accept types and generate term-level code: `IsEven` is a function that accepts a type-level list and generates an `Even` proof term.

From this perspective, **typeclasses are a way of specifying a proof search algorithm** to the compiler. In the case of `IsEven`, the proofs being generated are rather simple, so the proof search algorithm is quite mechanical. But in general, typeclasses can be used to perform proof search of significant complexity, given a sufficiently clever encoding into the type system.

## Aside: GADTs versus type families

Before moving on, I want to explicitly call attention to the relationship between GADTs and type families. Though at first glance they may seem markedly different, there are some similarities between the two, and sometimes they may be used to accomplish similar things.

Consider again the type of the `pairUp` function above (without the typeclass for simplicity):

```haskell
pairUp :: Even as -> HList as -> HList (PairUp as)
```

We used both a GADT, `Even` and a type family, `PairUp`. But we could have, in theory, used *only* a GADT and eliminated the type family altogether. Consider this variation on the `Even` proof term:

```haskell
data EvenPairs as bs where
  EvenNil  :: EvenPairs '[] '[]
  EvenCons :: EvenPairs as bs -> EvenPairs (a ': b ': as) ((a, b) ': bs)
```

This type has two type parameters rather than one, and though there’s no distinction between the two from GHC’s point of view, it can be useful to think of `as` as an “input” parameter and `bs` as an “output” parameter. The idea is that any `EvenPairs` proof relates both an even-length list type and its paired up equivalent:

  * `EvenNil` has type `EvenPairs '[] '[]`,

  * `EvenCons EvenNil` has type `EvenPairs '[a, b] '[(a, b)]`,

  * `EvenCons (EvenCons EvenNil)` has type `EvenPairs '[a, b, c, d] '[(a, b), (c, d)]`,

  * …and so on.

This allows us to reformulate our `pairUp` type signature this way:

```haskell
pairUp :: EvenPairs as bs -> HList as -> HList bs
```

The definition is otherwise unchanged. The `PairUp` type family is completely gone, because now `EvenPairs` itself defines the relation. In this way, GADTs can be used like type-level functions!

The inverse, however, is not true, at least not directly: we cannot eliminate the GADT altogether and exclusively use type families. One way to attempt doing so would be to define a type family that returns a constraint rather than a type:

```haskell
import Data.Kind (Constraint)

type family IsEvenTF as :: Constraint where
  IsEvenTF '[]            = ()
  IsEvenTF (_ ': _ ': as) = IsEvenTF as
```

The idea here is that `IsEvenTF as` produces a constraint can only be satisfied if `as` has an even number of elements, since that’s the only way it will eventually reduce to `()`, which in this case means the empty set of constraints, not the unit type (yes, the syntax for that is confusing). And in fact, it’s true that putting `IsEvenTF as =>` in a type signature successfully restricts `as` to be an even-length list, but it doesn’t allow us to write `pairUp`. To see why, we can try the following definition:

```haskell
pairUp :: IsEvenTF as => HList as -> HList (PairUp as)
pairUp HNil                     = HNil
pairUp (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp xs
```

Unlike the version using the GADT, this version of `pairUp` is not considered exhaustive:

```
warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘pairUp’: Patterns not matched: HCons _ HNil
```

This is because type families don’t provide the same bidirectional flow of information that GADTs do, they’re only type-level functions. The constraint generated by `IsEvenTF` provides no term-level evidence about the shape of `as`, so we can’t branch on it the way we can branch on the `Even` GADT.[^5] (In a sense, `IsEvenTF` is doing [validation, not parsing][blog:pdv].)

For this reason, I caution against overuse of type families. Their simplicity is seductive, but all too often you pay for that simplicity with inflexibility. GADTs combined with TMP for proof inference can provide the best of both worlds: complete control over the term-level proof that gets generated while still letting the compiler do most of the work for you.

## Guiding type inference

So far, this blog post has given relatively little attention to type inference. That is in some part a testament to the robustness of GHC’s type inference algorithm: even when fairly sophisticated TMP is involved, GHC often manages to propagate enough type information that type annotations are rarely needed.

However, when doing TMP, it would be irresponsible to not at least consider the type inference properties of programs. Type inference is what drives the whole typeclass resolution process to begin with, so poor type inference can easily make your fancy TMP construction next to useless. To take advantage of GHC to the fullest extent, programs should proactively guide the typechecker to help it infer as much as possible as often as possible.

To illustrate what that can look like, suppose we want to use TMP to generate an `HList` full of `()` values of an arbitrary length:

```haskell
class UnitList as where
  unitList :: HList as

instance UnitList '[] where
  unitList = HNil

instance UnitList as => UnitList (() ': as) where
  unitList = () `HCons` unitList
```

Testing in GHCi, we can see it behaves as desired:

```haskell
ghci> unitList :: HList '[(), (), ()]
() `HCons` () `HCons` () `HCons` HNil
```

Now suppose we write a function that accepts a list containing exactly one element and returns it:

```haskell
unsingleton :: HList '[a] -> a
unsingleton (x `HCons` HNil) = x
```

Naturally, we would expect these to compose without a hitch. If we write `unsingleton unitList`, our TMP should generate a list of length 1, and we should get back `()`. However, it may surprise you to learn that *isn’t*, in fact, what happens:[^6]

```
ghci> unsingleton unitList

error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘unitList’
      prevents the constraint ‘(UnitList '[a0])’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance UnitList as => UnitList (() : as)
```

What went wrong? The type error says that `a0` is ambiguous, but it only lists a single matching `UnitList` instance—the one we want—so how can it be ambiguous which one to select?

The problem stems from the way we defined `UnitList`. When we wrote the instance

```haskell
instance UnitList as => UnitList (() ': as) where
```

we said the first element of the type-level list must be `()`, so there’s nothing stopping someone from coming along and defining another instance:

```haskell
instance UnitList as => UnitList (Int ': as) where
  unitList = 0 `HCons` unitList
```

In that case, GHC would have no way to know which instance to pick. Nothing in the type of `unsingleton` forces the element in the list to have type `()`, so both instances are equally valid. To hedge against this future possibility, GHC rejects the program as ambiguous from the start.

Of course, this isn’t what we want. The `UnitList` class is supposed to *always* return a list of `()` values, so how can we force GHC to pick our instance anyway? The answer is to play a trick:

```haskell
instance (a ~ (), UnitList as) => UnitList (a ': as) where
  unitList = () `HCons` unitList
```

Here we’ve changed the instance so that it has the shape `UnitList (a ': as)`, with a type variable in place of the `()`, but we also added an equality constraint that forces `a` to be `()`. Intuitively, you might think these two instances are completely identical, but in fact they are not! As proof, our example now typechecks:

```haskell
ghci> unsingleton unitList
()
```

To understand why, it’s important to understand how GHC’s typeclass resolution algorithm works. Let’s start by establishing some terminology. Note that every instance declaration has the following shape:

```haskell
instance <constraints> => C <types>
```

The part to the left of the `=>` is known as the *instance context*, while the part to the right is known as the *instance head*. Now for the important bit: when GHC attempts to pick which typeclass instance to use to solve a typeclass constraint, **only the instance head matters, and the instance context is completely ignored**. Once GHC picks an instance, it commits to its choice, and only then does it consider the instance context.

This explains why our two `UnitList` instances behave differently:

  * Given the instance head `UnitList (() ': as)`, GHC won’t select the instance unless it knows the first element of the list is `()`.

  * But given the instance head `UnitList (a ': as)`, GHC will pick the instance regardless of the type of the first element. All that matters is that the list is at least one element long.

After the `UnitList (a ': as)` instance is selected, GHC attempts to solve the constraints in the instance context, including the `a ~ ()` constraint. This *forces* `a` to be `()`, resolving the ambiguity and allowing type inference to proceed.

This distinction might seem excessively subtle, but in practice it is enormously useful. It means you, the programmer, have direct control over the type inference process:

  * If you put a type in the instance head, you’re asking GHC to figure out how to make the types match up by some other means. Sometimes that’s very useful, since perhaps you want that type to inform which instance to pick.

  * But if you put an equality constraint in the instance context, the roles are reversed: you’re saying to the compiler “you don’t tell me, I’ll tell *you* what type this is,” effectively giving you a role in type inference itself.

From this perspective, typeclass instances with equality constraints make GHC’s type inference algorithm extensible. You get to pick which decisions are made and when, and crucially, you can use knowledge of your own program structure to expose more information to the typechecker.

Given all of the above, consider again the definition of `IsEven` from earlier:

```haskell
class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenCons evenProof
```

Though it didn’t cause any problems in the examples we tried, this definition isn’t optimized for type inference. If GHC needed to solve an `IsEven (a ': b0)` constraint, where `b0` is an ambiguous type variable, it would get stuck, since it doesn’t know that someone won’t come along and define an `IsEven '[a]` instance in the future.

To fix this, we can apply the same trick we used for `UnitList`, just in a slightly different way:

```haskell
instance (as ~ (b ': bs), IsEven bs) => IsEven (a ': as) where
  evenProof = EvenCons evenProof
```

Again, the idea is to move the type information we *learn* from picking this instance into the instance context, allowing it to guide type inference rather than making type inference figure it out from some other source. Consistently applying this transformation can **dramatically** improve type inference in programs that make heavy use of TMP.

## Example 3: Subtyping constraints

At least, we have reached the final example of this blog post. For this one, I have the pleasure of providing a real-world example from a production Haskell codebase: while I was working at [Hasura][hasura], I had the pleasure of designing an internal parser combinator library that captures aspects of the [GraphQL][graphql] type system. One such aspect of that type system is a form of subtyping; GraphQL essentially has two “kinds” of types—input types and output types—but some types can be used as both.

Haskell has no built-in support for subtyping, so most Haskell programs do their best to get away with parametric polymorphism instead. However, in our case, we actually need to distinguish (at runtime) types in the “both” category from those that are exclusively input or exclusively output types. Consequently, our `GQLKind` datatype has three cases:

```haskell
data GQLKind
  = Both
  | Input
  | Output
```

We use `DataKind`-promoted versions of this `GQLKind` type as a parameter to a `GQLType` GADT:

```haskell
data GQLType k where
  TScalar      :: GQLType 'Both
  TInputObject :: InputObjectInfo -> GQLType 'Input
  TIObject     :: ObjectInfo -> GQLType 'Output
  -- ...and so on...
```

This allows us to write functions that only accept input types or only accept output types, which is a wonderful property to be able to guarantee at compile-time! But there’s a problem: if we write a function that only accepts values of type `GQLType 'Input`, we can’t pass a `GQLType 'Both`, even though we really ought to be able to.

To fix this, we can use a little dependently typed programming. First, we’ll define a type to represent proof terms that witness a subkinding relationship:

```haskell
data SubKind k1 k2 where
  KRefl :: SubKind k k
  KBoth :: SubKind 'Both k
```

The first case, `KRefl`, states that every kind is trivially a subkind of itself. The second case, `KBoth`, states that `Both` is a subkind of any kind at all. (This is a particularly literal example of [using a type to define axioms][blog:axioms].) The next step is to use TMP to implement proof inference:

```haskell
class IsSubKind k1 k2 where
  subKindProof :: SubKind k1 k2

instance IsSubKind 'Both k where
  subKindProof = KBoth

instance (k ~ 'Input) => IsSubKind 'Input k where
  subKindProof = KRefl

instance (k ~ 'Output) => IsSubKind 'Output k where
  subKindProof = KRefl
```

These instances use the type equality trick described in the previous section to guide type inference, ensuring that if we ever need to prove that `k` is a superkind of `'Input` or `'Output`, type inference will force them to be equal.

Using `IsSubKind`, we can easily resolve the problem described above. Rather than write a function with a type like this:

```haskell
nullable :: GQLParser 'Input a -> GQLParser 'Input (Maybe a)
```

…we simply use an `IsSubKind` constraint, instead:

```haskell
nullable :: IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
```

Now both `'Input` and `'Both` kinds are accepted. In my experience, this caused no trouble at all for callers of these functions; everything worked completely automatically. *Consuming* the `SubKind` proofs was slightly more involved, but only ever so slightly. For example, we have a type family that looks like this:

```haskell
type family ParserInput k where
  ParserInput 'Both   = InputValue
  ParserInput 'Input  = InputValue
  ParserInput 'Output = SelectionSet
```

This type family is used to determine what a `GQLParser k a` actually consumes as input, based on the kind of the GraphQL type it corresponds to. In some functions, we need to prove to GHC that `IsSubKind k 'Input` implies `ParserInput k ~ InputValue`.

Fortunately, that is very easy to do using [the `(:~:)` type from `Data.Type.Equality` in `base`][hackage:base:Data.Type.Equality] to capture a term-level witness of a type equality. It’s an ordinary Haskell GADT that happens to have an infix type constructor, and this is its definition:

```haskell
data a :~: b where
  Refl :: a :~: a
```

Just as with any other GADT, `(:~:)` can be used to pack up type equalities and unpack them later; `a :~: b` just happens to be the GADT that corresponds precisely to the equality `a ~ b`. Using `(:~:)`, we can write a reusable proof that `IsSubKind k 'Input` implies `ParserInput k ~ InputValue`:

```haskell
inputParserInput :: forall k. IsSubKind k 'Input => ParserInput k :~: InputValue
inputParserInput = case subKindProof @k @'Input of
  KRefl -> Refl
  KBoth -> Refl
```

This function is a very simple proof by cases, where `Refl` can be read as “Q.E.D.”:

  * In the first case, matching on `KRefl` refines `k` to `'Input`, and `ParserInput 'Input` is `InputValue` by definition of `ParserInput`.

  * Likewise, in the second case, matching on `KBoth` refines `k` to `'Both`, and `ParserInput 'Both` is also `InputValue` by definition of `ParserInput`.

This `inputParserInput` helper allows functions like `nullable`, which internally need `ParserInput k ~ InputValue`, to take the form

```haskell
nullable :: forall k a. IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
nullable parser = case inputParserInput @k of
  Refl -> {- ...implementation goes here... -}
```

Overall, this burden is quite minimal, so the additional type safety is more than worth the effort. The same could not be said without `IsSubKind` doing work to infer the proofs at each use site, so in this case, TMP has certainly paid its weight!

# Wrapping up and closing thoughts

So concludes my introduction to Haskell TMP. As seems to happen all too often with my blog posts, this one has grown rather long, so allow me to provide a summary of the most important points:

  * Typeclass metaprogramming is a powerful technique for performing type-directed code generation, making it a form of “value inference” that infers values from types.

  * Unlike most other metaprogramming mechanisms, TMP has a wonderful synergy with type inference, which allows it to take advantage of information the programmer may not have even written explicitly.

  * Though I’ve called the technique “<em>typeclass</em> metaprogramming,” TMP really leverages the entirety of the modern GHC type system. Type families, GADTs, promoted types, and more all have their place in usefully applying type-level programming.

  * Finally, since TMP relies so heavily on type inference to do its job, it’s crucial to be thoughtful about how you design type-level code to give the typechecker as many opportunities to succeed as you possibly can.

The individual applications of TMP covered in this blog post—type-level computation, generic programming, and dependent typing—are all useful in their own right, and this post does not linger on any of them long enough to do any of them justice. That is, perhaps, the cost one pays when trying to discuss such an abstract, general technique. However, I hope that readers can see the forest for the trees and understand how TMP can be a set of techniques in their own right, applicable to the topics described above and more.

Readers may note that this blog post targets a slightly different audience than my other recent writing has been. That is a conscious choice: there is an unfortunate dearth of resources to help intermediate Haskell programmers become advanced Haskell programmers, in part because it’s hard to write them. The lack of resources makes tackling topics like this rather difficult, as too often it feels as though an entire web of concepts must be explained all at once, with no obvious incremental path that provides sufficient motivation every step of the way.

It remains to be seen whether my stab at the problem will be successful. But on the chance that it is, I suspect some readers will be curious about where to go next. Here are some ideas:

  * As mentioned earlier in this blog post, [the `GHC.Generics` module documentation][hackage:base:GHC.Generics] is a great resource if you want to explore generic programming further, and generic programming is a great way to put TMP to practical use.

  * I have long believed that [the GHC User’s Guide][ghc] is a criminally under-read and underappreciated piece of documentation. It is a treasure trove of knowledge, and I highly recommend reading through the sections on type-related language extensions if you want to get a better grasp of the mechanics of the Haskell type system.

  * Finally, if dependently typed programming in Haskell intrigues you, and you don’t mind staring into the sun, the [singletons][hackage:singletons] library provides abstractions and design patterns that can considerably cut down on the boilerplate. (Also, [the accompanying paper][rae:singletons] is definitely worth a read if you’d like to go down that route.)

Even if you don’t decide to pursue type-level programming in Haskell, I hope this blog post helps make some of the concepts involved less mystical and intimidating. I, for one, think this stuff is worth the effort involved in understanding. After all, you never know when it might come in handy.


[^1]: Not to be confused with C++’s [*template* metaprogramming][wiki:cpp-tmp], though there are significant similarities between the two techniques.

[^2]: There have been proposals to introduce ordered instances, known in the literature as [*instance chains*][mpj:instance-chains], but as of this writing, GHC does not implement them.

[^3]: Note that this also preserves an important property of the Haskell type system, parametricity. A function like `id :: a -> a` shouldn’t be allowed to do different things depending on which type is chosen for `a`, which our first version of `guardUnit` tried to violate. Typeclasses, being functions on types, can naturally do different things given different types, so a typeclass constraint is precisely what gives us the power to violate parametricity.

[^4]: Short for *generalized algebraic datatypes*, which is a rather unhelpful name for actually understanding what they are or what they’re for.

[^5]: If GHC allowed lightweight existential quantification, we could make that term-level evidence available with a sufficiently clever definition for `IsEvenTF`:

    ```haskell
    type family IsEvenTF as :: Constraint where
      IsEvenTF '[]       = ()
      IsEvenTF (a ': as) = exists b as'. (as ~ (b ': as'), IsEvenTF as')
    ```

    The type refinement provided by matching on `HCons` would be enough for the second case of `IsEvenTF` to be selected, which would provide an equality proof that `as` has at least two elements. Sadly, GHC does not support anything of this sort, and it’s unclear if it would be tractable to implement at all.

[^6]: Actually, I’ve cheated a little bit here, because `unsingleton unitList` really does typecheck in GHCi under normal circumstances. That’s because [the `ExtendedDefaultRules` extension][ghc:extended-default-rules] is enabled in GHCi by default, which defaults ambiguous type variables to `()`, which happens to be exactly what’s needed to make this contrived example typecheck. However, that doesn’t say anything very useful, since the same expression really would fail to typecheck inside a Haskell module, so I’ve turned `ExtendedDefaultRules` off to illustrate the problem.


[blog:axioms]: /blog/2020/08/13/types-as-axioms-or-playing-god-with-static-types/
[blog:pdv]: /blog/2019/11/05/parse-don-t-validate/
[ghc]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/
[ghc:closed-type-families]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/type_families.html#closed-type-families
[ghc:data-kinds]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/data_kinds.html
[ghc:derive-generic]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/generics.html#extension-DeriveGeneric
[ghc:extended-default-rules]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/ghci.html#extension-ExtendedDefaultRules
[ghc:gadts]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/gadt.html
[ghc:ghci-kind]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/ghci.html#ghci-cmd-:kind
[ghc:overlapping-instances]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/instances.html#overlapping-instances
[ghc:type-applications]: https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/type_applications.html
[graphql]: https://graphql.org/
[hackage:base:Data.Type.Equality]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Type-Equality.html
[hackage:base:GHC.Generics]: https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Generics.html
[hackage:mono-traversable]: https://hackage.haskell.org/package/mono-traversable
[hackage:servant]: https://hackage.haskell.org/package/servant
[hackage:singletons]: https://hackage.haskell.org/package/singletons
[hasura]: https://hasura.io/
[mpj:instance-chains]: https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-icfp2010-instances.pdf
[rae:singletons]: https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf
[wiki:cpp-tmp]: https://en.wikipedia.org/wiki/Template_metaprogramming

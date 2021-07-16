    Title: Functionally updating record types in Elm
    Date: 2015-11-06T19:58:40
    Tags: elm

[Elm][elm-lang] is a wonderful language for building web apps, and I love so much of its approach to language design. Elm does so many things *right* straight out of the box, and that's a real breath of fresh air in the intersection of functional programming and web development. Still, it gets one thing wrong, and unfortunately, that one thing is incredibly important. Elm took the "functions" out of "functional record types".

Almost any software program, at its core, is all about data. Maybe it's about computing data, maybe it's about manipulating data, or maybe it's about displaying data, but at the end of the day, some sort of data model is going to be needed. The functional model is a breathtakingly elegant system for handling data and shuttling it around throughout a program, and [functional reactive programming][frp], which Elm uses to model event-like interactions, makes this model work even better. The really important thing, though, is what tools Elm actually gives you to model your data.

# A brief primer on Elm records

Elm supports all the core datatypes one would expect—numbers, strings, booleans, optionals, etc.—and it allows users to define their own types with ADTs. However, Elm also provides another datatype, which it calls "records". Records are similar to objects in JavaScript: they're effectively key-value mappings. They're cool data structures, and they work well. Here's an example of creating a `Point` datatype in Elm:

```elm
type alias Point =
  { x : Float, y : Float }
```

Notice that `Point` is declared as a type *alias*, not as a separate type like an ADT. This is because record types are truly encoded in the type system as values with named fields, not as disparate types. This allows for some fun tricks, but that's outside the scope of this blog post.

# The good

What I'd like to discuss is what it looks like to *manipulate* these data structures. Constructing them is completely painless, and reading from them is super simple. This is where the record system gets everything very *right*.

```elm
origin : Point
origin = { x = 0, y = 0 }

distanceBetween : Point -> Point -> Float
distanceBetween a b =
  let dx = a.x - b.x
      dy = a.y - b.y
  in sqrt (dx*dx + dy*dy)
```

The syntax is clean and simple. Most importantly, however, the record system is functional (in the "functional programming" sense). In a functional system, it's useful to express concepts in terms of function composition, and this is very easy to do in Elm. Creating a function to access a field would normally be clunky if you always needed to do `record.field` to access the value. Fortunately, Elm provides some sugar:

```elm
-- These two expressions are equivalent:
(\record -> record.field)
.field
```

Using the `.field` shorthand allows writing some other functions in terms of composition, as most functional programmers would desire:

```elm
doubledX : Point -> Float
doubledX = ((*) 2) << .x
```

This satisfies me.

# The bad

So if everything in Elm is so great, what am I complaining about? Well, while the syntax to access fields is convenient, the syntax to *functionally set* fields is questionably clunky. Consider a function that accepts a point and returns a new point with its `x` field set to `0`:

```elm
zeroedX : Point -> Point
zeroedX point = { point | x <- 0 }
```

This doesn't look too bad, does it? It's clear and concise. To me, though, there's something deeply wrong here... this function has a lot of redundancy! It seems to me like we should be able to write this function more clearly in a point-free style. The `.field` shorthand "functionalizes" the record getter syntax, so there must be a function version of the update syntax, right? Maybe it would look something like this:

```elm
zeroedX : Point -> Point
zeroedX = !x 0
```

But alas, there is no such syntax.

Now you may ask... why does it matter? This seems trivial, and in fact, the explicit updater syntax may actually be more readable by virtue of how explicit it is. You'd be right, because so far, these examples have been horribly contrived. But let's consider a slightly more useful example: *functionally updating* a record.

What's the difference? Well, say I wanted to take a point and increment its `x` field by one. Well, I can easily write a function for that:

```elm
incrementX : Point -> Point
incrementX point = { point | x <- point.x + 1 }
```

Not terrible, though a *little* verbose. Still, what if we want to also add a function that *decrements* `x`?

```elm
decrementX : Point -> Point
decrementX point = { point | x <- point.x - 1 }
```

Oh, gosh. That's basically the exact same definition but with the operation flipped. Plus we probably want these operations for `y`, too. Fortunately, there's an easy solution: just pass a function in to *transform* the value! We can define an `updateX` function that allows us to do that easily, then we can define our derived operations in terms of that:

```elm
updateX : (Float -> Float) -> Point -> Point
updateX f point = { point | x <- f point.x }

incrementX : Point -> Point
incrementX = updateX ((+) 1)

decrementX : Point -> Point
decrementX = updateX (\x -> x - 1)
```

Not only is that much cleaner, but we can now use it to implement all sorts of other operations that allow us to add, subtract, multiply, or divide the `x` field. Now we just need to generalize our solution to work with the `x` *and* `y` fields!

Oh, wait. **We can't.**

# The ugly

This is where everything breaks down completely. Elm does not offer enough abstraction to reduce this level of crazy duplication:

```elm
updateX : (Float -> Float) -> Point -> Point
updateX f point = { point | x <- f point.x }

incrementX : Point -> Point
incrementX = updateX ((+) 1)

decrementX : Point -> Point
decrementX = updateX (\x -> x - 1)

updateY : (Float -> Float) -> Point -> Point
updateY f point = { point | y <- f point.y }

incrementY : Point -> Point
incrementY = updateY ((+) 1)

decrementY : Point -> Point
decrementY = updateY (\x -> x - 1)
```

We sure can give it a shot, though. At the very least, we *can* implement the increment and decrement functions in a more general way by passing in an updater function:

```elm
increment : ((Float -> Float) -> a -> a) -> a -> a
increment update = update ((+) 1)
```

Now, with `updateX` and `updateY`, we can increment either field very clearly and expressively. If we shorten the names to `uX` and `uY`, then the resulting code is actually very readable:

```elm
pointAbove = uY (\x -> x + 1)
pointBelow = uY (\x -> x - 1)
```

It's almost like English now: "update Y using this transformation". This is actually pretty satisfactory. The trouble arises when you have a struct with many fields:

```elm
type alias PlayerStats =
  { health : Integer
  , strength : Integer
  , charisma : Integer
  , intellect : Integer
  -- etc.
  }
```

It might be very convenient to have generic functional updaters in this case. One could imagine a game that has `Potion` items:

```elm
type Potion = Potion String (PlayerStats -> PlayerStats)
```

And then some different kinds of potions:

```elm
potions =
  [ (Potion "Health Potion" (uHealth ((+) 1))),
  , (Potion "Greater Intellect Potion" (uIntellect ((+) 3)))
  , (Potion "Potion of Weakness" (uStrength (\x -> x // 5)))
  ]
```

This is a really elegant way to think about items that can affect a player's stats! Unfortunately, it also means you have to define updater functions for *every single field in the record*. This can get tedious rather quickly:

```elm
uHealth : (Integer -> Integer) -> PlayerStats -> PlayerStats
uHealth f stats = { stats | health <- f stats.health }

uStrength : (Integer -> Integer) -> PlayerStats -> PlayerStats
uStrength f stats = { stats | strength <- f stats.strength }

uCharisma : (Integer -> Integer) -> PlayerStats -> PlayerStats
uCharisma f stats = { stats | charisma <- f stats.charisma }

-- etc.
```

This is pretty icky. Could there be a better way?

# Trying to create a more general abstraction

Interestingly, this pattern doesn't *need* to be this bad. There are better ways to do this. Let's revisit our updater functions.

Really, `update` can be defined in terms of two other primitive operations: a read and a (functional) write. What would it look like if we implemented it that way instead of requiring special updater functions to be defined? Well, it would look like this:

```elm
update : (a -> b) -> (b -> a -> a) -> (b -> b) -> a -> a
update get set f x = set (f (get x)) x
```

The type definition is a little long, but it's really pretty simple. We just supply a getter and a setter, then a function to do the transformation, and finally a record to actually transform. Of course, as you can see from the type, this function isn't actually specific to records: it can be used with any value for which a getter and setter can be provided.

The trouble here is that writing field setters isn't any easier in Elm than writing field updaters. They still look pretty verbose:

```elm
sHealth : Integer -> PlayerStats -> PlayerStats
sHealth x stats = { stats | health <- x }

uHealth : (Integer -> Integer) -> PlayerStats -> PlayerStats
uHealth = update .health sHealth
```

So, at the end of it all, this isn't really a better abstraction. Still remember my fantasy `!field` setter shorthand half a blog post ago? Now perhaps it makes a little more sense. *If* such a syntax existed, then defining the updater would be incredibly simple:

```elm
uHealth : (Integer -> Integer) -> PlayerStats -> PlayerStats
uHealth = update .health !health
```

Still, no syntax, no easy updaters, and by extension, no easy, declarative description of behavior without quite a bit of boilerplate.

# Conclusions and related work

Elm is a very promising language, and it seems to be in fairly rapid development. So far, its author, [Evan Czaplicki][czaplic], has taken a very cautious approach to implementing language features, especially potentially redundant ones. This caution is why things like operator slicing, "where" clauses, and special updater syntax have not yet made it into the language. Maybe at some point these will be deemed important enough to include, but for the time being, they've been excluded.

I obviously think that having this sort of thing is incredibly important to being able to write expressive code without a huge amount of overhead. However, I also do *not* want to give the impression that I think adding special setter syntax is the only way to do it.

Seasoned functional programmers will surely have noticed that many of these concepts sound a lot like lenses, and Elm actually already has a lens-like library authored by Evan himself, called [Focus][focus]. This, however, does not actually solve the problem: it requires manual description of setters just like the purely function based approach does. Really, lenses are just the logical next step in the line of abstraction I've already laid out above.

Interestingly, PureScript and Elm, the two Haskell-likes-on-the-frontend that I've paid attention to (though PureScript is much closer to Haskell than Elm), both have this very same problem. Haskell itself solves it with macros via Template Haskell. My favorite language, Racket, solves it with its own macro system. Is there another way to do these things that doesn't involve introducing a heavyweight macro system? Definitely. But I think this is a *necessary feature*, not a "nice to have", so if a macro system is out of the picture, then a simpler, less flexible solution is the obvious logical alternative.

I really like Elm, and most of my experiences with it have been more than enough to convince me that it is a fantastic language for the job. Unfortunately, the issue of functional record updaters has been quite the frustrating obstacle in my otherwise frictionless ride. I will continue to happily use Elm over other, far less accommodating tools, but I hope that issues like these will be smoothed out as the language and its ecosystem matures.

[czaplic]: https://twitter.com/czaplic
[elm-lang]: http://elm-lang.org
[focus]: https://github.com/evancz/focus
[frp]: https://en.wikipedia.org/wiki/Functional_reactive_programming

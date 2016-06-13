    Title: Four months with Haskell
    Date: 2016-06-12T16:02:27
    Tags: haskell, functional programming

At the end of January of this year, I switched to a new job, almost exclusively because I was enticed by the idea of being able to write Haskell. The concept of using such an interesting programming language every day instead of what I’d been doing before (mostly Rails and JavaScript) was very exciting, and I’m pleased to say that the switch seems to have been well worth it.

Haskell was a language I had played with in the past but never really used for anything terribly practical, but lately I think I can confidently say that it really is an *incredible* programming language. At the same time, it has some significant drawbacks, too, though probably not the ones people expect. I certainly wasn’t prepared for some of the areas where Haskell would blow me away, nor was I capable of realizing which parts would leave me hopelessly frustrated until I actually sat down and started writing lots and lots of code.

<!-- more -->

# Dispelling some myths

Before moving on and discussing my experiences in depth, I want to take a quick detour to dispel some frequent rumors I hear about why Haskell is at least potentially problematic. These are things I hear a *lot*, and nothing in my experience so far would lead me to believe these are actually true. Ultimately, I don’t want to spend too much time on these—I think that, for the most part, they are nitpicks that people complain about to avoid understanding the deeper and more insidious problems with the language—but I think it’s important to at least mention them.

## Hiring Haskell developers is not hard

I am on the first Haskell team in my company, and I am among the first Haskell developers we ever hired. Not only were we hiring without much experience with Haskell at all, we explicitly *did not* want to hire remote. Debate all you like about whether or not permitting remote work is a good idea, but I don’t think anyone would dispute that this constraint makes hiring much harder. We didn’t have any trouble finding a very large stream of qualified applicants, and it definitely seems to have dispelled any fears that we would have trouble finding new candidates in the future.

## Performing I/O in Haskell is easy

Haskell’s purity is a point of real contention, and it’s one of the most frustrating complaints I often hear about Haskell. It is surprisingly common to hear concerns along the lines of “I don’t want to use Haskell because its academic devotion to purity sounds like it would make it very hard to get anything done”. There are very valid reasons to avoid Haskell, but in practice, I/O is not one of them. In fact, I found that isolating I/O in Haskell was much the same as isolating I/O in every other language, which I need to do anyway to permit unit testing.

...you *do* write deterministic unit tests for your impure logic, right?

## Working with lots of monads is not very difficult

The “M word” has ended up being a running joke *about* Haskell that actually ends up coming up fairly rarely *within* the Haskell community. To be clear, there is *no doubt* in my mind that monads make Haskell intimidating and provide a steep learning curve for new users. The proliferation of the joke that monads are impossible to explain, to the point of becoming mythologized, is absolutely indicative of a deeper problem about Haskell’s accessibility. However, once people learn the basics about monads, I’ve found that applying them is just as natural as applying any other programming pattern.

Monads are used to assist the programmer, not impede them, and they really do pay off in practice. When something has a monadic interface, there’s a decent chance I already know what that interface is going to do, and that makes working with lots of different monads surprisingly easy. Admittedly, I do rely very, very heavily on tooling to help me out here, but with things like mouseover type tooltips, I’ve actually found that working with a variety of different monads and monad transformers is actually quite pleasant, and it makes things very readable!

# Haskell: the good parts

With the disclaimers out of the way, I really just want to gush for a little bit. This is not going to be an objective, reasoned survey of why Haskell is good. I am not even really going to touch upon why types are so great and why purity is so wonderful—I’d love to discuss those in depth, but that’s for a different blog post. For now, I just want to touch upon the real surprises, the real things that made me *excited* about Haskell in ways I didn’t expect. These are the things that my subjective little experience has found fun.

## Language extensions *are* Haskell

There was a time in my life when I spent a lot of time writing C. There are a lot of compilers for C, and they all implement the language in subtly different but often incompatible ways, especially on different platforms. The only way to maintain a modicum of predictability was to adhere to the standards *religiously*, even when certain GCC or MSVC extensions seem tantalizingly useful. I was actually bitten a few times by real instances where I figured I’d just use a harmless extension that was implemented everywhere, then found out it worked slightly differently across different compilers in a particular edge case. It was a learning experience.

It seems that this fear provides a very real distrust for using GHC’s numerous *language extensions*, and indeed, for a long time, I felt that it was probably an admirable goal to stick to Haskell 98 or Haskell 2010 as closely as possible. Sometimes I chose a slightly more verbose solution that was standard Haskell to avoid turning on a trivial extension that would make the code look a little bit cleaner.

About a year later, I’m finding that attitude was not only a mistake, but it forced me to often completely miss out on a lot of Haskell’s core value. GHC *won*, and now GHC and Haskell are basically synonymous. With that in mind, the portability concerns of language extensions are a bit of a non-issue, and turning them on is a very good idea! Some extensions are more than a little dangerous, so they cannot all be turned on without thinking, but the question is absolutely not “Is using language extensions a good idea?” and more “Is using *this* language extension a good idea?”

This is important, and I bring it up for a reason: so much of the awesomeness of Haskell is locked behind language extensions. Turning a lot of these on is one of the main things that made me really start to see how incredibly powerful Haskell actually is.

## Phantom types

I’m going to start out by talking about *phantom types*, which are a pretty simple concept but a powerful one, and they serve as the foundation for a lot of other cool type-level tricks that can make Haskell extremely interesting. The basic idea of a phantom type is simple; it’s a type parameter that isn’t actually used to represent any particular runtime value:

```haskell
newtype Id a = Id Text
```

This type represents an id for some kind of value, but although the the kind of value is specified in the type as the `a` type parameter, it isn’t actually used anywhere on the data definition—no matter what `a` is, an `Id` is just a piece of text. This makes it possible to write functions that operate on specific kinds of ids, and those invariants will be statically checked by the compiler, even though the runtime representation is entirely identical:

```haskell
fetchUser :: MonadDB m => Id User -> m User
```

Using `FlexibleInstances`, it’s also possible to create different instances for different kinds of ids. For example, it would be possible to have different `Show` instances depending on the type of id in question.

```haskell
instance Show (Id User) where
  show (Id txt) = "user #" <> unpack txt

instance Show (Id Post) where
  show (Id txt) = "post #" <> unpack txt
```

This provides a simple framework for encoding entirely arbitrary information information into the type system, then asking the compiler to actually check assertions about that information. This is made even more powerful with some other extensions, which I’ll talk about shortly.

## Letting the compiler write code

One of the things I really dislike, more than most things, is boilerplate. A little bit of boilerplate is fine—even necessary at times—but as soon as I start wondering if a code generator would improve things, I think the programming language has pretty much failed me.

I write a lot of Racket because, in a sense, Racket is the ultimate boilerplate killer: the macro system is a first-class code generator integrated with the rest of the language, and it means that boilerplate is almost never an issue. Of course, that’s not always true: sometimes a bit of boilerplate *is* still necessary because macros cannot deduce enough information about the program to generate the code entirely on their own, and in Haskell, some of that information is actually present in the type system.

This leads to two absolutely incredible extensions, both of which are simple and related, but which actually *completely change* how I approach problems when programming. These two extensions are `GeneralizedNewtypeDeriving` and `StandaloneDeriving`.

### Newtypes and type safety

The basic idea is that “newtypes” are just simple wrapper types in Haskell. This turns out to be extremely important when trying to find the value of Haskell because they allow you to harden type safety by specializing types to *your* domain. For example, consider a type representing a user’s name:

```haskell
newtype Name = Name Text
```

This type is extremely simple, and in fact isn’t even at all different from a simple `Text` value with respect to its representation, since all combinations of unicode characters are allowed in a name. Therefore, what’s the point of a separate type? Well, this allows Haskell to introduce actual compilation failures when two different kinds of textual data are mixed. This is not a new idea, and even in languages that don’t support this sort of thing, Joel Spolsky’s old blog post [Making Wrong Code Look Wrong][spolsky-wrong] describes how it can be done by convention. Still, almost every modern language makes this possible: in C, it would be a single-member `struct`, in class-based OO languages, it would be a single-member class... this is not a complicated idea.

The difference lies in its usage. In other languages, this strategy is actually not very frequently employed for the simple reason that it is almost always extremely annoying. You are forced to do tons of wrapping/unwrapping, and at that point it isn’t really clear if you’re even getting all that much value out of the distinction when your first solution to a type mismatch is wrapping or unwrapping the value without a second thought. In Haskell, however, this can be heavily mitigated by asking the compiler to *automatically derive typeclass implementations*, which allow the unwrapping/wrapping to effectively happen implicitly for a constrained set of operations.

### Using `GeneralizedNewtypeDeriving`

Consider the `Name` type once again, but this time, let’s derive a class:

```haskell
newtype Name = Name Text
  deriving (IsString)
```

The `IsString` typeclass in Haskell allows custom types to automatically be created from string literals. It is *not* handled specially by Haskell’s `deriving` mechanism. Since `Text` implements `IsString`, an instance will be generated that simply defers to the underlying type, automatically generating the code to wrap the result up in a `Name` box at the end. This means that code like this will now just magically work:

```haskell
name :: Name
name = "Alyssa P. Hacker"
```

No boilerplate needs to be written! This is a neat trick, but it actually turns out to be far more useful than that simple example in practice. What really makes this functionality shine is when you want to derive *some* kinds of functionality but disallow some others. For example, using the [`text-conversions`][text-conversions] package, it’s possible to do something like this:

```haskell
newtype Id a = Id Text
  deriving (Eq, Show, ToText, ToJSON)
```

This creates an opaque `Id` type, but it automatically generates conversions *to* textual formats. However, it does *not* automatically create `FromText` or `FromJSON` instances, which would be dangerous because decoding `Id`s can potentially fail. It’s then possible to write out those instances manually to preserve a type safety:

```haskell
instance FromText (Maybe (Id a)) where
  fromText str = if isValidId str then Just (Id str) else Nothing

instance FromJSON (Id a) where
  parseJSON (String val) = maybe (fail "invalid id") return (fromText val)
  parseJSON _            = fail "invalid id"
```

### Using `StandaloneDeriving`

The ordinary `deriving` mechanism is extremely useful, especially when paired with the above, but sometimes it is desirable to have a little bit more flexibility. In these cases, `StandaloneDeriving` can help.

Take the `Id` example again: it has a phantom type, and simply adding something like `deriving (ToText)` with derive `ToText` instances for *all* kinds of ids. It is potentially useful, however, to derive instances for more specific id types. Using standalone `deriving` constructs permits this sort of flexibility.

```haskell
deriving instance ToText (Id User)

instance ToText (Id Post) where
  toText = postIdToText
```

This is an example where GHC language extensions end up becoming significantly more than the sum of their parts, which seems to be a fairly frequent realization. The `StandaloneDeriving` mechanism is a little bit useful without `GeneralizedNewtypeDeriving`, but when combined, they are incredibly powerful tools for getting a very fine-grained kind of type safety *without* writing any boilerplate.

## DataKinds are super cool, with caveats

Phantom types are quite wonderful, but they can only encode *types*, not arbitrary data. That’s where `DataKinds` and `KindSignatures` come in: they allow lifting arbitrary datatypes to the type level so that things that would normally be purely runtime values can be used at compile-time as well.

The way this works is pretty simple—when you define a datatype, you also define a “datakind”:

```haskell
data RegistrationStatus = Registered | Anonymous
```

Normally, the above declaration declares a *type*, `RegistrationStatus`, and two *data constructors*, `Registered` and `Anonymous`. With `DataKinds`, it also defines a *kind*, `RegistrationStatus`, and two *type constructors*, `Registered` and `Anonymous.`

If that’s confusing, the way to understand that is to realize there is a sort of natural ordering here: types describe values, and kinds describe types. Therefore, turning on `DataKinds` “lifts” each definition by a single level, so types become kinds and values become types. This permits using these things at the type level:

```haskell
newtype UserId (s :: RegistrationStatus) = UserId Text
```

In this example, `UserId` still has a single phantom type variable, `s`, but this time it is constrained to the `RegistrationStatus` kind. Therefore, it can *only* be `Registered` or `Anonymous`. This cooperates well with the aforementioned `StandaloneDeriving` mechanism, and it mostly provides a convenient way to constrain type variables to custom kinds.

In general, `DataKinds` is a much more powerful extension, allowing things like type-level natural numbers or strings, which can be used to perform actual type-level computation (especially in combination with `TypeFamilies`) or a sort of metaprogramming. In some cases, they can even be used to implement things emulating things you can do with dependent types.

I think `DataKinds` are a very cool Haskell extension, but there are a couple caveats. One of the main ones is how new kinds are defined: `DataKinds` “hijacks” the existing datatype declaration syntax by making every single datatype declaration define a type *and* a kind. This is a little confusing, and it would be nice if a different syntax was used so that each could be defined independently.

Similarly, it seems that a lot of work is being done to allow using runtime values at the type level, but I wonder if people will ever need to use, say, runtime values at the *kind* level. This immediately evokes thoughts of Racket’s phase-based macro system, and I wonder if some of this duplication would be unnecessary with something similar.

Food for thought, but overall, `DataKinds` are a very nice addition to help with precisely and specifically typing particular problems.

## Typeclasses can emulate effects

This is something that I’ve found interesting in my time writing Haskell because I have *no idea* if it’s idiomatic or not, but it seems pretty powerful. The initial motivator for this idea was figuring out how to test our code without constantly dropping into `IO`.

More generally, we wanted to be able to unit test by “mocking” out collaborators, as it would be described in object oriented programming. I was always semi-distrustful of mocking, and indeed, it seems likely that it is heavily abused in certain circles, but I’ve come to appreciate the need that sometimes it is important to stub things out, *even in pure code*.

As an example, consider some code that needs access to the current time. This is something that would normally require `IO`, but we likely want to be able to use the value in a pure context without “infecting” the entire program with `IO` types. In Haskell, I have generally seen three ways of handling this sort of thing:

  1. Just inject the required values into the function and produce them “higher up” where I/O is okay. If threading the value around becomes too burdensome, use a Reader monad.

  2. Use a free monad or similar to create a pure DSL of sorts, then write interpreters for various implementations, one of which uses `IO`.

  3. Create custom monadic typeclasses that provide interfaces to the functionality you want to perform, then create instances, one of which is an instance over `IO`.

This last approach seems to be less common in Haskell, but it’s the approach we took, and it seems to work out remarkably well. Returning to the need to get the current time, we could pretty easily write such a typeclass to encode that need:

```haskell
class Monad m => CurrentTime m where
  getCurrentTime :: m UTCTime
```

Now we can write functions that use the current time:

```haskell
validateToken :: CurrentTime m => Token -> m Bool
validateToken tok = do
  currentTime <- getCurrentTime
  return (tokenExpirationDate tok > currentTime)
```

Now, we can write instances for `CurrentTime` that will allow us to run the same code in different contexts:

```haskell
newtype AppM a = AppM { runAppM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

runTestM :: TestM a -> a
runTestM (TestM x) = runIdentity x

instance CurrentTime AppM where
  getCurrentTime = liftIO Data.Time.Clock.getCurrentTime

instance CurrentTime TestM where
  getCurrentTime = return $ posixSecondsToUTCTime 0
```

Where this really starts to shine is when adding additional effects. For example, the above token validation function might also need information about some kind of secret used for signing. Under this model, it’s just another typeclass:

```haskell
class Monad m => TokenSecret m where
  getTokenSecret :: m Secret

validateToken :: (CurrentTime m, TokenSecret m) => Token -> m Bool
validateToken tok = do
  currentTime <- getCurrentTime
  secret <- getTokenSecret
  return (tokenExpirationDate tok > currentTime
       && verifySignature tok secret)
```

Of course, so far all of these functions have been extremely simple, and we’ve basically been using them as a glorified reader monad. In practice, though, we use this pattern for lots more than just retrieving values. For example, we might have a typeclass for database interactions:

```haskell
class Monad m => Persistence m where
  fetchUser :: Id User -> m (Maybe User)
  insertUser :: User -> m (Either PersistenceError (Id User))
```

With all of this done, it becomes incredibly easy to see which functions are using which effects:

```haskell
postUsers
  :: (CurrentTime m, Persistence m, TokenSecret m)
  => User -> m Response
postUsers = ...

getHealthcheck
  :: CurrentTime m
  => m Response
getHealthcheck = ...
```

There’s no need to perform any lifting, and this all seems to scale quite nicely. We’ve written some additional utilities to help write tests against functions using these kinds of monadic interfaces, and even though there’s a little bit of annoying boilerplate in a few spots, overall it seems to work quite elegantly.

I’m not entirely sure how common this is in the Haskell community, but it’s certainly pretty neat how easy it is to get nearly all of the benefits of effect types in other languages simply by composing some of Haskell’s simplest features.

## Atom’s ide-haskell tooling is invaluable

Alright, so, confession time: I don’t use Emacs.

I know, I know, how is that possible? I write Lisp, after all. Well, honestly, I tried picking it up a number of times, but none of those times did I get far enough to ditch my other tools. For Racket work, I use DrRacket, but for almost everything else, I use Atom.

Atom has a lot of flaws, but it’s also pretty amazing in places, and I absolutely *love* the Haskell tooling written by the wonderful [atom-haskell][] folks. I use it constantly, and even though it doesn’t always work perfectly, it works pretty well. When it has problems, I’ve at least figured out how to get it working correctly.

This is probably hard to really explain without seeing it for yourself, but I’ve found that I basically *depend* on this sort of tooling to be fully productive in Haskell, and I have no problem admitting that. The ability to get instant feedback about type errors tied to visual source locations, to be able to directly manipulate the source by selecting expressions and getting type information, and even the option to get inline linter suggestions means I spend a lot less time glancing at the terminal, and even less time in the REPL.

The tooling is far from perfect, and it leaves a lot to be desired in places (the idea of using that static information for automated, project-wide refactoring *a la* Java is tantalizing), but most of those things are ideas of what amazing things could be, not broken or missing essentials. I am pretty satisfied with ide-haskell right now, and I can only hope it continues to get better and better.

# Frustrations, drawbacks, and pain points

Haskell is not perfect. In fact, far from it. There is a vast array of little annoyances that I have with the language, as is the case with any language. Still, there are a few overarching problems that I would really like to at least mention. These are the biggest sources of frustration for me so far.

## Purity, failure, and exception-handling

One of Haskell’s defining features is its purity—I don’t think many would disagree with that. Some people consider it a drawback, others consider it one of its greatest boons. Personally, I like it a lot, and I think one of the best parts about it is how it requires the programmer to be incredibly deliberate about failure.

In many languages, when looking up a value from a container where the key doesn’t exist, there are really two ways to go about expressing this failure:

  1. Throw an exception.
  2. Return `null`.

The former is scary because it means *any* call to any function can make the entire program blow up, and it’s often impossible to know which functions even have the potential to throw. This creates a certain kind of non-local control flow that can sometimes cause a lot of unpredictability. The second option is much the same, especially when any value in a program might be `null`; it just defers the failure.

In languages with option types, this is somewhat mitigated. Java now has option types, too, but they are still frequently cumbersome to use because there is nothing like monads to use to simply chain operations together. Haskell, in comparison, has an incredible complement of tools to simply handle errors without a whole lot of burden on the programmer, and I have found that, in practice, this is *actually helpful* and I really do write better error-handling code.

### First, the good parts

I have seen a comparison drawn between throwing checked exceptions and returning `Maybe` or `Either` types, but in practice the difference is massive. Handling checked exceptions is a monotonous chore because they are not first-class values, they are actually entirely separate linguistic constructs. Consider a library that throws a `LibraryException`, and you want to wrap that library and convert those exceptions to `ApplicationException`s. Well, have fun writing this code dozens of times:

```java
try {
  x = doSomething();
} catch (LibraryException ex) {
  throw ApplicationException.fromLibraryException(ex);
}

// ...

try {
  y = doSomethingElse();
} catch (LibraryException ex) {
  throw ApplicationException.fromLibraryException(ex);
}
```

In Haskell, failure is just represented by first-class values, and it’s totally possible to write helper functions to abstract over that kind of boilerplate:

```haskell
libraryToApplication :: LibraryError -> ApplicationError
libraryToApplication = ...

liftLibrary :: Either LibraryError a -> Either ApplicationError a
liftLibrary = mapLeft libraryToApplication
```

Now, that same boilerplate-y code becomes nearly invisible:

```haskell
x <- liftLibrary doSomething

-- ...

y <- liftLibrary doSomethingElse
```

This might not *seem* like much, but it really cuts down on the amount of visual noise, which ends up making all the difference. Boilerplate incurs a cost much bigger than simply taking the time to type it all out (though that’s important, too): the cognitive overhead of parsing which parts of a program are boilerplate has a significant impact on readability.

### So what’s the problem?

If error handling is so great in Haskell, then why am I putting it under the complaints section? Well, it turns out that not everyone seems to think it’s as great as I make it out to be because people seem to keep writing Haskell APIs that throw exceptions!

Despite what some purists would have you believe, Haskell has exceptions, and they are not uncommon. Lots of things can throw exceptions, some of which are probably reasonable. Failing to connect to a database is a pretty catastrophic error, so it seems fair that it would throw. On the other hand, inserting a duplicate record is pretty normal operation, so it seems like that should *not* throw.

I mostly treat exceptions in Haskell as unrecoverable catastrophes. If I throw an error in *my* code, I do not intend to catch it. That means something horrible happened, and I just want that horribleness to show up in a log somewhere so I can fix the problem. If I care about failure, there are better ways to handle that failure gracefully.

It’s also probably worth noting that exceptions in Haskell can be thrown from anywhere, even pure code, but can only be *caught* within the `IO` monad. This is especially scary, but I’ve seen it happen in actual libraries out in the wild, even ones that the entire Haskell ecosystem is built on. One of the crowning examples of this is the `text` package, which provides a function called `decodeUtf8` to convert bytestrings into text. Its type is very simple:

```haskell
decodeUtf8 :: ByteString -> Text
```

But wait, what if the bytestring is not actually a valid UTF-8 string?

Boom. There goes the application.

Okay, okay, well, at least the `text` package provides another function, this one called `decodeUtf8'`, which returns an `Either`. This is good, and I’ve trained myself to only ever use `decodeUtf8'`, but it still has some pretty significant problems:

  - The *safe* version of this function is the “prime” version, rather than the other way around, which encourages people to use the unsafe one. Ideally, the unsafe one should be explicitly labeled as such... maybe call it `unsafeDecodeUtf8`?

  - This is not a hypothetical problem. When using a Haskell JWT library, we found a function that converts a string into a JWT. Since not all strings are JWTs, the library intelligently returns a `Maybe`. Therefore, we figured we were safe.

    A couple weeks later, we found that providing this function with invalid data was returning HTTP 500 errors. Why? Our error handling was meticulous! Well, the answer was a `decodeUtf8` call, hidden inside of the JWT library. This is especially egregious, given that the API it exposed returned a `Maybe` anyway! It would have been trivial to use the safe version there, instead, but the poor, misleading name led the library developer to overlook the bug lurking in the otherwise innocuous function.

    Even worse, this function was totally pure, and we used it in pure code, so we could not simply wrap the function and catch the exception. We had two options: use `unsafePerformIO` (yuck!) or perform a check before handing the data to the buggy function. We chose the latter, but in some cases, I imagine that could be too difficult to do in order to make it feasible.

The point I’m trying to make is that this is a real problem, and it seems to me that throwing exceptions invalidates one of the primary advantages of Haskell. It disappointed me to realize that a significant amount of code written by FP Complete, one of the primary authors of some of the most important “modern Haskell” code in existence (including Stack), seem to very frequently expose APIs that will throw.

I’m not sure how much of this stems from a fundamental divide in the Haskell ecosystem and how much it is simply due to Michael Snoyman’s coding style, given that he is the primary author of a number of these tools and libraries that seem very eager to throw exceptions. As just one example of a real situation in which we were surprised by this behavior, we used Snoyman’s http-client library and found that it mysteriously throws upon nearly *any* failure state:

> A note on exceptions: for the most part, all actions that perform I/O should be assumed to throw an `HttpException` in the event of some problem, and all pure functions will be total. For example, `withResponse`, `httpLbs`, and `BodyReader` can all throw exceptions.

This doesn’t seem entirely unreasonable—after all, isn’t a failure to negotiate TLS fairly catastrophic?—until you consider our use case. We needed to make a subrequest during the extent of another HTTP request to our server, and if that subrequest fails, we absolutely need to handle that failure gracefully. Of course, this is not *terrible* given that we are in `IO` so we can actually catch these exceptions, but since this behavior was only noted in a single aside at the top of the documentation, we didn’t realize we were forgetting error handling until far too late and requests were silently failing.

Exceptions seem to devalue one of the most powerful concepts in Haskell: if I don’t consider all the possibilities, my code *does not compile*. In practice, when working with APIs that properly encode these possibilities into the type system, this value proposition seems to be real. I really do find myself writing code that works correctly as soon as it compiles. It’s almost magical.

Using exceptions throws that all out the window, and I wish the Haskell ecosystem was generally more cautious about when to use them.

## The String problem

I sort of alluded to this a tiny bit in the last section, and that is probably indicative of how bad this issue is. I’m just going to be blunt: **In Haskell, strings suck.**

This is always a bit of an amusing point whenever it is discussed because of how silly it seems. Haskell is a research language with a cutting-edge type system and some of the fanciest features of any language in existence. When everyday programming might use things like “profunctors”, “injective type families”, and “generalized algebraic datatypes”, you would think that dealing with *strings* would be a well-solved problem.

But it isn’t. Haskell libraries frequently use not one, not two, but ***five*** kinds of strings. Let’s list them off, shall we?

  - First off, there’s the built-in `String` type, which is actually an alias for the `[Char]` type. For those not intimately familiar with Haskell, that’s a *linked list of characters*. As [Stephen Diehl][stephendiehl] recently put it in [a blog post describing the disaster that is Haskell string types](http://www.stephendiehl.com/posts/strings.html):

    > This is not only a bad representation, it’s quite possibly the least efficient (non-contrived) representation of text data possible and has horrible performance in both time and space. *And it’s used everywhere in Haskell.*

    The point is, it’s really bad. This type is not a useful representation for textual data in practical applications.

  - Moving on, we have a fairly decent type, `Text`, which comes from `Data.Text` in the `text` package. This is a decent representation of text, and it’s probably the one that everything should use. Well, maybe. Because `Text` comes in two varieties: lazy and strict. Nobody seems to agree on which of those two should be used, though, and they are totally incompatible types: functions that work with one kind of text won’t work with the other. You have to manually convert between them.

  - Finally, we have `ByteString`, which is horribly misnamed because it really isn’t a string at all, at least not in the textual sense. A better name for this type would have simply been `Bytes`, which sounds a lot scarier. And that would be good, because data typed as a `ByteString` is as close as you can get in Haskell to not assigning a type at all: a bytestring holds arbitrary bytes without assigning them any meaning whatsoever!

    Or at least, that’s the intention. The trouble is that people *don’t* treat bytestrings like that—they just use them to toss pieces of text around, even when those pieces of text have a well-defined encoding and represent textual data. This leads to the `decodeUtf8` problem mentioned above, but it’s bigger than that because it often ends up with some poor APIs that assign some interpretation to `ByteString` data without assigning it a different type.

    Again, this is throwing away so much of Haskell’s safety. It would be like using `Int` to keep track of boolean data (“just use 0 and 1!”) or using empty and singleton lists instead of using `Maybe`. When you use the precise type, you encode invariants and contracts into statically-checked assertions, but when you use general types like `ByteString`, you give that up.

    Oh, and did I mention that `ByteString`s also come in incompatible lazy and strict versions, too?

So, obviously, the answer is to just stop using the bad types and to just use (one kind of) `Text` everywhere. Great! Except that the other types are totally inescapable. The entire standard library uses `String` exclusively—after all, `text` is a separate package—and small libraries often use `String` instead of `text` because they have no need to bring in the dependency. Of course, this just means every real application pays the performance hit of converting between all these different kinds of strings.

Similarly, those that *do* use `Text` often use different kinds of text, so code ends up littered with `fromStrict` or `toStrict` coercions, which (again) have a cost. I’ve already ranted enough about `ByteString`, but basically, if you’re using `ByteString` in your API to pass around data that is semantically text, you are causing me pain. Please stop.

It seems that the way `Data.Text` probably *should* have been designed was by making `Text` a typeclass, then making the lazy and strict implementations instances of that typeclass. Still, the fact that both of them exist would always cause problems. I’m actually unsure which one is the “correct” choice—I don’t know enough about how the two perform in practice—but it seems likely that picking *either* one would be a performance improvement over the current system, which is constantly spending time converting between the two.

This issue has been ranted about plenty, so I won’t ramble on, but if you’re designing new libraries, please, *please* use `Text`. Your users will thank you.

## Documentation is nearly worthless

Finally, let’s talk about documentation.

One of my favorite programming languages is Racket. Racket has a documentation tool called Scribble. Scribble is special because it is a totally separate domain-specific language for writing documentation, and it makes it fun and easy to write good explanations. There are even forms for typesetting automatically-rendered examples that look like a REPL. If the examples ever break or become incorrect, the docs don’t even compile.

All of the Racket core library documentation makes sure to set a good example about what good documentation should look like. The vast majority of the documentation is paragraphs of prose and simple but practical examples. There are also type signatures (in the form of contracts), and those are super important, but they are so effective because of how the prose explains what each function does, when to use it, *why* you’d use it, and *why you wouldn’t use it*.

Everything is cross-referenced automatically. The documentation is completely searchable locally out of the box. As soon as you install a package, its docs are automatically indexed. User-written libraries tend to have pretty good docs, too, because the standard libraries set such a good example *and* because the tools are so fantastic. Racket docs are really nice, and they’re so good they actually make things like Stack Overflow or even Google mostly irrelevant. It’s all there in the manual.

Haskell documentation is the opposite of everything I just said.

  - The core libraries are poorly documented. Most functions include a sentence of description, and almost none include examples. At their worst, the descriptions simply restate the type signature.

  - Third-party libraries’ documentation is even worse, going frequently completely undocumented and actually only including type signatures and nothing else.

  - Haddock is an incredibly user-hostile tool for writing anything other than tiny snippets of documentation and is not very good at supporting prose. Notably, Haddock’s documentation is not generated using Haddock (and it still manages to be almost unusable). Forcing all documentation into inline comments makes users unlikely to write much explanation, and there is no ability for abstraction.

  - Reading documentation locally is very difficult because there is no easy way to open documentation for a particular package in a web browser, and it’s *certainly* not searchable. This is especially ridiculous given that Hoogle exists, which is one of best ways to search API docs in existence. There should be a `stack hoogle` command that just opens a Hoogle page for all locally-installed packages and Just Works, but there isn’t.

  - Most valuable information exists outside of documentation, so Google becomes a go-to immediately after a quick glance at the docs, and information is spread across blog posts, mailing lists, and obscure reddit posts.

This is a problem that cannot be fixed by just making Haddock better, nor can it be fixed simply by improving the existing standard library documentation. There is a fundamental problem with Haskell documentation (which, to be completely fair, is not unique to Haskell), which is that its tools do not support anything more than API docs.

Good documentation is so much more than “here’s what this function does”; it’s about guides and tutorials and case studies and common pitfalls. [This is documentation for someone new to lenses.][racket-lens] [This is not.][haskell-lens] Take note of the difference.

# Conclusion and other thoughts

Haskell is an incredible programming platform, and indeed, it is sometimes mind-boggling how complete it is. It also has a lot of rough edges, sometimes in places that feel like they need a lot more care, or perhaps they’re even simply unfinished.

I could spend weeks writing about all the things I really like or dislike about the language, discussing in fine detail all the things that have made me excited or all the little bits that have made me want to tear my hair out. Heck, I could probably spend a month writing about strings alone. That’s not the point, though... I took a risk with Haskell, and it’s paid off. I’m not yet sure exactly how I feel about it, or when I would chose it relative to other tools, but it is currently very high on my list of favorite technologies.

I did not come to Haskell with a distaste for static typing, despite the fact that I write so much Racket, a dynamically typed language (by default, at least). I don’t really use Typed Racket, and despite my love for Haskell and its type system, I am not sure I will use much more of it than I did before. Haskell and Racket are very different languages, which is justified in some places and probably sort of circumstantial in others.

The future of Haskell seems bright, and a lot of the changes in the just-released GHC 8 are extremely exciting. I did not list records as a pain point because the changes in GHC 8 appear to make them a *lot* more palatable, although whether or not they solve that problem completely remains to be seen. I will absolutely continue to write Haskell and push it to its limits where I can, and hopefully try and take as much as I can from it along the way.

[atom-haskell]: https://github.com/atom-haskell
[haskell-lens]: https://hackage.haskell.org/package/lens#readme
[racket-lens]: http://docs.racket-lang.org/lens/lens-guide.html
[spolsky-wrong]: http://www.joelonsoftware.com/articles/Wrong.html
[stephendiehl]: http://www.stephendiehl.com/
[text-conversions]: https://hackage.haskell.org/package/text-conversions

    Title: Using types to unit-test in Haskell
    Date: 2016-10-03T01:20:43
    Tags: haskell, testing

Object-oriented programming languages make unit testing easy by providing obvious boundaries between units of code in the form of classes and interfaces. These boundaries make it easy to stub out parts of a system to test functionality in isolation, which makes it possible to write fast, deterministic test suites that are robust in the face of change. When writing Haskell, it can be unclear how to accomplish the same goals: even inside pure code, it can become difficult to test a particular code path without also testing all its collaborators.

Fortunately, by taking advantage of Haskell’s expressive type system, it’s possible to not only achieve parity with object-oriented testing techniques, but also to provide stronger static guarantees as well. Furthermore, it’s all possible without resorting to extra-linguistic hacks that static object-oriented languages sometimes use for mocking, such as dynamic bytecode generation.

<!-- more -->

# First, an aside on testing philosophy

Testing methodology is a controversial topic within the larger programming community, and there are a multitude of different approaches. This blog post is about *unit testing*, an already nebulous term with a number of different definitions. For the purposes of this post, I will define a unit test as a test that stubs out collaborators of the code under test in some way. Accomplishing that in Haskell is what this is primarily about.

I want to be clear that I do not think that unit tests are the only way to write tests, nor the best way, nor even always an applicable way. Depending on your domain, rigorous unit testing might not even make sense, and other forms of tests (end-to-end, integration, benchmarks, etc.) might fulfill your needs.

In practice, though, implementing those other kinds of tests seems to be well-documented in Haskell compared to pure, object-oriented style unit testing. As my Haskell applications have grown, I have found myself wanting a more fine-grained testing tool that allows me to both test a piece of my codebase in isolation and also use my domain-specific types. This blog post is about that.

With that disclaimer out of the way, let’s talk about testing in Haskell.

# Drawing seams using types

One of the primary attributes of unit tests in object-oriented languages, especially statically-typed ones, is the concept of “seams” within a codebase. These are internal boundaries between components of a system. Some boundaries are obvious—interactions with a database, manipulation of the file system, and performing I/O over the network, to name a few examples—but others are more subtle. Especially in larger codebases, it can be helpful to isolate two related but distinct pieces of functionality as much as possible, which makes them easier to reason about, even if they’re actually part of the same codebase.

In OO languages, these seams are often marked using interfaces, whether explicitly (in the case of static languages) or implicitly (in the case of dynamic ones). By programming to an interface, it’s possible to create “fake” implementations of that interface for use in unit tests, effectively making it possible to stub out code that isn’t directly relevant to the code being tested.

In Haskell, representing these seams is a lot less obvious. Consider a fairly trivial function that reverses a file’s contents on the file system:

```haskell
reverseFile :: FilePath -> IO ()
reverseFile path = do
  contents <- readFile path
  writeFile path (reverse contents)
```

This function is impossible to test without testing against a real file system. It simply performs I/O directly, and there’s no way to “mock out” the file system for testing purposes. Now, admittedly, this function is so trivial that a unit test might not seem worth the cost, but consider a slightly more complicated function that interacts with a database:

```haskell
renderUserProfile :: Id User -> IO HTML
renderUserProfile userId = do
  user <- fetchUser userId
  posts <- fetchRecentPosts userId

  return $ div
    [ h1 (userName user <> "’s Profile")
    , h2 "Recent Posts"
    , ul (map (li . postTitle) posts)
    ]
```

It might now be a bit more clear that it could be useful to test the above function without running a real database and doing all the necessary context setup before each test case. Indeed, it would be nice if a test could just provide stubbed implementations for `fetchUser` and `fetchRecentPosts`, then make assertions about the output.

One way to solve this problem is to pass the results of those two functions to `renderUserProfile` as arguments, turning it into a pure function that could be easily tested. This becomes obnoxious for functions of even just slightly more complexity, though (it is not unreasonable to imagine needing a handful of different queries to render a user’s profile page), and it requires significantly restructuring code simply because the tests need it.

The above code is not only difficult to test, however—it has another problem, too. Specifically, both functions return `IO` values, which means they can effectively do *anything*. Haskell has a very strong type system for typing terms, but it doesn’t provide any guarantees about effects beyond a simple yes/no answer about function purity. Even though the `renderUserProfile` function should really only need to interact with the database, it could theoretically delete files, send emails, make HTTP requests, or do any number of other things.

Fortunately, it’s possible to solve *both* problems—a lack of testability and a lack of type safety—using the same general technique. This approach is reminiscent of the interface-based seams of object-oriented languages, but unlike most object-oriented approaches, it provides additional type safety guarantees without the need to explicitly modify the code to support some kind of dependency injection.

## Making implicit interfaces explicit

Statically typed, object-oriented languages provide interfaces as a language construct to encode certain kinds of contracts into the type system, and Haskell has something similar. Typeclasses are, in many ways, an analog to OO interfaces, and they can be used in a similar way. In the above case, let’s write down interfaces that the `reverseFile` and `renderUserProfile` functions can use:

```haskell
class Monad m => MonadFS m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

class Monad m => MonadDB m where
  fetchUser :: Id User -> m User
  fetchRecentPosts :: Id User -> m [Post]
```

The really nice thing about these interfaces is that our function implementations don’t have to change *at all* to take advantage of them. In fact, all we have to change is their types:

```haskell
reverseFile :: MonadFS m => FilePath -> m ()
reverseFile path = do
  contents <- readFile path
  writeFile path (reverse contents)

renderUserProfile :: MonadDB m => Id User -> m HTML
renderUserProfile userId = do
  user <- fetchUser userId
  posts <- fetchRecentPosts userId

  return $ div
    [ h1 (userName user <> "’s Profile")
    , h2 "Recent Posts"
    , ul (map (li . postTitle) posts)
    ]
```

This is pretty neat, since we haven’t had to alter our code at all, but we’ve managed to completely decouple ourselves from `IO`. This has the direct effect of both making our code more abstract (we no longer rely on the “real” file system or a “real” database, which makes our code easier to test) and restricting what our functions can do (just from looking at the type signatures, we know what side-effects they can perform).

Of course, since we’re now coding against an interface, our code doesn’t actually do much of anything. If we want to actually use the functions we’ve written, we’ll have to define instances of `MonadFS` and `MonadDB`. When actually running our code, we’ll probably still use `IO` (or some monad transformer stack with `IO` at the bottom), so we can define trivial instances for that existing use case:

```haskell
instance MonadFS IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

instance MonadDB IO where
  fetchUser = SQL.fetchUser
  fetchRecentPosts = SQL.fetchRecentPosts
```

Even if we go no further, **this is already incredibly useful**. By restricting the sorts of effects our functions can perform at the type level, it becomes a lot easier to see which code is interacting with what. This can be invaluable when working in a part of a moderately large codebase that you are unfamiliar with. Even if the only instance of these typeclasses is `IO`, the benefits are immediately apparent.

Of course, this blog post is about testing, so we’re going to go further and take advantage of these seams we’ve now drawn. The question is: how?

# Testing with typeclasses: an initial attempt

Given that we now have functions depending on an interface instead of `IO`, we can create separate instances of our typeclasses for use in tests. Let’s start with the `renderUserProfile` function. We’ll create a simple wrapper around the `Identity` type, since we don’t actually care much about the “effects” of our `MonadDB` methods:

```haskell
import Data.Functor.Identity

newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

unTestM :: TestM a -> a
unTestM (TestM (Identity x)) = x
```

Now, we’ll create a trivial instance of `MonadDB` for `TestM`:

```haskell
instance MonadDB TestM where
  fetchUser _ = return User { userName = "Alyssa" }
  fetchRecentPosts _ = return
    [ Post { postTitle = "Metacircular Evaluator" } ]
```

With this instance, it’s now possible to write a simple unit test of the `renderUserProfile` function that doesn’t need a real database running at all:

```haskell
spec = describe "renderUserProfile" $ do
  it "shows the user’s name" $ do
    let result = unTestM (renderUserProfile (intToId 1234))
    result `shouldContainElement` h1 "Alyssa’s Profile"

  it "shows a list of the user’s posts" $ do
    let result = unTestM (renderUserProfile (intToId 1234))
    result `shouldContainElement` ul [ li "Metacircular Evaluator" ]
```

This is pretty nice, and running the above tests reveals a nice property of these kinds of isolated test cases: the test suite runs *really, really fast*. Communicating with a database, even in extremely simple ways, takes a measurable amount of time, especially with dozens of tests. In contrast, even with hundreds of tests, our unit test suite runs in less than a tenth of a second.

This all seems to be successful, so let’s try and apply the same testing technique to `reverseFile`.

## Testing side-effectful code

Looking at the type signature for `reverseFile`, we have a small problem:

```haskell
reverseFile :: MonadFS m => FilePath -> m ()
```

Specifically, the return type is `()`. Making any assertions against the result of this function would be completely worthless, given that it’s guaranteed to be the same exact thing each time. Instead, `reverseFile` is inherently side-effectful, so we want to be able to test that it properly interacts with the file system in the correct way.

In order to do this, a simple wrapper around `Identity` won’t be enough, but we can replace it with something more powerful: `Writer`. Specifically, we can use a writer monad to “log” what gets called in order to test side-effects. We’ll start by creating a new `TestM` type, just like last time:

```haskell
newtype TestM a = TestM (Writer [String] a)
  deriving (Functor, Applicative, Monad, MonadWriter [String])

logTestM :: TestM a -> [String]
logTestM (TestM w) = execWriter w
```

Using this slightly more powerful type, we can write a useful instance of `MonadFS` that will track the argument given to `writeFile`:

```haskell
instance MonadFS TestM where
  readFile _ = return "hello"
  writeFile _ contents = tell [contents]
```

Again, the instance is quite simple, but it now enables us to write a straightforward unit test for `reverseFile`:

```haskell
spec = describe "reverseFile" $
  it "reverses a file’s contents on the filesystem" $ do
    let calls = logTestM (reverseFile "foo.txt")
    calls `shouldBe` ["olleh"]
```

Again, quite simple to both implement and use, and the test itself is blindingly fast. There’s another problem, though, which is that we have technically left part of `reverseFile` untested: we’ve completely ignored the `path` argument.

In this contrived example, it may seem silly to test something so trivial, but in real code, it’s quite possible that one would care very much about testing multiple different aspects about a single function. When testing `renderUserProfile`, this was not hard, since we could reuse the same `TestM` type and `MonadDB` instance for both test cases, but in the `reverseFile` example, we’ve ignored the path entirely.

We *could* adjust our `MonadFS` instance to also track the path provided to each method, but this has a few problems. First, it means every test case would depend on all the various properties we are testing, which would mean updating every test case when we add a new one. It would also be simply impossible if we needed to track multiple types—in this particular case, it turns out that `String` and `FilePath` are actually the same type, but in practice, there may be a handful of disparate, incompatible types.

Both of the above issues could be fixed by creating a sum type and manually filtering out the relevant elements in each test case, but a much more intuitive approach would be to simply have a separate instance for each case. Unfortunately, in Haskell, creating a new instance means creating an entirely new type. To illustrate how much duplication that would entail, we could create the following type and instance for testing proper propagation of the `path` argument:

```haskell
newtype TestM' a = TestM' (Writer [FilePath] a)
  deriving (Functor, Applicative, Monad, MonadWriter [FilePath])

logTestM' :: TestM' a -> [FilePath]
logTestM' (TestM' w) = execWriter w

instance MonadFS TestM' where
  readFile path = tell [path] >> return ""
  writeFile path _ = tell [path]
```

Now it’s possible to add an extra test case that asserts that the proper path is provided to the two filesystem functions:

```haskell
spec = describe "reverseFile" $ do
  it "reverses a file’s contents on the filesystem" $ do
    let calls = logTestM (reverseFile "foo.txt")
    calls `shouldBe` ["olleh"]

  it "operates on the file at the provided path" $ do
    let paths = logTestM' (reverseFile "foo.txt")
    paths `shouldBe` ["foo.txt", "foo.txt"]
```

This works, but it’s ultimately unacceptably complicated. Our test harness code is now significantly larger than the actual tests themselves, and the amount of boilerplate is frustrating. Verbose test suites are especially bad, since forcing programmers to jump through hoops just to implement a single test reduces the likelihood that people will actually write good tests, if they write tests at all. In contrast, if writing tests is easy, then people will naturally write more of them.

The above strategy to writing tests is not good enough, but it does reveal a particular problem: in Haskell, typeclass instances are not first-class values that can be manipulated and abstracted over, they are static constructs that can only be managed by the compiler, and users do not have a direct way to modify them. With some cleverness, however, we can actually create an approximation of first-class typeclass dictionaries, which will allow us to dramatically simplify the above testing mechanism.

# Creating first-class typeclass instances

In order to provide an easy way to construct instances, we need a way to represent instances as ordinary Haskell values. This is not terribly difficult, given that instances are conceptually just records containing a collection of functions. For example, we could create a datatype that represents an instance of the `MonadFS` typeclass:

```haskell
data MonadFSInst m = MonadFSInst
  { _readFile :: FilePath -> m String
  , _writeFile :: FilePath -> String -> m ()
  }
```

To avoid namespace clashes with the actual method identifiers, the record fields are prefixed with an underscore, but otherwise, the translation is remarkably straightforward. Using this record type, we can easily create values that represent the two instances we defined above:

```haskell
contentInst :: MonadWriter [String] m => MonadFSInst m
contentInst = MonadFSInst
  { _readFile = \_ -> return "hello"
  , _writeFile = \_ contents -> tell [contents]
  }

pathInst :: MonadWriter [FilePath] m => MonadFSInst m
pathInst = MonadFSInst
  { _readFile = \path -> tell [path] >> return ""
  , _writeFile = \path _ -> tell [path]
  }
```

These two values represent two different implementations of `MonadFS`, but since they’re ordinary Haskell values, they can be manipulated and even *extended* like any other records. This can be extremely useful, since it makes it possible to create a sort of “base” instance, then have individual test cases override individual pieces of functionality piecemeal.

Of course, although we’ve written these two instances, we have no way to actually use them. After all, Haskell does not provide a way to explicitly provide typeclass dictionaries. Fortunately, we can create a sort of “proxy” type that will use a reader to thread the dictionary around explicitly, and the instance can defer to the dictionary’s implementation.

## Creating an instance proxy

To represent our proxy type, we’ll use a combination of a `Writer` and a `ReaderT`; the former to implement the logging used by instances, and the latter to actually thread around the dictionary. Our type will look like this:

```haskell
newtype TestM log a =
    TestM (ReaderT (MonadFSInst (TestM log)) (Writer log) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader (MonadFSInst (TestM log))
           , MonadWriter log
           )

logTestM :: MonadFSInst (TestM log) -> TestM log a -> log
logTestM inst (TestM m) = execWriter (runReaderT m inst)
```

This might look rather complicated, and it is, but let’s break down exactly what it’s doing.

  1. The `TestM` type includes two type parameters. The first is the type of value that will be logged (hence the name `log`), which corresponds to the argument to `Writer` from previous incarnations of `TestM`. Unlike those types, though, we want this version to work with any `Monoid`, so we’ll make it a type parameter. The second parameter is simply the type of the current monadic value, as before.

  2. The type itself is defined as a wrapper around a small monad transformer stack, the first of which is `ReaderT`. The state threaded around by the reader is, in this case, the instance dictionary, which is `MonadFSInst`.

     However, recall that `MonadFSInst` accepts a type variable—the type of a monad itself—so we must provide `TestM log` as an argument to `MonadFSInst`. This slight bit of indirection allows us to tie the knot between the mutually dependent instances and proxy type.

  3. The base monad in the transformer stack is `Writer`, which is used to actually implement the logging functionality, just like in prior cases. The only difference now is that the `log` type parameter now determines what the writer actually produces.

  4. Finally, as before, we use `GeneralizedNewtypeDeriving` to derive all the relevant `mtl` classes, adding the somewhat wordy `MonadReader` constraint to the list.

Using this single type, we can now implement a `MonadFS` instance that defers to the dictionary carried around within `TestM`’s reader state:

```haskell
instance Monoid log => MonadFS (TestM log) where
  readFile path = do
    f <- asks _readFile
    f path
  writeFile path contents = do
    f <- asks _writeFile
    f path contents
```

This may seem somewhat boilerplate-y, and it is to some extent, but the important consideration is that this boilerplate only needs to be written *once*. With this in place, it’s now possible to write an arbitrary number of first-class instances that use the above mechanism without extending the mechanism at all.

To see what actually using this code would look like, let’s update the `reverseFile` tests to use the new `TestM` implementation, as well as the `contentInst` and `pathInst` dictionaries from earlier:

```haskell
spec = describe "reverseFile" $ do
  it "reverses a file’s contents on the filesystem" $ do
    let calls = logTestM contentInst (reverseFile "foo.txt")
    calls `shouldBe` ["olleh"]

  it "operates on the file at the provided path" $ do
    let paths = logTestM pathInst (reverseFile "foo.txt")
    paths `shouldBe` ["foo.txt", "foo.txt"]
```

We can do a little bit better, though. Really, the definitions of `contentInst` and `pathInst` are specific to each test case. With ordinary typeclass instances, we cannot scope them to any particular block, but since `MonadFSInst` is just an ordinary Haskell datatype, we can manipulate them just like any other Haskell values. Therefore, we can just inline those instances’ definitions into the test cases themselves to keep them closer to the actual tests.

```haskell
spec = describe "reverseFile" $ do
  it "reverses a file’s contents on the filesystem" $ do
    let contentInst = MonadFSInst
          { _readFile = \_ -> return "hello"
          , _writeFile = \_ contents -> tell [contents]
          }
    let calls = logTestM contentInst (reverseFile "foo.txt")
    calls `shouldBe` ["olleh"]

  it "operates on the file at the provided path" $ do
    let pathInst = MonadFSInst
          { _readFile = \path -> tell [path] >> return ""
          , _writeFile = \path _ -> tell [path]
          }
    let paths = logTestM pathInst (reverseFile "foo.txt")
    paths `shouldBe` ["foo.txt", "foo.txt"]
```

This is pretty good. We’re now able to create inline instances of our `MonadFS` typeclass, which allows us to write extremely concise unit tests using ordinary Haskell typeclasses as system seams. We’ve managed to cut down on the boilerplate considerably, though we still have a couple problems. For one, this example only uses a single typeclass containing only two methods. A real `MonadFS` typeclass would likely have at least a dozen methods for performing various filesystem operations, and writing out the instance dictionaries for every single method, even the ones that aren’t used within the code under test, would be pretty frustratingly verbose.

This problem is solvable, though. Since instances are just ordinary Haskell records, we can create a “base” instance that just throws an exception whenever the method is called:

```haskell
baseInst :: MonadFSInst m
baseInst = MonadFSInst
  { _readFile = error "unimplemented instance method ‘_readFile’"
  , _writeFile = error "unimplemented instance method ‘_writeFile’"
  }
```

Then code that only uses `readFile` could only override that particular method, for example:

```haskell
let myInst = baseInst { _readFile = ... }
```

Normally, of course, this would be a terrible idea. However, since this is all just test code, it can be extremely useful in quickly figuring out what methods need to be stubbed out for a particular test case. Since all the code actually gets run at test time, attempts to use unimplemented instance methods will immediately raise an error, informing the programmer which methods need to be implemented to make the test pass. This can also help to significantly cut down on the amount of effort it takes to implement each test.

Another problem is that our approach is specialized exclusively to `MonadFS`. What about functions that use both `MonadFS` *and* `MonadDB`, for example? Fortunately, that is not hard to solve, either. We can adapt the `MonadFSInst` type to include fields for all of the typeclasses relevant to our system, turning it into a generic test fixture of sorts:

```haskell
data FixtureInst m = FixtureInst
  { -- MonadFS
    _readFile :: FilePath -> m String
  , _writeFile :: FilePath -> String -> m ()

    -- MonadDB
  , _fetchUser :: Id User -> m User
  , _fetchRecentPosts :: Id User -> m [Post]
  }
```

Updating `TestM` to use `FixtureInst` instead of `MonadFSInst` is trivial, and all the rest of the infrastructure still works. However, this means that every time a new typeclass is added, three things need to be updated:

  1. Its methods need to be added to the `FixtureInst` record.
  2. Those methods need to be given error-raising defaults in the `baseInst` value.
  3. An actual instance of the typeclass needs to be written for `TestM` that defers to the `FixtureInst` value.

Furthermore, most of this manual manipulation of methods is required every time a particular typeclass changes, whether that means adding a method, removing a method, renaming a method, or changing a method’s type. This is especially frustrating given that all this code is really just mechanical boilerplate that could all be derived by the set of typeclasses being tested.

That last point is especially important: aside from the instances themselves, every piece of boilerplate above is obviously possible to generate from existing types alone. With that piece of information in mind, we can do even better: we can use Template Haskell.

# Removing the boilerplate using `test-fixture`

The above code was not only rather boilerplate-heavy, it was pretty complicated. Fortunately, you don’t actually have to write it. Enter the library [`test-fixture`][test-fixture]:

```haskell
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "FixtureInst" [''MonadFS, ''MonadDB]

spec = describe "reverseFile" $ do
  it "reverses a file’s contents on the filesystem" $ do
    let contentInst = def
          { _readFile = \_ -> return "hello"
          , _writeFile = \_ contents -> log contents
          }
    let calls = logTestFixture (reverseFile "foo.txt") contentInst
    calls `shouldBe` ["olleh"]

  it "operates on the file at the provided path" $ do
    let pathInst = def
          { _readFile = \path -> log path >> return ""
          , _writeFile = \path _ -> log path
          }
    let paths = logTestFixture (reverseFile "foo.txt") pathInst
    paths `shouldBe` ["foo.txt", "foo.txt"]
```

**That’s it.** The above code automatically generates everything you need to write fast, simple, deterministic unit tests in Haskell. The `mkFixture` function is a Template Haskell macro that expands into a definition quite similar to the `FixtureInst` type we wrote by hand, but since it’s automatically generated from the typeclass definitions, it never needs to be updated.

The `logTestFixture` function replaces the `logTestM` function we wrote by hand, but it works exactly the same. The `Control.Monad.TestFixture` library also exports a `log` function that is a synonym for `tell . singleton`, but using `tell` directly still works if you prefer.

The `mkFixture` function also generates a `Default` instance, which replaces the `baseInst` value defined earlier. It functions the same way, though, producing useful error messages that refer to the names of unimplemented typeclass methods that have not been stubbed out.

This blog post is not a `test-fixture` tutorial—indeed, it is much more complicated than a `test-fixture` tutorial would be, since it covers what the library is really doing under the hood—but if you’re interested, I would highly recommend you take a look at the [`test-fixture` documentation on Hackage][test-fixture].

# Conclusion, credits, and similar techniques

This blog post came about as the result of a need my coworkers and I found when writing Haskell code; we wanted a way to write unit tests quickly and easily, but we didn’t find much advice from the rest of the Haskell ecosystem. The `test-fixture` library is the result of that exploratory work, and we currently use it to test a significant portion of our Haskell code.

It would be extremely unfair to suggest that I was the inventor of this technique or the inventor of the library. Two of my coworkers, [Joe Vargas][jxv] and [Greg Wiley][gwiley], came up with the general approach and wrote `Control.Monad.TestFixture`, and I simply wrote the Template Haskell macro to eliminate the boilerplate. With that in mind, I think I can say with some fairness that I think this technique is a joy to use when unit testing is a desirable goal, and I would definitely recommend it if you are interested in doing isolated testing in Haskell.

The general technique of using typeclasses to emulate effects was in part inspired by the well-known `mtl` library. An alternate approach to writing unit-testable Haskell code is using free monads, but overall, I prefer this approach over free monads because the typeclass constraints add type safety in ways that free monads do not (at least not without additional boilerplate), and this approach also lends itself well to static analysis-based boilerplate reduction techniques. It has its own tradeoffs, though, so if you’ve had success with free monads, then I certainly make no claim this is a superior approach, just one that I’ve personally found pleasant.

As a final note, if you *do* check out `test-fixture`, feel free to leave feedback by opening issues on [the GitHub issue tracker][test-fixture-issues]—even things like confusing documentation are worth a bug report.

[gwiley]: https://github.com/aztecrex
[jxv]: https://github.com/jxv
[test-fixture]: http://hackage.haskell.org/package/test-fixture
[test-fixture-issues]: https://github.com/cjdev/test-fixture/issues

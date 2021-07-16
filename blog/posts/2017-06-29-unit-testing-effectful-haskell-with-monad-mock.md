    Title: Unit testing effectful Haskell with monad-mock
    Date: 2017-06-29T12:08:49
    Tags: haskell, testing

Nearly eight months ago, [I wrote a blog post about unit testing effectful Haskell code][using-types-to-unit-test] using a library called test-fixture. That library has served us well, but it wasn’t as easy to use as I would have liked, and it worked better with certain patterns than others. Since then, I’ve learned more about Haskell and more about testing, and I’m pleased to announce that I am releasing an entirely new testing library, [monad-mock][].

# A first glance at monad-mock

The monad-mock library is, first and foremost, designed to be *easy*. It doesn’t ask much from you, and it requires almost zero boilerplate.

The first step is to write an mtl-style interface that encodes an effect you want to mock. For example, you might want to test some code that interacts with the filesystem:

```haskell
class Monad m => MonadFileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
```

Now you just have to write your code as normal. For demonstration purposes, here’s a function that defines copying a file in terms of `readFile` and `writeFile`:

```haskell
copyFile :: MonadFileSystem m => FilePath -> FilePath -> m ()
copyFile a b = do
  contents <- readFile a
  writeFile b contents
```

Making this function work on the real filesystem is trivial, since we just need to define an instance of `MonadFileSystem` for `IO`:

```haskell
instance MonadFileSystem IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile
```

But how do we test this? Well, we *could* run some real code in `IO`, which might not be so bad for such a simple function, but this seems like a bad idea. For one thing, a bad implementation of `copyFile` could do some pretty horrible things if it misbehaved and decided to overwrite important files, and if you’re constantly running a test suite whenever a file changes, it’s easy to imagine causing a lot of damage. Running tests against the real filesystem also makes tests slower and harder to parallelize, and it only gets much worse once you are doing more complex effects than interacting with the filesystem.

Using monad-mock, we can test this function in just a couple of lines of code:

```haskell
import Control.Exception (evaluate)
import Control.Monad.Mock
import Control.Monad.Mock.TH
import Data.Function ((&))
import Test.Hspec

makeMock "FileSystemAction" [ts| MonadFileSystem |]

spec = describe "copyFile" $
  it "reads a file and writes its contents to another file" $
    evaluate $ copyFile "foo.txt" "bar.txt"
      & runMock [ ReadFile "foo.txt" :-> "contents"
                , WriteFile "bar.txt" "contents" :-> () ]
```

That’s it!

The last two lines of the above snippet are the real interesting bits, which specify the actions that are expected to be executed, and it couples them with their results. You will find that if you tweak the list in any way, such as reordering the actions, eliminating one or both of them, or adding an additional action to the end, the test will fail. We could even turn this into a property-based test that generated arbitrary file paths and file contents.

Admittedly, in this trivial example, the mock is a little silly, since converting this into a property-based test would demonstrate how much we’ve basically just reimplemented the function in our test. However, once our function starts to do somewhat more complicated things, then our tests become more meaningful. Here’s a similar function that only copies a file if it is nonempty:

```haskell
copyNonemptyFile :: MonadFileSystem m => FilePath -> FilePath -> m ()
copyNonemptyFile a b = do
  contents <- readFile a
  unless (null contents) $
    writeFile b contents
```

This function has some logic which is very clearly *not* expressed in its type, and it would be difficult to encode that information into the type in a safe way. Fortunately, we can guarantee that it works by writing some tests:

```haskell
describe "copyNonemptyFile" $ do
  it "copies a file with contents" $
    evaluate $ copyNonemptyFile "foo.txt" "bar.txt"
      & runMock [ ReadFile "foo.txt" :-> "contents"
                , WriteFile "bar.txt" "contents" :-> () ]

  it "does nothing with an empty file" $
    evaluate $ copyNonemptyFile "foo.txt" "bar.txt"
      & runMock [ ReadFile "foo.txt" :-> "" ]
```

These tests are much more useful, and they have some actual value to them. Imagine we had accidentally written `when` instead of `unless`, an easy typo to make. Our tests would fail with some useful error messages:

```
1) copyNonemptyFile copies a file with contents
     uncaught exception: runMockT: expected the following unexecuted actions to be run:
       WriteFile "bar.txt" "contents"

2) copyNonemptyFile does nothing with an empty file
     uncaught exception: runMockT: expected end of program, called writeFile
       given action: WriteFile "bar.txt" ""
```

You now know enough to write tests with monad-mock.

# Why unit test?

When the issue of testing is brought up in Haskell, it is often treated with a certain distaste by a portion of the community. There are some points I’ve seen a number of times, and though they take different forms, they boil down to two ideas:

  1. “Haskell code does not need tests because the type system can prove correctness.”

  2. “Testing in Haskell is trivial because it is a pure language, and testing pure functions is easy.”

I’ve been writing Haskell professionally for over a year now, and I can happily say that there *is* some truth to both of those things! When my Haskell code typechecks, I feel a confidence in it that I would not feel were I using a language with a less powerful type system. Furthermore, Haskell encourages a “pure core, impure shell” approach to system design that makes testing many things pleasant and straightforward, and it completely eliminates the worry of subtle nondeterminism leaking into tests.

That said, Haskell is not a proof assistant, and its type system cannot guarantee everything, especially for code that operates on the boundaries of what Haskell can control. For much the same reason, I find that my pure code is the code I am *least* likely to need to test, since it is also the code with the strongest type safety guarantees, operating on types in my application’s domain. In contrast, the effectful code is often what I find the most value in extensively testing, since it often contains the most subtle complexity, and it is frequently difficult or even impossible to encode into types.

Haskell has the power to provide remarkably strong correctness guarantees with a surprisingly small amount of effort by using a combination of tests and types, using each to accommodate for the other’s weaknesses and playing to each technique’s strengths. Some code is test-driven, other code is type-driven. Most code ends up being a mix of both. Testing is just a tool like any other, and it’s nice to feel confident in one’s ability to effectively structure code in a decoupled, testable manner.

# Why mock?

Even if you accept that testing is good, the question of whether or not to *mock* is a subtler issue. To some people, “unit testing” is synonymous with mocks. This is emphatically not true, and in fact, overly aggressive mocking is one of the best ways to make your test suite completely worthless. The monad-mock approach to mocking is a bit more principled than mocking in many dynamic, object-oriented languages, but it comes with many of the same drawbacks: mocks couple your tests to your implementation in ways that make them less valuable and less meaningful.

For the `MonadFileSystem` example above, I would actually probably *not* use a mock. Instead, I would use a **fake**, in-memory filesystem implementation:

```haskell
newtype FakeFileSystemT m a = FakeFileSystemT (StateT [(FilePath, String)] m a)
  deriving (Functor, Applicative, Monad)

fakeFileSystemT :: Monad m => [(FilePath, String)]
                -> FakeFileSystemT m a -> m (a, [(FilePath, String)])
fakeFileSystemT fs (FakeFileSystemT x) = second sort <$> runStateT x fs

instance Monad m => MonadFileSystem (FakeFileSystemT m) where
  readFile path = FakeFileSystemT $ get >>= \fs -> lookup path fs &
    maybe (fail $ "readFile: no such file ‘" ++ path ++ "’") return
  writeFile path contents = FakeFileSystemT . modify $ \fs ->
    (path, contents) : filter ((/= path) . fst) fs
```

The above snippet demonstrates how easy it is to define a `MonadFileSystem` implementation in terms of `StateT`, and while this may seem like a lot of boilerplate, it really isn’t. You have to write a fake *once* per interface, and the above block is a minuscule twelve lines of code. With this technique, you are still able to write tests that depend on the state of the filesystem before and after running the implementation, but you decouple yourself from the precise process of getting there:

```haskell
describe "copyNonemptyFile" $ do
  it "copies a file with contents" $ do
    let ((), fs) = runIdentity $ copyNonemptyFile "foo.txt" "bar.txt"
          & fakeFileSystemT [ ("foo.txt", "contents") ]
    fs `shouldBe` [ ("bar.txt", "contents"), ("foo.txt", "contents") ]

  it "does nothing with an empty file" $ do
    let ((), fs) = runIdentity $ copyNonemptyFile "foo.txt" "bar.txt"
          & fakeFileSystemT [ ("foo.txt", "") ]
    fs `shouldBe` [ ("foo.txt", "") ]
```

This is better than using a mock, and I would highly recommend doing it if you can! However, a lot of real applications have to interact with services of much greater complexity than an idealized filesystem, and creating that sort of in-memory fake is not always practical. One such situation might be interacting with AWS CloudFormation, for example:

```haskell
class Monad m => MonadAWS m where
  createStack :: StackName -> StackTemplate -> m (Either AWSError StackId)
  listStacks :: m (Either AWSError [StackSummaries])
  describeStack :: StackId -> m (Either AWSError StackInfo)
  -- and so on...
```

AWS is a very complex system, and it can do dozens of different things (and fail in dozens of different ways) based on an equally complex set of inputs. For example, in the above API, `createStack` needs to parse its template, which can be YAML or JSON, in order to determine which of many possible errors and behaviors can be produced, both on the initial call and on subsequent ones.

Creating a fake implementation of *AWS* is hardly feasible, and this is where a mock can be useful. By simply writing `makeMock "AWSAction" [ts| MonadAWS |]`, we can test functions that interact with AWS in a pure way without necessarily needing to replicate all of its complexity.

## Isolating mocks

Of course, tests that use mocks provide less value than tests that use “smarter” fakes, since they are far more tightly coupled to the implementation, and it’s dramatically more likely that you will need to change the tests when you change the logic. To avoid this, it can be helpful to create multiple interfaces to the same thing: a high-level interface and a low-level one. If our above `MonadAWS` is a low-level interface, we could create a high-level counterpart that does precisely what our application needs:

```haskell
class Monad m => MonadDeploy m where
  executeDeployment :: Deployment -> m (Either DeployError ())
```

When running our application “for real”, we would use `MonadAWS` to implement `MonadDeploy`:

```haskell
executeDeploymentImpl :: MonadAWS m => Deployment -> m (Either DeployError ())
executeDeploymentImpl = ...
```

The nice thing about this is we can actually test `executeDeploymentImpl` using a `MonadAWS` mock, so we can still have unit test coverage of the code on the boundaries of our system! Additionally, by containing the mock to a single place, we can test the rest of our code using a smarter fake implementation of `MonadDeploy`, helping to decouple our code from AWS’s complex API and improve the reliability and usefulness of our test suite.

They key point here is that mocking is just a small piece of the larger testing puzzle in *any* language, and that is just as true in Haskell. An overemphasis on mocking is an easy way to end up with a test suite that feels useless, probably because it is. Use mocks as a technique to insulate your application from the complexity in others’ APIs, then use more domain-specific testing techniques and type-level assertions to ensure the correctness of your logic.

# How monad-mock works

If you’ve read this far and are convinced that monad-mock is useful, you may safely stop reading now. However, if you are interested in the details of what it actually does and what makes it tick, the rest of this blog post is going to focus on how the implementation works and how it compares to other techniques.

The centerpiece of monad-mock’s API is its monad transformer, `MockT`, which is a type constructor that accepts three types:

```haskell
newtype MockT (f :: * -> *) (m :: * -> *) (a :: *)
```

The `m` and `a` type variables obviously correspond to the usual monad transformer arguments, which represent the underlying monad and the result of the monadic computation, respectively. The `f` variable is more interesting, since it’s what makes `MockT` work at all, and it isn’t even a type: it’s a type constructor with kind `* -> *`. What does it mean?

Looking at the type signature of `runMockT` gives us a little bit more information about what that `f` actually represents:

```haskell
runMockT :: (Action f, Monad m) => [WithResult f] -> MockT f m a -> m a
```

This type signature provides two pieces of key information:

  1. The `f` parameter is constrained by the `Action f` constraint.

  2. Running a mocked computation requires supplying a list of `WithResult f` values. This list corresponds to the list of expectations provided to `runMock` in earlier examples.

To understand both of these things, it helps to examine the definition of an actual datatype that can have an `Action` instance. For the filesystem example, the action datatype looks like this:

```haskell
data FileSystemAction r where
  ReadFile :: FilePath -> FileSystemAction String
  WriteFile :: FilePath -> String -> FileSystemAction ()
```

Notice how each constructor clearly corresponds to one of the methods of `MonadFileSystem`, with a type to match. Now the purpose of the type provided to the `FileSystemAction` constructor (in this case `r`) should hopefully become clear: it represents the type of the value *produced* by each method. Also note that the type is completely phantom—it does not appear in negative position in any of the constructors.

With this in mind, we can take a look at the definition of `WithResult`:

```haskell
data WithResult f where
  (:->) :: f r -> r -> WithResult f
```

This is what defines the `(:->)` constructor from earlier in the blog post, and you can see that it effectively just represents a tuple of an action and a value of its associated result. It’s completely type-safe, since it ensures the result matches the type argument to the action.

Finally, this brings us to the `Action` class, which is not complex, but is unfortunately necessary:

```haskell
class Action f where
  eqAction :: f a -> f b -> Maybe (a :~: b)
  showAction :: f a -> String
```

Notice that these methods are effectively just `(==)` and `show`, lifted to type constructors of kind `* -> *`. One significant difference is that `eqAction` produces `Maybe (a :~: b)` instead of `Bool`, where `(:~:)` is from `Data.Type.Equality`. This is a type equality witness, which means a successful equality between two values allows the compiler to be sure that the two *types* are equal. This is necessary for the implementation of `runMockT` due to the phantom type in actions—in order to convince GHC that we can properly return the result of a mocked action, we need to assure it that the value we’re going to return is actually of the proper type.

Implementing this typeclass is not particularly burdensome, but it’s entirely boilerplate, so even if you want to define your own action type (that is, you don’t want to use `makeMock`), you can use the `deriveAction` function from `Control.Monad.Mock.TH` to derive an `Action` instance on an existing datatype.

## Connecting the mock to its class

Now that we have an action with which to mock a class, we need to actually define an instance of that class for `MockT`. For this process, monad-mock provides a `mockAction` function with the following type:

```haskell
mockAction :: (Action f, Monad m) => String -> f r -> MockT f m r
```

This function accepts two arguments: the name of the method being mocked and the action that represents the current call. This is easier to illustrate with an actual instance of `MonadFileSystem` using `MockT` and our `FileSystemAction` type:

```haskell
instance Monad m => MonadFileSystem (MockT FileSystemAction m) where
  readFile a = mockAction "readFile" (ReadFile a)
  writeFile a b = mockAction "writeFile" (WriteFile a b)
```

This allows `readFile` and `writeFile` to defer to the mock, and providing the names of the functions as strings helps monad-mock to produce useful error messages upon failure. Internally, `MockT` is a `StateT` that keeps track of a list of `WithResult f` values as its state. Each call to the mock checks the action against the internal list of calls, and if they match, it returns the associated result. Otherwise, it throws an exception.

This scheme is simple, but it seems to work remarkably well. There are some obvious enhancements that will probably be eventually necessary, like allowing action results that run in the underlying monad `m` in order to support things like `throwError` from `MonadError`, but so far, it hasn’t been necessary for what we’ve been using it for. Certain tricky signatures defy this simple technique, such as signatures where a monadic action appears in a negative position (that is, the signatures you need things like [monad-control][] or [monad-unlift][] for), but we’ve found that most of our effects don’t have any reason to include such signatures.

# A brief comparison with free(r) monads

At this point, astute readers will likely be thinking about free monads, which parts of this technique greatly resemble. The representation of actions as GADTs is especially similar to [freer][], which does something extremely similar. Indeed, you can think of this technique as something that combines a freer-style representation with mtl-style classes. Given that freer already does this, you might ask yourself what the point is.

If you are already sold on free monads, monad-mock may very well be uninteresting to you. From the perspective of theoretical novelty, monad-mock is not anything new or different. However, there are a variety of practical reasons to prefer mtl over free, and it’s nice to see how easy it is to enjoy the testing benefits of free without too much extra effort.

An in-depth comparison between mtl and free is well outside the scope of this blog post. However, the key point is that this technique *only* affects test code, so the real runtime implementation will not be affected in any way. This means you can take advantage of the performance benefits and ecosystem support of mtl without sacrificing simple, expressive testing.

# Conclusion

To cap things off, I want to emphasize monad-mock’s role as a single part of a larger initiative we’ve been making for the better part of the past eighteen months. Haskell is a language with ever-evolving techniques and style, and it’s sometimes dizzying to figure out how to use all the pieces together to develop robust, maintainable applications. While monad-mock might not be anything drastically different from existing testing techniques, my hope is that it can provide an opinionated mechanism to make testing easy and accessible, even for complex interactions with other services and systems.

I’ve made an effort to make it abundantly clear in this blog post that monad-mock is *not* a silver bullet to testing, and in fact, I would prefer other techniques for ensuring correctness whenever possible. Even so, mocking is a nice tool to have in your toolbox, and it’s a good fallback to get even the worst APIs under test coverage.

If you want to try out monad-mock for yourself, [take a look at the documentation on Hackage][monad-mock] and start playing around! It’s still early software, so it’s not the most proven or featureful, but we’ve managed to get mileage out of it already, all the same. If you find any problems, have a use case it does not support, or just find something about it unclear, please do not hesitate to [open an issue on the GitHub repository][monad-mock-repo]—we obviously can’t fix issues we don’t know about.

Thanks as always to the many people who have contributed ideas that have shaped my philosophy and approach to testing and have helped provide the tools that make this library work. Happy testing!

[freer]: https://hackage.haskell.org/package/freer
[monad-control]: https://hackage.haskell.org/package/monad-control
[monad-mock]: https://hackage.haskell.org/package/monad-mock
[monad-mock-repo]: https://github.com/cjdev/monad-mock
[monad-unlift]: https://hackage.haskell.org/package/monad-unlift
[using-types-to-unit-test]: /blog/2016/10/03/using-types-to-unit-test-in-haskell/

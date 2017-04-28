    Title: Lifts for free: making mtl typeclasses derivable
    Date: 2017-04-28T00:13:19
    Tags: haskell

Perhaps the most important abstraction a Haskell programmer must understand to effectively write modern Haskell code, beyond the level of the monad, is the *monad transformer*, a way to compose monads together in a limited fashion. One frustrating downside to monad transformers is a proliferation of `lift`s, which explicitly indicate which monad in a transformer “stack” a particular computation should run in. Fortunately, the venerable [mtl][] provides typeclasses that make this lifting mostly automatic, using typeclass machinery to insert `lift` where appropriate.

Less fortunately, the mtl approach does not actually eliminate `lift` entirely, it simply moves it from use sites to instances. This requires a small zoo of extraordinarily boilerplate-y instances, most of which simply implement each typeclass method using `lift`. While we cannot eliminate the instances entirely without somewhat dangerous techniques like [overlapping instances][overlapping-instances], we *can* automatically derive them using features of modern GHC, eliminating the truly unnecessary boilerplate.

<!-- more -->

# The problem with mtl-style typeclasses

To understand what problem it is exactly that we’re trying to solve, we first need to take a look at an actual mtl-style typeclass. I am going to start with an mtl-*style* typeclass, rather than an actual typeclass in the mtl, due to slight complications with mtl’s actual typeclasses that we’ll get into later. Instead, let’s start with a somewhat boring typeclass, which we’ll call `MonadExit`:

```haskell
import System.Exit (ExitCode)

class Monad m => MonadExit m where
  exitWith :: ExitCode -> m ()
```

This is a simple typeclass that abstracts over the concept of early exit, given an exit code. The most obvious implementation of this typeclass is over `IO`, which will actually exit the program:

```haskell
import qualified System.Exit as IO (exitWith)

instance MonadExit IO where
  exitWith = IO.exitWith
```

One of the cool things about these typeclasses, though, is that we don’t have to have just one implementation. We could also write a pure implementation of `MonadExit`, which would simply short-circuit the current computation and return the `ExitCode`:

```haskell
instance MonadExit (Either ExitCode) where
  exitWith = Left
```

Instead of simply having an instance on a concrete monad, though, we probably want to be able to use this in a larger monad stack, so we can define an `ExitT` monad transformer that can be inserted into any monad transformer stack:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (MonadTrans)

newtype ExitT m a = ExitT (ExceptT ExitCode m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runExitT :: ExitT m a -> m (Either ExitCode a)
runExitT (ExitT x) = runExceptT x

instance Monad m => MonadExit (ExitT m) where
  exitWith = ExitT . throwError
```

With this in place, we can write actual programs using our `ExitT` monad transformer:

```haskell
ghci> runExitT $ do
        lift $ putStrLn "hello"
        exitWith (ExitFailure 1)
        lift $ putStrLn "world"
hello
Left (ExitFailure 1)
```

This is pretty cool! Unfortunately, experienced readers will see the rather large problem with what we have so far. Specifically, it won’t actually work if we try and wrap `ExitT` in another monad transformer:

```haskell
ghci> logIn password = runExitT $ flip runReaderT password $ do
        password <- ask
        unless (password == "password1234") $ -- super secure password
          exitWith (ExitFailure 1)
        return "access granted"

ghci> logIn "not the right password"
<interactive>: error:
    • No instance for (MonadExit (ReaderT [Char] (ExitT m0)))
        arising from a use of ‘it’
    • In a stmt of an interactive GHCi command: print it
```

The error message is relatively self-explanatory if you are familiar with mtl error messages: there is no `MonadExit` instance for `ReaderT`. This makes sense, since we only defined a `MonadExit` instance for *`ExitT`*, nothing else. Fortunately, the instance for `ReaderT` is completely trivial, since we just need to use `lift` to delegate to the next monad in the stack:

```haskell
instance MonadExit m => MonadExit (ReaderT r m) where
  exitWith = lift . exitWith
```

Now that the delegating instance is set up, we can actually use our `logIn` function:

```haskell
ghci> logIn "not the right password"
Left (ExitFailure 1)
ghci> logIn "password1234"
Right "access granted"
```

## An embarrassment of instances

We’ve managed to make our program work properly now, but we’ve still only defined the delegating instance for `ReaderT`. What if someone wants to use `ExitT` with `WriterT`? Or `StateT`? Or any of `ExceptT`, `RWST`, or `ContT`? Well, we have to define instances for each and every one of them, and as it turns out, the instances are all identical!

```haskell
instance (MonadExit m, Monoid w) => MonadExit (WriterT w m) where
  exitWith = lift . exitWith

instance MonadExit m => MonadExit (StateT s m) where
  exitWith = lift . exitWith

instance (MonadExit m, Monoid w) => MonadExit (RWST r w s m) where
  exitWith = lift . exitWith

instance MonadExit m => MonadExit (ExceptT e m) where
  exitWith = lift . exitWith

instance MonadExit m => MonadExit (ContT r m) where
  exitWith = lift . exitWith
```

This is bad enough on its own, but this is actually the *simplest* case: a typeclass with a single method which is trivially lifted through any other monad transformer. Another thing we’ve glossed over is actually defining all the delegating instances for the *other* mtl typeclasses on `ExitT` itself. Fortunately, we can derive these ones with `GeneralizedNewtypeDeriving`, since `ExceptT` has already done most of the work for us:

```haskell
newtype ExitT m a = ExitT (ExceptT ExitCode m a)
  deriving ( Functor, Applicative, Monad, MonadIO -- base
           , MonadBase IO -- transformers-base
           , MonadTrans, MonadReader r, MonadWriter w, MonadState s -- mtl
           , MonadThrow, MonadCatch, MonadMask -- exceptions
           , MonadTransControl, MonadBaseControl IO -- monad-control
           )
```

Unfortunately, we have to write the `MonadError` instance manually if we want it, since we don’t want to pick up the instance from `ExceptT`, but rather wish to defer to the underlying monad. This means writing some truly horrid delegation code:

```haskell
instance MonadError e m => MonadError e (ExitT m) where
  throwError = lift . throwError

  catchError (ExitT x) f = ExitT . ExceptT $ catchError (runExceptT x) $ \e ->
    let (ExitT x') = f e in runExceptT x'
```

(Notably, this is so awful because `catchError` is more complex than the simple `exitWith` method we’ve studied so far, which is why we’re starting with a simpler typeclass. We’ll get more into this later, as promised.)

This huge number of instances is sometimes referred to as the “n<sup>2</sup> instances” problem, since it requires every monad transformer have an instance of every single mtl-style typeclass. Fortunately, in practice, this proliferation is often less horrible than it might seem, mostly because deriving helps a lot. However, remember that if `ExitT` *weren’t* a simple wrapper around an existing monad transformer, we wouldn’t be able to derive the instances at all! Instead, we’d have to write them all out by hand, just like we did with all the `MonadExit` instances.

It’s a shame that these typeclass instances can’t be derived in a more general way, allowing derivation for arbitrary monad transformers instead of simply requiring the newtype deriving machinery. As it turns out, with clever use of modern GHC features, we actually **can**. It’s not even all that hard.

# Default instances with default signatures

It’s not hard to see that our `MonadExit` instances are all exactly the same: just `lift . exitWith`. Why is that, though? Well, every instance is an instance on a monad transformer over a monad that is already an instance of `MonadExit`. In fact, we can express this in a type signature, and we can extract `lift . exitWith` into a separate function:

```haskell
defaultExitWith :: (MonadTrans t, MonadExit m) => ExitCode -> t m ()
defaultExitWith = lift . exitWith
```

However, writing `defaultExitWith` really isn’t any easier than writing `lift . exitWith`, so this deduplication doesn’t really buy us anything. However, it *does* indicate that we could write a default implementation of `exitWith` if we could require just a little bit more from the implementing type. With [GHC’s `DefaultSignatures` extension][default-signatures], we can do precisely that.

The idea is that we can write a separate type signature for a default implementation of `exitWith`, which can be more specific than the type signature for `exitWith` in general. This allows us to use our `defaultExitWith` implementation more or less directly:

```haskell
{-# LANGUAGE DefaultSignatures #-}

class Monad m => MonadExit m where
  exitWith :: ExitCode -> m ()

  default exitWith :: (MonadTrans t, MonadExit m1) => ExitCode -> t m1 ()
  exitWith = lift . exitWith
```

We have to use `m1` instead of `m`, since type variables in the instance head are always scoped, and the names would conflict. However, this creates another problem, since our specialized type signature replaces `m` with `t m1`, which won’t quite work (as GHC can’t automatically figure out they should be the same). Instead, we can use `m` in the type signature, then just add a type equality constraint ensuring that `m` and `t m1` must be the same type:

```haskell
class Monad m => MonadExit m where
  exitWith :: ExitCode -> m ()

  default exitWith :: (MonadTrans t, MonadExit m1, m ~ t m1) => ExitCode -> m ()
  exitWith = lift . exitWith
```

Now we can write all of our simple instances without even needing to write a real implementation! All of the instance bodies can be empty:

```haskell
instance MonadExit m => MonadExit (ReaderT r m)
instance (MonadExit m, Monoid w) => MonadExit (WriterT w m)
instance MonadExit m => MonadExit (StateT s m)
instance (MonadExit m, Monoid w) => MonadExit (RWST r w s m)
instance MonadExit m => MonadExit (ExceptT e m)
instance MonadExit m => MonadExit (ContT r m)
```

While this doesn’t completely alleviate the pain of writing instances, it’s definitely an improvement over what we had before. With [GHC 8.2’s new `DerivingStrategies` extension][deriving-strategies], it becomes especially beneficial when defining entirely new transformers that should also have `ExitT` instances, since they can be derived with `DeriveAnyClass`:

```haskell
newtype ParserT m a = ParserT (Text -> m (Maybe (Text, a)))
  deriving anyclass (MonadExit)
```

This is pretty wonderful.

Given that only `MonadExit` supports being derived in this way, we sadly still need to implement the other, more standard mtl-style typeclasses ourselves, like `MonadIO`, `MonadBase`, `MonadReader`, `MonadWriter`, etc. However, what if all of those classes provided the same convenient default signatures that our `MonadExit` does? If that were the case, then we could write something like this:

```haskell
newtype ParserT m a = ParserT (Text -> m (Maybe (Text, a)))
  deriving anyclass ( MonadIO, MonadBase b
                    , MonadReader r, MonadWriter w, MonadState s
                    , MonadThrow, MonadCatch, MonadMask
                    , MonadExit
                    )
```

Compared to having to write all those instances by hand, this would be a pretty enormous difference. Unfortunately, many of these typeclasses are not quite as simple as our `MonadExit`, and we’d have to be a bit more clever to make them derivable.

# Making mtl’s classes derivable

Our `MonadExit` class was extremely simple, since it only had a single method with a particularly simple type signature. For reference, this was the type of our generic `exitWith`:

```haskell
exitWith :: MonadExit m => ExitCode -> m ()
```

Let’s now turn our attention to `MonadReader`. At first blush, this typeclass should not be any trickier to implement than `MonadExit`, since the types of `ask` and `reader` are both quite simple:

```haskell
ask :: MonadReader r m => m r
reader :: MonadReader r m => (r -> a) -> m a
```

However, the type of the other method, `local`, throws a bit of a wrench in our plans. It has the following type signature:

```haskell
local :: MonadReader r m => (r -> r) -> m a -> m a
```

Why is this so much more complicated? Well, the key is in the second argument, which has the type `m a`. That’s not something that can be simply `lift`ed away! Try it yourself: try to write a `MonadReader` instance for some monad transformer. It’s not as easy as it looks!

We can illustrate the problem by creating our own version of `MonadReader` and implementing it for something like `ExceptT` ourselves. We can start with the trivial methods first:

```haskell
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  reader = lift . reader
```

However, implementing `local` is harder. Let’s specialize the type signature to `ExceptT` to make it more clear why:

```haskell
local :: MonadReader r m => (r -> r) -> ExceptT e m a -> ExceptT e m a
```

Our base monad, `m`, implements `local`, but we have to convert the first argument from `ExceptT e m a` into `m (Either e a)` first, run it through `local` in `m`, then wrap it back up in `ExceptT`:

```haskell
instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  reader = lift . reader
  local f x = ExceptT $ local f (runExceptT x)
```

This operation is actually a mapping operation of sorts, since we’re mapping `local f` over `x`. For that reason, this can be rewritten using the `mapExceptT` function provided from `Control.Monad.Except`:

```haskell
instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  reader = lift . reader
  local = mapExceptT . local
```

If you implement `MonadReader` instances for other transformers, like `StateT` and `WriterT`, you’ll find that the instances are exactly the same *except* for `mapExceptT`, which is replaced with `mapStateT` and `mapWriterT`, respectively. This is sort of obnoxious, given that we want to figure out how to create a generic version of `local` that works with any monad transformer, but this requires concrete information about which monad we’re in. Obviously, the power `MonadTrans` gives us is not enough to make this generic. Fortunately, there is a typeclass which does: [`MonadTransControl`][MonadTransControl] from the `monad-control` package.

Using `MonadTransControl`, we can write a generic `mapT` function that maps over an arbitrary monad transformer with a `MonadTransControl` instance:

```haskell
mapT :: (Monad m, Monad (t m), MonadTransControl t)
     => (m (StT t a) -> m (StT t b))
     -> t m a
     -> t m b
mapT f x = liftWith (\run -> f (run x)) >>= restoreT . return
```

This type signature may look complicated (and, well, it is), but the idea is that the `StT` associated type family encapsulates the monadic state that `t` introduces. For example, for `ExceptT`, `StT (ExceptT e) a` is `Either e a`. For `StateT`, `StT (StateT s) a` is `(a, s)`. Some transformers, like `ReaderT`, have no state, so `StT (ReaderT r) a` is just `a`.

I will not go into the precise mechanics of how `MonadTransControl` works in this blog post, but it doesn’t matter significantly; the point is that we can now use `mapT` to create a generic implementation of `local` for use with `DefaultSignatures`:

```haskell
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  default ask :: (MonadTrans t, MonadReader r m1, m ~ t m1) => m r
  ask = lift ask

  local :: (r -> r) -> m a -> m a
  default local :: (MonadTransControl t, MonadReader r m1, m ~ t m1) => (r -> r) -> m a -> m a
  local = mapT . local

  reader :: (r -> a) -> m a
  reader f = f <$> ask
```

Once more, we now get instances of our typeclass, in this case `MonadReader`, **for free**:

```haskell
instance MonadReader r m => MonadReader r (ExceptT e m)
instance (MonadReader r m, Monoid w) => MonadReader r (WriterT w m)
instance MonadReader r m => MonadReader r (StateT s m)
```

It’s also worth noting that we *don’t* get a `ContT` instance for free, even though `ContT` has a `MonadReader` instance in mtl. Unlike the other monad transformers mtl provides, `ContT` does not have a `MonadTransControl` instance because it cannot be generally mapped over. While a `mapContT` function does exist, its signature is more restricted:

```haskell
mapContT :: (m r -> m r) -> ContT k r m a -> ContT k r m a
```

It happens that `local` can still be implemented for `ContT`, so it can still have a `MonadReader` instance, but it cannot be derived in the same way as it can for the other transformers. Still, in practice, I’ve found that most user-defined transformers do not have such complex control flow, so they can safely be instances of `MonadTransControl`, and they get this deriving for free.

## Extending this technique to other mtl typeclasses

The default instances for the other mtl typeclasses are slightly different from the one for `MonadReader`, but for the most part, the same general technique applies. Here’s a derivable `MonadError`:

```haskell
class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  default throwError :: (MonadTrans t, MonadError e m1, m ~ t m1) => e -> m a
  throwError = lift . throwError

  catchError :: m a -> (e -> m a) -> m a
  default catchError :: (MonadTransControl t, MonadError e m1, m ~ t m1) => m a -> (e -> m a) -> m a
  catchError x f = liftWith (\run -> catchError (run x) (run . f)) >>= restoreT . return

instance MonadError e m => MonadError e (ReaderT r m)
instance (MonadError e m, Monoid w) => MonadError e (WriterT w m)
instance MonadError e m => MonadError e (StateT s m)
instance (MonadError e m, Monoid w) => MonadError e (RWST r w s m)
```

The `MonadState` interface turns out to be extremely simple, so it doesn’t even need `MonadTransControl` at all:

```haskell
class Monad m => MonadState s m | m -> s where
  get :: m s
  default get :: (MonadTrans t, MonadState s m1, m ~ t m1) => m s
  get = lift get

  put :: s -> m ()
  default put :: (MonadTrans t, MonadState s m1, m ~ t m1) => s -> m ()
  put = lift . put

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let (a, s') = f s
    put s'
    return a

instance MonadState s m => MonadState s (ExceptT e m)
instance MonadState s m => MonadState s (ReaderT r m)
instance (MonadState s m, Monoid w) => MonadState s (WriterT w m)
```

Everything seems to be going well! However, not everything is quite so simple.

## A `MonadWriter` diversion

Unexpectedly, `MonadWriter` turns out to be by far the trickiest of the bunch. It’s not too hard to create default implementations for most of the methods of the typeclass:

```haskell
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  writer :: (a, w) -> m a
  default writer :: (MonadTrans t, MonadWriter w m1, m ~ t m1) => (a, w) -> m a
  writer = lift . writer

  tell :: w -> m ()
  default tell :: (MonadTrans t, MonadWriter w m1, m ~ t m1) => w -> m ()
  tell = lift . tell

  listen :: m a -> m (a, w)
  default listen :: (MonadTransControl t, MonadWriter w m1, m ~ t m1) => m a -> m (a, w)
  listen x = do
    (y, w) <- liftWith (\run -> listen (run x))
    y' <- restoreT (return y)
    return (y', w)
```

However, `MonadWriter` has a fourth method, `pass`, which has a particularly tricky type signature:

```haskell
pass :: m (a, w -> w) -> m a
```

As far as I can tell, this is not possible to generalize using `MonadTransControl` alone, since it would require inspection of the result of the monadic argument (that is, it would require a function from `StT t (a, b) -> (StT t a, b)`), which is not possible in general. My gut is that this could likely also be generalized with a slightly more powerful abstraction than `MonadTransControl`, but it is not immediately obvious to me what that abstraction should be.

One extremely simple way to make this possible would be to design something to serve this specific use case:

```haskell
type RunSplit t = forall m a b. Monad m => t m (a, b) -> m (StT t a, Maybe b)
class MonadTransControl t => MonadTransSplit t where
  liftWithSplit :: Monad m => (RunSplit t -> m a) -> t m a
```

Instances of `MonadTransSplit` would basically just provide a way to pull out bits of the result, if possible:

```haskell
instance MonadTransSplit (ReaderT r) where
  liftWithSplit f = liftWith $ \run -> f (fmap split . run)
    where split (x, y) = (x, Just y)

instance MonadTransSplit (ExceptT e) where
  liftWithSplit f = liftWith $ \run -> f (fmap split . run)
    where split (Left e) = (Left e, Nothing)
          split (Right (x, y)) = (Right x, Just y)

instance MonadTransSplit (StateT s) where
  liftWithSplit f = liftWith $ \run -> f (fmap split . run)
    where split ((x, y), s) = ((x, s), Just y)
```

Then, using this, it would be possible to write a generic version of `pass`:

```haskell
default pass :: (MonadTransSplit t, MonadWriter w m1, m ~ t m1) => m (a, w -> w) -> m a
pass m = do
  r <- liftWithSplit $ \run -> pass $ run m >>= \case
    (x, Just f) -> return (x, f)
    (x, Nothing) -> return (x, id)
  restoreT (return r)
```

However, this seems pretty overkill for just one particular method, given that I have no idea if `MonadTransSplit` would be useful *anywhere* else. One interesting thing about going down this rabbit hole, though, is that I learned that `pass` has some somewhat surprising behavior when mixed with transformers like `ExceptT` or `MaybeT`, if you don’t carefully consider how it works. It’s a strange method with a somewhat strange interface, so I don’t think I have a satisfactory conclusion about `MonadWriter` yet.

# Regrouping and stepping back

Alright, that was a lot of fairly intense, potentially confusing code. What the heck did we actually accomplish? Well, we got a couple of things:

  1. First, we developed a technique for writing simple mtl-style typeclasses that are derivable using `DeriveAnyClass` (or simply writing an empty instance declaration). We used a `MonadExit` class as a proof of concept, but really, the technique is applicable to most mtl-style typeclasses that represent simple effects (including, for example, `MonadIO`).

     This technique is useful in isolation, even if you completely disregard the rest of the blog post. For an example where I recently applied it in real code, see [the default signatures provided with `MonadPersist` from the `monad-persist` library][monad-persist-default-sigs], which make [defining instances completely trivial][monad-persist-instances]. If you use mtl-style typeclasses in your own application to model effects, I don’t see much of a reason *not* to use this technique.

  2. After `MonadExit`, we applied the same technique to the mtl-provided typeclasses `MonadReader`, `MonadError`, and `MonadState`. These are a bit trickier, since the first two need `MonadTransControl` in addition to the  usual `MonadTrans`.

     Whether or not this sort of thing should actually be added to mtl itself probably remains to be seen. For the simplest typeclass, `MonadState`, it seems like there probably aren’t many downsides, but given the difficulty implementing it for `MonadWriter` (or, heaven forbid, `MonadCont`, which I didn’t even seriously take a look at for this blog post), it doesn’t seem like an obvious win. Consistency is important.

     Another downside that I sort of glossed over is possibly even more significant from a practical point of view: adding default signatures to `MonadReader` would require the removal of the default implementation of `ask` that is provided by the existing library (which implements `ask` in terms of `reader`). This would be backwards-incompatible, so it’d be difficult to change, even if people wanted to do it. Still, it’s interesting to consider what these typeclasses might look like if they were designed today.

Overall, these techniques are not a silver bullet for deriving mtl-style typeclasses, nor do they eliminate the n<sup>2</sup> instances problem that mtl style suffers from. That said, they *do* significantly reduce boilerplate and clutter in the simplest cases, and they demonstrate how modern Haskell’s hierarchy of typeclasses provides a lot of power, both to describe quite abstract concepts and to alleviate the need to write code by hand.

I will continue to experiment with the ideas described in this blog post, and I’m sure some more pros and cons will surface as I explore the design space. If you have any suggestions for how to deal with “the `MonadWriter` problem”, I’d be very interested to hear them! In the meantime, consider using the technique in your application code when writing effectful, monadic typeclasses.

[default-signatures]: https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#default-method-signatures
[deriving-strategies]: https://downloads.haskell.org/~ghc/8.2.1-rc1/docs/html/users_guide/glasgow_exts.html#deriving-strategies
[MonadTransControl]: http://hackage.haskell.org/package/monad-control-1.0.1.0/docs/Control-Monad-Trans-Control.html#t:MonadTransControl
[monad-persist-default-sigs]: https://github.com/cjdev/monad-persist/blob/1ce8568d881da3171f8689dd65f4f2df5f6dd313/library/Control/Monad/Persist.hs#L226-L271
[monad-persist-instances]: https://github.com/cjdev/monad-persist/blob/1ce8568d881da3171f8689dd65f4f2df5f6dd313/library/Control/Monad/Persist.hs#L506-L513
[mtl]: https://hackage.haskell.org/package/mtl
[overlapping-instances]: https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#overlapping-instances

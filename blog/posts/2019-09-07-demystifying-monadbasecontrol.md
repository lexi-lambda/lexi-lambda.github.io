    Title: Demystifying `MonadBaseControl`
    Date: 2019-09-07T18:00:00
    Tags: haskell

[`MonadBaseControl` from the `monad-control` package][monad-control:MonadBaseControl] is a confusing typeclass, and its methods have complicated types. For many people, it’s nothing more than scary, impossible-to-understand magic that is, for some reason, needed when lifting certain kinds of operations. Few resources exist that adequately explain how, why, and when it works, which sadly seems to have resulted in some [FUD](https://en.wikipedia.org/wiki/Fear,_uncertainty,_and_doubt) about its use.

There’s no doubt that the machinery of `MonadBaseControl` is complex, and the role it plays in practice is often subtle. However, its essence is actually much simpler than it appears, and I promise it can be understood by mere mortals. In this blog post, I hope to provide a complete survey of `MonadBaseControl`—how it works, how it’s designed, and how it can go wrong—in a way that is accessible to anyone with a firm grasp of monads and monad transformers. To start, we’ll motivate `MonadBaseControl` by reinventing it ourselves.

# The higher-order action problem

Say we have a function with the following type:[^0]

```haskell
foo :: IO a -> IO a
```

If we have an action built from a transformer stack like

```haskell
bar :: StateT X IO Y
```

then we might wish to apply `foo` to `bar`, but that is ill-typed, since `IO` is not the same as `StateT X IO`. In cases like these, we often use `lift`, but it’s not good enough here: `lift` _adds_ a new monad transformer to an action, but here we need to _remove_ a transformer. So we need a function with a type like this:

```haskell
unliftState :: StateT X IO Y -> IO Y
```

However, if you think about that type just a little bit, it’s clear something’s wrong: it throws away information, namely the state. You may remember that a `StateT X IO Y` action is equivalent to a function of type `X -> IO (Y, X)`, so our hypothetical `unliftState` function has two problems:

  1. We have no `X` to use as the initial state.

  2. We’ll lose any modifications `bar` made to the state, since the result type is just `Y`, not `(Y, X)`.

Clearly, we’ll need something more sophisticated, but what?

# A naïve solution

Given that `foo` doesn’t know anything about the state, we can’t easily thread it through `foo` itself. However, by using `runStateT` explicitly, we could do some of the state management ourselves:

```haskell
foo' :: StateT s IO a -> StateT s IO a
foo' m = do
  s <- get
  (v, s') <- lift $ foo (runStateT m s)
  put s'
  pure v
```

Do you see what’s going on there? It’s not actually very complicated: we get the current state, then pass it as the initial state to `runStateT`. This produces an action `IO (a, s)` that has *closed over* the current state. We can pass that action to `foo` without issue, since `foo` is polymorphic in the action’s return type. Finally, all we have to do is `put` the modified state back into the enclosing `StateT` computation, and we can get on with our business.

That strategy works okay when we only have one monad transformer, but it gets hairy quickly as soon as we have two or more. For example, if we had `baz :: ExceptT X (StateT Y IO) Z`, then we *could* do the same trick by getting the underlying

```haskell
Y -> IO (Either X Z, Y)
```

function, closing over the state, restoring it, and doing the appropriate case analysis to re-raise any `ExceptT` errors, but that’s a lot of work to do for every single function! What we’d like to do instead is somehow abstract over the pattern we used to write `foo'` in a way that scales to arbitrary monad transformers.

# The essence of `MonadBaseControl`

To build a more general solution for “unlifting” arbitrary monad transformers, we need to start thinking about monad transformer state. The technique we used to implement `foo'` operated on the following process:

  1. Capture the action’s input state and close over it.

  2. Package up the action’s output state with its result and run it.

  3. Restore the action’s output state into the enclosing transformer.

  4. Return the action’s result.

For `StateT s`, it turns out that the input state and output state are both `s`, but other monad transformers have state, too. Consider the input and output state for the following common monad transformers:

<div class="table-wrapper">
  <table class="no-line-wrapping">
    <thead><tr>
      <th>transformer</th>
      <th>representation</th>
      <th>input state</th>
      <th>output state</th>
    </tr></thead>
    <tr>
      <td><code>StateT s m a</code></td>
      <td><code>s -> m (a, s)</code></td>
      <td><code>s</code></td>
      <td><code>s</code></td>
    </tr>
    <tr>
      <td><code>ReaderT r m a</code></td>
      <td><code>r -> m a</code></td>
      <td><code>r</code></td>
      <td><code>()</code></td>
    </tr>
    <tr>
      <td><code>WriterT w m a</code></td>
      <td><code>m (a, w)</code></td>
      <td><code>()</code></td>
      <td><code>w</code></td>
    </tr>
  </table>
</div>

Notice how the input state is whatever is to the left of the `->`, while the output state is whatever extra information gets produced alongside the result. Using the same reasoning, we can also deduce the input and output state for compositions of multiple monad transformers, such as the following:

<div class="table-wrapper">
  <table class="no-line-wrapping">
    <thead><tr>
      <th>transformer</th>
      <th>representation</th>
      <th>input state</th>
      <th>output state</th>
    </tr></thead>
    <tr>
      <td><code>ReaderT r (WriterT w m) a</code></td>
      <td><code>r -> m (a, w)</code></td>
      <td><code>r</code></td>
      <td><code>w</code></td>
    </tr>
    <tr>
      <td><code>StateT s (ReaderT r m) a</code></td>
      <td><code>r -> s -> m (a, s)</code></td>
      <td><code>(r, s)</code></td>
      <td><code>s</code></td>
    </tr>
    <tr>
      <td><code>WriterT w (StateT s m) a</code></td>
      <td><code>s -> m ((a, w), s)</code></td>
      <td><code>s</code></td>
      <td><code>(w, s)</code></td>
    </tr>
  </table>
</div>

Notice that when monad transformers are composed, their states are composed, too. This is useful to keep in mind, since our goal is to capture the four steps above in a typeclass, polymorphic in the state of the monad transformers we need to lift through. At minimum, we need two new operations: one to capture the input state and close over it (step 1) and one to restore the output state (step 3). One class we might come up with could look like this:

```haskell
class MonadBase b m => MonadBaseControl b m | m -> b where
  type InputState m
  type OutputState m
  captureInputState :: m (InputState m)
  closeOverInputState :: m a -> InputState m -> b (a, OutputState m)
  restoreOutputState :: OutputState m -> m ()
```

If we can write instances of that typeclass for various transformers, we can use the class’s operations to implement `foo'` in a generic way that works with any combination of them:

```haskell
foo' :: MonadBaseControl IO m => m a -> m a
foo' m = do
  s <- captureInputState
  let m' = closeOverInputState m s
  (v, s') <- liftBase $ foo m'
  restoreOutputState s'
  pure v
```

So how do we implement those instances? Let’s start with `IO`, since that’s the base case:

```haskell
instance MonadBaseControl IO IO where
  type InputState IO = ()
  type OutputState IO = ()
  captureInputState = pure ()
  closeOverInputState m () = m <&> (, ())
  restoreOutputState () = pure ()
```

Not very exciting. The `StateT s` instance, on the other hand, is significantly more interesting:

```haskell
instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
  type InputState (StateT s m) = (s, InputState m)
  type OutputState (StateT s m) = (s, OutputState m)
  captureInputState = (,) <$> get <*> lift captureInputState
  closeOverInputState m (s, ss) = do
    ((v, s'), ss') <- closeOverInputState (runStateT m s) ss
    pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> put s
```

**This instance alone includes most of the key ideas behind `MonadBaseControl`.** There’s a lot going on, so let’s break it down, step by step:

  1. Start by examining the definitions of `InputState` and `OutputState`. Are they what you expected? You’d be forgiven for expecting the following:

     ```haskell
     type InputState (StateT s m) = s
     type OutputState (StateT s m) = s
     ```

     After all, that’s what we wrote in the table, isn’t it?

     However, if you give it a try, you’ll find it doesn’t work. `InputState` and `OutputState` must capture the state of the *entire* monad, not just a single transformer layer, so we have to combine the `StateT s` state with the state of the underlying monad. In the simplest case we get

     ```haskell
     InputState (StateT s IO) = (s, ())
     ```

     which is boring, but in a more complex case, we need to get something like this:

     ```haskell
     InputState (StateT s (ReaderT IO)) = (s, (r, ()))
     ```

     Therefore, `InputState (StateT s m)` combines `s` with `InputState m` in a tuple, and `OutputState` does the same.

  2. Moving on, take a look at `captureInputState` and `closeOverInputState`. Just as `InputState` and `OutputState` capture the state of the entire monad, these functions need to be inductive in the same way.

     `captureInputState` acquires the current state using `get`, and it combines it with the remaining monadic state using `lift captureInputState`. `closeOverInputState` uses the captured state to peel off the outermost `StateT` layer, then calls `closeOverInputState` recursively to peel off the rest of them.

  3. Finally, `restoreOutputState` restores the state of the underlying monad stack, then restores the `StateT` state, ensuring everything ends up back the way it’s supposed to be.

Take the time to digest all that—work through it yourself if you need to—as it’s a dense piece of code. Once you feel comfortable with it, take a look at the instances for `ReaderT` and `WriterT` as well:

```haskell
instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
  type InputState (ReaderT r m) = (r, InputState m)
  type OutputState (ReaderT r m) = OutputState m
  captureInputState = (,) <$> ask <*> lift captureInputState
  closeOverInputState m (s, ss) = closeOverInputState (runReaderT m s) ss
  restoreOutputState ss = lift (restoreOutputState ss)

instance (MonadBaseControl b m, Monoid w) => MonadBaseControl b (WriterT w m) where
  type InputState (WriterT w m) = InputState m
  type OutputState (WriterT w m) = (w, OutputState m)
  captureInputState = lift captureInputState
  closeOverInputState m ss = do
    ((v, s'), ss') <- closeOverInputState (runWriterT m) ss
    pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> tell s
```

Make sure you understand these instances, too. It should be easier this time, since they share most of their structure with the `StateT` instance, but note the asymmetry that arises from the differing input and output states. (It may even help to try and write these instances yourself, focusing on the types whenever you get stuck.)

If you feel alright with them, then congratulations: you’re already well on your way to grokking `MonadBaseControl`!

## Hiding the input state

So far, our implementation of `MonadBaseControl` works, but it’s actually slightly more complicated than it needs to be. As it happens, all valid uses of `MonadBaseControl` will always end up performing the following pattern:

```haskell
s <- captureInputState
let m' = closeOverInputState m s
```

That is, we close over the input state as soon as we capture it. We can therefore combine `captureInputState` and `closeOverInputState` into a single function:

```haskell
captureAndCloseOverInputState :: m a -> m (b (a, OutputState m))
```

What’s more, we no longer need the `InputState` associated type at all! This is an improvement, since it simplifies the API and removes the possibility for any misuse of the input state, since it’s never directly exposed. On the other hand, it has a more complicated type: it produces a monadic action *that returns another monadic action*. This can be a little more difficult to grok, which is why I presented the original version first, but it may help to consider how the above type arises naturally from the following definition:

```haskell
captureAndCloseOverInputState m = closeOverInputState m <$> captureInputState
```

Let’s update the `MonadBaseControl` class to incorporate this simplification:

```haskell
class MonadBase b m => MonadBaseControl b m | m -> b where
  type OutputState m
  captureAndCloseOverInputState :: m a -> m (b (a, OutputState m))
  restoreOutputState :: OutputState m -> m ()
```

We can then update all the instances to use the simpler API by simply fusing the definitions of `captureInputState` and `closeOverInputState` together:

```haskell
instance MonadBaseControl IO IO where
  type OutputState IO = ()
  captureAndCloseOverInputState m = pure (m <&> (, ()))
  restoreOutputState () = pure ()

instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
  type OutputState (StateT s m) = (s, OutputState m)
  captureAndCloseOverInputState m = do
    s <- get
    m' <- lift $ captureAndCloseOverInputState (runStateT m s)
    pure $ do
      ((v, s'), ss') <- m'
      pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> put s

instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
  type OutputState (ReaderT r m) = OutputState m
  captureAndCloseOverInputState m = do
    s <- ask
    lift $ captureAndCloseOverInputState (runReaderT m s)
  restoreOutputState ss = lift (restoreOutputState ss)

instance (MonadBaseControl b m, Monoid w) => MonadBaseControl b (WriterT w m) where
  type OutputState (WriterT w m) = (w, OutputState m)
  captureAndCloseOverInputState m = do
    m' <- lift $ captureAndCloseOverInputState (runWriterT m)
    pure $ do
      ((v, s'), ss') <- m'
      pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> tell s
```

This is already very close to a full `MonadBaseControl` implementation. The `captureAndCloseOverInputState` implementations are getting a little out of hand, but bear with me—they’ll get simpler before this blog post is over.

## Coping with partiality

Our `MonadBaseControl` class now works with `StateT`, `ReaderT`, and `WriterT`, but one transformer we haven’t considered is `ExceptT`. Let’s try to extend our table from before with a row for `ExceptT`:

<div class="table-wrapper">
  <table class="no-line-wrapping">
    <thead><tr>
      <th>transformer</th>
      <th>representation</th>
      <th>input state</th>
      <th>output state</th>
    </tr></thead>
    <tr>
      <td><code>ExceptT e m a</code></td>
      <td><code>m (Either e a)</code></td>
      <td><code>()</code></td>
      <td><code>???</code></td>
    </tr>
  </table>
</div>

Hmm… what *is* the output state for `ExceptT`?

The answer can’t be `e`, since we might not end up with an `e`—the computation might not fail. `Maybe e` would be closer… could that work?

Well, let’s try it. Let’s write a `MonadBaseControl` instance for `ExceptT`:

```haskell
instance MonadBaseControl b m => MonadBaseControl b (ExceptT e m) where
  type OutputState (ExceptT e m) = (Maybe e, OutputState m)
  captureAndCloseOverInputState m = do
    m' <- lift $ captureAndCloseOverInputState (runExceptT m)
    pure $ do
      ((v, s'), ss') <- m'
      pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> case s of
    Just e -> throwError e
    Nothing -> pure ()
```

Sadly, the above implementation doesn’t typecheck; it is rejected with the following type error:

```
• Couldn't match type ‘Either e a’ with ‘(a, Maybe e)’
  Expected type: m (b ((a, Maybe e), OutputState m))
    Actual type: m (b (Either e a, OutputState m))
• In the second argument of ‘($)’, namely
    ‘captureAndCloseOverInputState (runExceptT m)’
  In a stmt of a 'do' block:
    m' <- lift $ captureAndCloseOverInputState (runExceptT m)
  In the expression:
    do m' <- lift $ captureAndCloseOverInputState (runExceptT m)
       return do ((v, s'), ss') <- m'
                 pure (v, (s', ss'))
```

We promised a `(a, Maybe e)`, but we have an `Either e a`, and there’s certainly no way to get the former from the latter. Are we stuck? (If you’d like, take a moment to think about how you’d solve this type error before moving on, as it may be helpful for understanding the following solution.)

The fundamental problem here is *partiality*. The type of the `captureAndCloseOverInputState` method always produces an action in the base monad that includes an `a` *in addition* to some other output state. But `ExceptT` is different: when it an error is raised, it doesn’t produce an `a` at all—it only produces an `e`. Therefore, as written, it’s impossible to give `ExceptT` a `MonadBaseControl` instance.

Of course, we’d very much *like* to give `ExceptT` a `MonadBaseControl` instance, so that isn’t very satisfying. Somehow, we need to change `captureAndCloseOverInputState` so that it doesn’t always need to produce an `a`. There are a few ways we could accomplish that, but an elegant way to do it is this:

```haskell
class MonadBase b m => MonadBaseControl b m | m -> b where
  type WithOutputState m a
  captureAndCloseOverInputState :: m a -> m (b (WithOutputState m a))
  restoreOutputState :: WithOutputState m a -> m a
```

We’ve replaced the old `OutputState` associated type with a new `WithOutputState` type, and the key difference between them is that `WithOutputState` describes the type of a *combination* of the result (of type `a`) and the output state, rather than describing the type of the output state alone. For total monad transformers like `StateT`, `ReaderT`, and `WriterT`, `WithOutputState m a` will just be a tuple of the result value and the output state, the same as before. For example, here’s an updated `MonadBaseControl` instance for `StateT`:

```haskell
instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
  type WithOutputState (StateT s m) a = WithOutputState m (a, s)
  captureAndCloseOverInputState m = do
    s <- get
    lift $ captureAndCloseOverInputState (runStateT m s)
  restoreOutputState ss = do
    (a, s) <- lift $ restoreOutputState ss
    put s
    pure a
```

Before we consider how this helps us with `ExceptT`, let’s pause for a moment and examine the revised `StateT` instance in detail, as there are some new things going on here:

  - Take a close look at the definition of `WithOutputState (StateT s m) a`. Note that we’ve defined it to be `WithOutputState m (a, s)`, *not* `(WithOutputState m a, s)`. Consider, for a moment, the difference between these types. Can you see why we used the former, not the latter?

    If it’s unclear to you, that’s okay—let’s illustrate the difference with an example. Consider two similar monad transformer stacks:

    ```haskell
    m1 :: StateT s (ExceptT e IO) a
    m2 :: ExceptT e (StateT s IO) a
    ```

    Both these stacks contain `StateT` and `ExceptT`, but they are layered in a different order. What’s the difference? Well, consider what `m1` and `m2` return once fully unwrapped:

    ```haskell
    runExceptT (runStateT m1 s) :: m (Either e (a, s))
    runStateT (runExceptT m2) s :: m (Either e a, s)
    ```

    These results are meaningfully different: in `m1`, the state is discarded if an error is raised, but in `m2`, the final state is always returned, even if the computation is aborted. What does this mean for `WithOutputState`?

    Here’s the important detail: **the state is discarded when `ExceptT` is “inside” `StateT`, not the other way around.** This can be counterintuitive, since the `s` ends up *inside* the `Either` when the `StateT` constructor is on the *outside* and vice versa. This is really just a property of how monad transformers compose, not anything specific to `MonadBaseControl`, so an explanation of why this happens is outside the scope of this blog post, but the relevant insight is that the `m` in `StateT s m a` controls the eventual action’s output state.

    If we had defined `WithOutputState (StateT s m) a` to be `(WithOutputState m a, s)`, we’d be in a pickle, since `m` would be unable to influence the presence of `s` in the output state. Therefore, we have no choice but to use `WithOutputState m (a, s)`. (If you are still confused by this, try it yourself; you’ll find that there’s no way to make the other definition typecheck.)

  - Now that we’ve developed an intuitive understanding of why `WithOutputState` must be defined the way it is, let’s look at things from another perspective. Consider the type of `runStateT` once more:

    ```haskell
    runStateT :: StateT s m a -> s -> m (a, s)
    ```

    Note that the result type is `m (a, s)`, with the `m` on the outside. As it happens, this correspondence simplifies the definition of `captureAndCloseOverInputState`, since we no longer have to do any fiddling with its result—it’s already in the proper shape, so we can just return it directly.

  - Finally, this instance illustrates an interesting change to `restoreOutputState`. Since the `a` is now packed inside the `WithOutputState m a` value, the caller of `captureAndCloseOverInputState` needs some way to get the `a` back out! Conveniently, `restoreOutputState` can play that role, both restoring the output state and unpacking the result.

    Even ignoring partial transformers like `ExceptT`, this is an improvement over the old API, as it conveniently prevents the programmer from forgetting to call `restoreOutputState`. However, as we’ll see shortly, it is much more than a convenience: once `ExceptT` comes into play, it is essential!

With those details addressed, let’s return to `ExceptT`. Using the new interface, writing an instance for `ExceptT` is not only possible, it’s actually rather easy:

```haskell
instance MonadBaseControl b m => MonadBaseControl b (ExceptT e m) where
  type WithOutputState (ExceptT e m) a = WithOutputState m (Either e a)
  captureAndCloseOverInputState m =
    lift $ captureAndCloseOverInputState (runExceptT m)
  restoreOutputState ss =
    either throwError pure =<< lift (restoreOutputState ss)
```

This instance illustrates why it’s so crucial that `restoreOutputState` have the aforementioned dual role: it must handle the case where no `a` exists at all! In the case of `ExceptT`, it restores the state in the enclosing monad by re-raising an error.

Now all that’s left to do is update the other instances:

```haskell
instance MonadBaseControl IO IO where
  type WithOutputState IO a = a
  captureAndCloseOverInputState = pure
  restoreOutputState = pure

instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
  type WithOutputState (ReaderT r m) a = WithOutputState m a
  captureAndCloseOverInputState m = do
    s <- ask
    lift $ captureAndCloseOverInputState (runReaderT m s)
  restoreOutputState ss = lift $ restoreOutputState ss

instance (MonadBaseControl b m, Monoid w) => MonadBaseControl b (WriterT w m) where
  type WithOutputState (WriterT w m) a = WithOutputState m (a, w)
  captureAndCloseOverInputState m =
    lift $ captureAndCloseOverInputState (runWriterT m)
  restoreOutputState ss = do
    (a, s) <- lift $ restoreOutputState ss
    tell s
    pure a
```

Finally, we can update our lifted variant of `foo` to use the new interface so it will work with transformer stacks that include `ExceptT`:

```haskell
foo' :: MonadBaseControl IO m => m a -> m a
foo' m = do
  m' <- captureAndCloseOverInputState m
  restoreOutputState =<< liftBase (foo m')
```

At this point, it’s worth considering something: although getting the `MonadBaseControl` class and instances right was a lot of work, the resulting `foo'` implementation is actually incredibly simple. That’s a good sign, since we only have to write the `MonadBaseControl` instances once (in a library), but we have to write functions like `foo'` quite often.

# Scaling to the real `MonadBaseControl`

The `MonadBaseControl` class we implemented in the previous section is complete. It is a working, useful class that is equivalent in power to [the “real” `MonadBaseControl` class in the `monad-control` library][monad-control:MonadBaseControl]. However, if you compare the two, you’ll notice that the version in `monad-control` looks a little bit different. What gives?

Let’s compare the two classes side by side:

```haskell
-- ours
class MonadBase b m => MonadBaseControl b m | m -> b where
  type WithOutputState m a
  captureAndCloseOverInputState :: m a -> m (b (WithOutputState m a))
  restoreOutputState :: WithOutputState m a -> m a

-- theirs
class MonadBase b m => MonadBaseControl b m | m -> b where
  type StM m a
  liftBaseWith :: (RunInBase m b -> b a) -> m a
  restoreM :: StM m a -> m a
```

Let’s start with the similarities, since those are easy:

  - Our `WithOutputState` associated type is precisely equivalent to their `StM` associated type, they just use a (considerably) shorter name.

  - Likewise, our `restoreOutputState` method is precisely equivalent to their `restoreM` method, simply under a different name.

That leaves `captureAndCloseOverInputState` and `liftBaseWith`. Those two methods both do similar things, but they aren’t identical, and that’s where all the differences lie. To understand `liftBaseWith`, let’s start by inlining the definition of the `RunInBase` type alias so we can see the fully-expanded type:

```haskell
liftBaseWith
  :: MonadBaseControl b m
  => ((forall c. m c -> b (StM m c)) -> b a)
  -> m a
```

That type is complicated! However, if we break it down, hopefully you’ll find it’s not as scary as it first appears. Let’s reimplement the `foo'` example from before using `liftBaseWith` to show how this version of `MonadBaseControl` works:

```haskell
foo' :: MonadBaseControl IO m => m a -> m a
foo' m = do
  s <- liftBaseWith $ \runInBase -> foo (runInBase m)
  restoreM s
```

This is, in some ways, superficially similar to the version we wrote using our version of `MonadBaseControl`. Just like in our version, we capture the input state, apply `foo` in the `IO` monad, then restore the state. But what exactly is doing the state capturing, and what is `runInBase`?

Let’s start by adding a type annotation to `runInBase` to help make it a little clearer what’s going on:

```haskell
foo' :: forall m a. MonadBaseControl IO m => m a -> m a
foo' m = do
  s <- liftBaseWith $ \(runInBase :: forall b. m b -> IO (StM m b)) ->
    foo (runInBase m)
  restoreM s
```

That type should look sort of recognizable. If we replace `StM` with `WithOutputState`, then we get a type that looks very similar to that of our original `closeOverInputState` function, except it doesn’t need to take the input state as an argument. How does that work?

Here’s the trick: `liftBaseWith` starts by capturing the input state, just as before. However, it then builds a function, `runInBase`, which is like `closeOverInputState` partially-applied to the input state it captured. It hands that function to us, and we’re free to apply it to `m`, which produces the `IO (StM m a)` action we need, and we can now pass that action to `foo`. The result is returned in the outer monad, and we restore the state using `restoreM`.

## Sharing the input state

At first, this might seem needlessly complicated. When we first started, we separated capturing the input state and closing over it into two separate operations (`captureInputState` and `closeOverInputState`), but we eventually combined them so that we could keep the input state hidden. Why does `monad-control` split them back into two operations again?

As it turns out, when lifting `foo`, there’s no advantage to the more complicated API of `monad-control`. In fact, we could implement our `captureAndCloseOverInputState` operation in terms of `liftBaseWith`, and we could use that to implement `foo'` the same way we did before:

```haskell
captureAndCloseOverInputState :: MonadBaseControl b m => m a -> m (b (StM m a))
captureAndCloseOverInputState m = liftBaseWith $ \runInBase -> pure (runInBase m)

foo' :: MonadBaseControl IO m => m a -> m a
foo' m = do
  m' <- captureAndCloseOverInputState m
  restoreM =<< liftBase (foo m')
```

However, that approach has a downside once we need to lift more complicated functions. `foo` is exceptionally simple, as it only accepts a single input argument, but what if we wanted to lift a more complicated function that took *two* monadic arguments, such as this one:

```haskell
bar :: IO a -> IO a -> IO a
```

We could implement that by calling `captureAndCloseOverInputState` twice, like this:

```haskell
bar' :: MonadBaseControl IO m => m a -> m a -> m a
bar' ma mb = do
  ma' <- captureAndCloseOverInputState ma
  mb' <- captureAndCloseOverInputState mb
  restoreM =<< liftBase (bar ma' mb')
```

However, that would capture the monadic state twice, which is rather inefficient. By using `liftBaseWith`, the state capturing is done just once, and it’s shared between all calls to `runInBase`:

```haskell
bar' :: MonadBaseControl IO m => m a -> m a -> m a
bar' ma mb = do
  s <- liftBaseWith $ \runInBase ->
    bar (runInBase ma) (runInBase mb)
  restoreM s
```

By providing a “running” function (`runInBase`) instead of direct access to the input state, `liftBaseWith` allows sharing the captured input state between multiple actions without exposing it directly.

## Sidebar: continuation-passing and impredicativity

One last point before we move on: although the above explains why `captureAndCloseOverInputState` is insufficient, you may be left wondering why `liftBaseWith` can’t just *return* `runInBase`. Why does it need to be given a continuation? After all, it would be nicer if we could just write this:

```haskell
bar' :: MonadBaseControl IO m => m a -> m a -> m a
bar' ma mb = do
  runInBase <- askRunInBase
  restoreM =<< liftBase (bar (runInBase ma) (runInBase mb))
```

To understand the problem with a hypothetical `askRunInBase` function, remember that the type of `runInBase` is polymorphic:

```haskell
runInBase :: forall a. m a -> b (StM m a)
```

This is important, since if you need to lift a function with a type like

```haskell
baz :: IO b -> IO c -> IO (Either b c)
```

then you’ll want to instantiate that `a` variable with two different types. We’d need to retain that power in `askRunInBase`, so it would need to have the following type:

```haskell
askRunInBase :: MonadBaseControl b m => m (forall a. m a -> b (StM m a))
```

Sadly, that type is illegal in Haskell. Type constructors must be applied to monomorphic types, but in the above type signature, `m` is applied to a polymorphic type.[^1] The `RankNTypes` GHC extension introduces a single exception: the `(->)` type constructor is special and may be applied to polymorphic types. That’s why `liftBaseWith` is legal, but `askRunInBase` is not: since `liftBaseWith` is passed a higher-order function that receives `runInBase` as an argument, the polymorphic type appears immediately under an application of `(->)`, which is allowed.

The aforementioned restriction means we’re basically out of luck, but if you *really* want `askRunInBase`, there is a workaround. GHC is perfectly alright with a field of a datatype being polymorphic, so we can define a newtype that wraps a suitably-polymorphic function:

```haskell
newtype RunInBase b m = RunInBase (forall a. m a -> b (StM m a))
```

We can now alter `askRunInBase` to return our newtype, and we can implement it in terms of `liftBaseWith`:[^2]

```haskell
askRunInBase :: MonadBaseControl b m => m (RunInBase b m)
askRunInBase = liftBaseWith $ \runInBase -> pure $ RunInBase runInBase
```

To use `askRunInBase`, we have to pattern match on the `RunInBase` constructor, but it isn’t very noisy, since we can do it directly in a `do` binding. For example, we could implement a lifted version of `baz` this way:

```haskell
baz' :: MonadBaseControl IO m => m a -> m b -> m (Either a b)
baz' ma mb = do
  RunInBase runInBase <- askRunInBase
  s <- liftBase (baz (runInBase ma) (runInBase mb))
  bitraverse restoreM restoreM s
```

As of version 1.0.2.3, `monad-control` does not provide a newtype like `RunInBase`, so it also doesn’t provide a function like `askRunInBase`. For now, you’ll have to use `liftBaseWith`, but it might be a useful future addition to the library.

# Pitfalls

At this point in the blog post, we’ve covered the essentials of `MonadBaseControl`: how it works, how it’s designed, and how you might go about using it. However, so far, we’ve only considered situations where `MonadBaseControl` works well, and I’ve intentionally avoided examples where the technique breaks down. In this section, we’re going to take a look at the pitfalls and drawbacks of `MonadBaseControl`, plus some ways they can be mitigated.

## No polymorphism, no lifting

All of the pitfalls of `MonadBaseControl` stem from the same root problem, and that’s the particular technique it uses to save and restore monadic state. We’ll start by considering one of the simplest ways that technique is thwarted, and that’s monomorphism. Consider the following two functions:

```haskell
poly :: IO a -> IO a
mono :: IO X -> IO X
```

Even after all we’ve covered, it may surprise you to learn that although `poly` can be easily lifted to `MonadBaseControl IO m => m a -> m a`, it’s *impossible* to lift `mono` to `MonadBaseControl IO m => m X -> m X`. It’s a little unintuitive, as we often think of polymorphic types as being more complicated (so surely lifting polymorphic functions ought to be harder), but in fact, it’s the flexibility of polymorphism that allows `MonadBaseControl` to work in the first place.

To understand the problem, remember that when we lift a function of type `forall a. b a -> b a` using `MonadBaseControl`, we actually instantiate `a` to `(StM m c)`. That produces a function of type `b (StM m c) -> b (StM m c)`, which is isomorphic to the `m c -> m c` type we want. The instantiation step is easily overlooked, but it’s crucial, since otherwise we have no way to thread the state through the otherwise opaque function we’re trying to lift!

In the case of `mono`, that’s exactly the problem we’re faced with. `mono` will not accept an `IO (StM m X)` as an argument, only precisely an `IO X`, so we can’t pass along the monadic state. For all its machinery, `MonadBaseControl` is no help at all if no polymorphism is involved. Trying to generalize `mono` without modifying its implementation is a lost cause.

## The dangers of discarded state

Our inability to lift `mono` is frustrating, but at least it’s conclusively impossible. In practice, however, many functions lie in an insidious in-between: polymorphic enough to be lifted, but not without compromises. The simplest of these functions have types such as the following:

```haskell
sideEffect :: IO a -> IO ()
```

Unlike `mono`, it’s entirely possible to lift `sideEffect`:

```haskell
sideEffect' :: MonadBaseControl IO m => m a -> m ()
sideEffect' m = liftBaseWith $ \runInBase -> sideEffect (runInBase m)
```

This definition typechecks, but you may very well prefer it didn’t, since it has a serious problem: any changes made by `m` to the monadic state are completely discarded once `sideEffect'` returns! Since `sideEffect'` never calls `restoreM`, there’s no way the state of `m` can be any different from the original state, but it’s impossible to call `restoreM` since we don’t actually get an `StM m ()` result from `sideEffect`.

Sometimes this may be acceptable, since some monad transformers don’t actually have any output state anyway, such as `ReaderT r`. In other cases, however, `sideEffect'` could be a bug waiting to happen. One way to make `sideEffect'` safe would be to add a `StM m a ~ a` constraint to its context, since that guarantees the monad transformers being lifted through are stateless, and nothing is actually being discarded. Of course, that significantly restricts the set of monad transformers that can be lifted through.

### Rewindable state

One scenario where state discarding can actually be useful is operations with so-called rewindable or transactional state. The most common example of such an operation is `catch`:

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

When lifted, state changes from the action *or* from the exception handler will be “committed,” but never both. If an exception is raised during the computation, those state changes are discarded (“rewound”), giving `catch` a kind of backtracking semantics. This behavior arises naturally from the way a lifted version of `catch` must be implemented:

```haskell
catch' :: (Exception e, MonadBaseControl IO m) => m a -> (e -> m a) -> m a
catch' m f = do
  s <- liftBaseWith $ \runInBase ->
    catch (runInBase m) (runInBase . f)
  restoreM s
```

If `m` raises an exception, it will never return an `StM m a` value, so there’s no way to get ahold of any of the state changes that happened before the exception. Therefore, the only option is to discard that state.

This behavior is actually quite useful, and it’s definitely not unreasonable. However, useful or not, it’s inconsistent with state changes to mutable values like `IORef`s or `MVar`s (they stay modified whether an exception is raised or not), so it can still be a gotcha. Either way, it’s worth being aware of.

### Partially discarded state

The next function we’re going to examine is `finally`:

```haskell
finally :: IO a -> IO b -> IO a
```

This function has a similar type to `catch`, and it even has similar semantics. Like `catch`, `finally` can be lifted, but unlike `catch`, its state *can’t* be given any satisfying treatment. The only way to implement a lifted version is

```haskell
finally' :: MonadBaseControl IO m => m a -> m b -> m a
finally' ma mb = do
  s <- liftBaseWith $ \runInBase ->
    finally (runInBase ma) (runInBase mb)
  restoreM s
```

which always discards all state changes made by the second argument. This is clear just from looking at `finally`’s type: since `b` doesn’t appear anywhere in the return type, there’s simply no way to access that action’s result, and therefore no way to access its modified state.

However, don’t despair: there actually *is* a way to produce a lifted version of `finally` that preserves all state changes. It can’t be done by lifting `finally` directly, but if we reimplement `finally` in terms of simpler lifted functions that are more amenable to lifting, we can produce a lifted version of `finally` that preserves all the state:[^3]

```haskell
finally' :: MonadBaseControl IO m => m a -> m b -> m a
finally' ma mb = mask' $ \restore -> do
  a <- liftBaseWith $ \runInBase ->
    try (runInBase (restore ma))
  case a of
    Left e -> mb *> liftBase (throwIO (e :: SomeException))
    Right s -> restoreM s <* mb
```

This illustrates an important (and interesting) point about `MonadBaseControl`: whether or not an operation can be made state-preserving is not a fundamental property of the operation’s type, but rather a property of the types of the exposed primitives. There is sometimes a way to implement a state-preserving variant of operations that might otherwise seem unliftable given the right primitives and a bit of cleverness.

### Forking state

As a final example, I want to provide an example where the state may not actually be discarded *per se*, just inaccessible. Consider the type of `forkIO`:

```haskell
forkIO :: IO () -> IO ThreadId
```

Although `forkIO` isn’t actually polymorphic in its argument, we can convert *any* `IO` action to one that produces `()` via `void`, so it might as well be. Therefore, we can lift `forkIO` in much the same way we did with `sideEffect`:

```haskell
forkIO' :: MonadBaseControl IO m => m () -> m ThreadId
forkIO' m = liftBaseWith $ \runInBase -> forkIO (void $ runInBase m)
```

As with `sideEffect`, we can’t recover the output state, but in this case, there’s a fundamental reason that goes deeper than the types: we’ve forked off a concurrent computation! We’ve therefore split the state in two, which might be what we want… but it also might not. `forkIO` is yet another illustration that it’s important to think about the state-preservation semantics when using `MonadBaseControl`, or you may end up with a bug!

# `MonadBaseControl` in context

Congratulations: you’ve made it through most of this blog post. If you’ve followed everything so far, you now understand `MonadBaseControl`. All the tricky parts are over. However, before wrapping up, I’d like to add a little extra information about how `MonadBaseControl` relates to various other parts of the Haskell ecosystem. In practice, that information can be as important as understanding `MonadBaseControl` itself.

## The remainder of `monad-control`

If you look at [the documentation for `monad-control`][monad-control:Control.Monad.Trans.Control], you’ll find that it provides more than just the `MonadBaseControl` typeclass. I’m not going to cover everything else in detail in this blog post, but I do want to touch upon it briefly.

First off, you should definitely take a look at the handful of helper functions provided by `monad-control`, such as [`control`](https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#v:control) and [`liftBaseOp_`](https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#v:liftBaseOp_). These functions provide support for lifting common function types without having to use `liftBaseWith` directly. It’s useful to understand `liftBaseWith`, since it’s the most general way to use `MonadBaseControl`, but in practice, it is simpler and more readable to use the more specialized functions wherever possible. Many of the examples in this very blog post could be simplified using them, and I only stuck to `liftBaseWith` to introduce as few new concepts at a time as possible.

Second, I’d like to mention the related [`MonadTransControl`](https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#t:MonadTransControl) typeclass. You hopefully remember from earlier in the blog post how we defined `MonadBaseControl` instances inductively so that we could lift all the way down to the base monad. `MonadTransControl` is like `MonadBaseControl` if it intentionally did *not* do that—it allows lifting through a single transformer at a time, rather than through all of them at once.

Usually, `MonadTransControl` is not terribly useful to use directly (though I did use it once [in a previous blog post of mine](/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/#making-mtls-classes-derivable) to help derive instances of mtl-style classes), but it *is* useful for implementing `MonadBaseControl` instances for your own transformers. If you define a `MonadTransControl` instance for your monad transformer, you can get a `MonadBaseControl` implementation for free using the provided [`ComposeSt`](https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#t:ComposeSt), [`defaultLiftBaseWith`](https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#v:defaultLiftBaseWith), and [`defaultRestoreM`](https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#v:defaultRestoreM) bindings; see the documentation for more details.

## `lifted-base` and `lifted-async`

If you’re going to use `MonadBaseControl`, the [`lifted-base`][lifted-base] and [`lifted-async`][lifted-async] packages are good to know about. As their names imply, they provide lifted versions of bindings in the `base` and `async` packages, so you can use them directly without needing to lift them yourself. For example, if you needed a lifted version of `mask` from `Control.Exception`, you could swap it for the `mask` export from `Control.Exception.Lifted`, and everything would mostly just work (though always be sure to check the documentation for any caveats on state discarding).

## Relationship to `MonadUnliftIO`

Recently, FP Complete has developed the [`unliftio`](https://hackage.haskell.org/package/unliftio) package as an alternative to `monad-control`. It provides the [`MonadUnliftIO`](https://hackage.haskell.org/package/unliftio-core-0.1.2.0/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO) typeclass, which is similar in spirit to `MonadBaseControl`, but heavily restricted: it is specialized to `IO` as the base monad, and it *only* allows instances for stateless monads, such as `ReaderT`. This is designed to encourage the so-called [`ReaderT` design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern), which avoids ever using stateful monads like `ExceptT` or `StateT` over `IO`, encouraging the use of `IO` exceptions and mutable variables (e.g. `MVar`s or `TVar`s) instead.

I should be clear: I really like most of what FP Complete has done—to this day, I still use `stack` as my Haskell build tool of choice—and I think the suggestions given in the aforementioned “`ReaderT` design pattern” blog post have real weight to them. I have a deep respect for Michael Snoyman’s commitment to opinionated, user-friendly tools and libraries. But truthfully, I can’t stand `MonadUnliftIO`.

`MonadUnliftIO` is designed to avoid all the complexity around state discarding that `MonadBaseControl` introduces, and on its own, that’s a noble goal. Safety first, after all. The problem is that `MonadUnliftIO` really is extremely limiting, and what’s more, it can actually be trivially encoded in terms of `MonadBaseControl` as follows:

```haskell
type MonadUnliftIO m = (MonadBaseControl IO m, forall a. StM m a ~ a)
```

This alias can be used to define safe, lifted functions that never discard state while still allowing functions that *can* be safely lifted through stateful transformers to do so. Indeed, the [`Control.Concurrent.Async.Lifted.Safe`](https://hackage.haskell.org/package/lifted-async-0.10.0.4/docs/Control-Concurrent-Async-Lifted-Safe.html) module from `lifted-async` does exactly that (albeit with a slightly different formulation than the above alias).

To be fair, the `unliftio` README does address this in its [comparison section](https://github.com/fpco/unliftio/tree/bb2e26e7fbbaebb15555f417ba9753a76b3218b2/unliftio#monad-control):

> `monad-control` allows us to unlift both styles. In theory, we could write a variant of `lifted-base` that never does state discards […] In other words, this is an advantage of `monad-control` over `MonadUnliftIO`. We've avoided providing any such extra typeclass in this package though, for two reasons:
>
>  - `MonadUnliftIO` is a simple typeclass, easy to explain. We don't want to complicated [sic] matters […]
>
>  - Having this kind of split would be confusing in user code, when suddenly [certain operations are] not available to us.

In other words, the authors of `unliftio` felt that `MonadBaseControl` was simply not worth the complexity, and they could get away with `MonadUnliftIO`. Frankly, if you feel the same way, by all means, use `unliftio`. I just found it too limiting given the way I write Haskell, plain and simple.

# Recap

So ends another long blog post. As often seems the case, I set out to write something short, but I ended up writing well over 5,000 words. I suppose that means I learned something from this experience, too: `MonadBaseControl` is more complicated than I had anticipated! Maybe there’s something to take away from that.

In any case, it’s over now, so I’d like to briefly summarize what we’ve covered:

  - [`MonadBaseControl`][monad-control:MonadBaseControl] allows us to lift higher-order monadic operations.

  - It operates by capturing the current monadic state and explicitly threading it through the action in the base monad before restoring it.

  - That technique works well for polymorphic operations for the type `forall a. b a -> b a`, but it can be tricky or even impossible for more complex operations, sometimes leading to discarded state.

    This can sometimes be mitigated by restricting certain operations to stateless monads using a `StM m a ~ a` constraint, or by reimplementing the operation in terms of simpler primitives.

  - The [`lifted-base`][lifted-base] and [`lifted-async`][lifted-async] packages provide lifted versions of existing operations, avoiding the need to lift them yourself.

As with many abstractions in Haskell, don’t worry too much if you don’t have a completely firm grasp of `MonadBaseControl` at first. Insight often comes with repeated experience, and `monad-control` can still be used in useful ways even without a perfect understanding. My hope is that this blog post has helped you build intuitions about `MonadBaseControl` even if some of the underlying machinery remains a little fuzzy, and I hope it can also serve as a reference for those who want or need to understand (or just be reminded of) all the little details.

Finally, I’ll admit `MonadBaseControl` isn’t especially elegant or beautiful as Haskell abstractions go. In fact, in many ways, it’s a bit of a kludge! Perhaps, in time, effect systems will evolve and mature so that it and its ilk are no longer necessary, and they may become distant relics of an inferior past. But in the meantime, it’s here, it’s useful, and I think it’s worth embracing. If you’ve shied away from it in the past, I hope I’ve illuminated it enough to make you consider giving it another try.

[^0]: One example of a function with that type is `mask_`.

[^1]: Types with polymorphic types under type constructors are called *impredicative*. GHC technically has limited support for impredicativity via the `ImpredicativeTypes` language extension, but as of GHC 8.8, it has been fairly broken for some time. A fix is apparently being worked on, but even if that effort is successful, I don’t know what impact it will have on type inference.

[^2]: Note that `askRunInBase = liftBaseWith (pure . RunInBase)` does *not* typecheck, as it would require impredicative polymorphism: it would require instantiating the type of `(.)` with polymorphic types. The version using `($)` works because GHC actually has special typechecking rules for `($)`! Effectively, `f $ x` is really syntax in GHC.

[^3]: Assume that `mask'` is a suitably lifted version of `mask` (which can in fact be made state-preserving).


[lifted-async]: http://hackage.haskell.org/package/lifted-async
[lifted-base]: http://hackage.haskell.org/package/lifted-base
[monad-control:Control.Monad.Trans.Control]: https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html
[monad-control:MonadBaseControl]: https://hackage.haskell.org/package/monad-control-1.0.2.3/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl

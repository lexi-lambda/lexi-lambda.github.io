    Title: No, dynamic type systems are not inherently more open
    Date: 2020-01-19T00:00:00
    Tags: types, haskell, programming languages, functional programming

Internet debates about typing disciplines continue to be plagued by a pervasive myth that dynamic type systems are inherently better at modeling “open world” domains. The argument usually goes like this: the goal of static typing is to pin everything down as much as possible, but in the real world, that just isn’t practical. Real systems should be loosely coupled and worry about data representation as little as possible, so dynamic types lead to a more robust system in the large.

This story sounds compelling, but it isn’t true. The flaw is in the premise: static types are *not* about “classifying the world” or pinning down the structure of every value in a system. The reality is that static type systems allow specifying exactly how much a component needs to know about the structure of its inputs, and conversely, how much it doesn’t. Indeed, in practice static type systems excel at processing data with only a partially-known structure, as they can be used to ensure application logic doesn’t accidentally assume too much.

# Two typing fallacies

I’ve wanted to write this blog post for a while, but what finally made me decide to do it were misinformed comments responding to [my previous blog post][parse-dont-validate]. Two comments in particular caught my eye, [the first of which was posted on /r/programming](https://www.reddit.com/r/programming/comments/dt0w63/parse_dont_validate/f6ulpsy/):

> Strongly disagree with the post […] it promotes a fundamentally entangled and static view of the world. It assumes that we can or should theorize about what is "valid" input at the edge between the program and the world, thus introducing a strong sense of coupling through the entire software, where failure to conform to some schema will automatically crash the program.
>
> This is touted as a feature here but imagine if the internet worked like this. A server changes their JSON output, and we need to recompile and reprogram the entire internet. This is the static view that is promoted as a feature here. […] The "parser mentality" is fundamentally rigid and global, whereas robust system design should be decentralised and leave interpretation of data to the receiver.

Given the argument being made in the blog post—that you should use precise types whenever possible—one can see where this misinterpretation comes from. How could a proxy server possibly be written in such a style, since it cannot anticipate the structure of its payloads? The commenter’s conclusion is that strict static typing is at odds with programs that don’t know the structure of their inputs ahead of time.

[The second comment was left on Hacker News](https://news.ycombinator.com/item?id=21479933), and it is significantly shorter than the first one:

> What would be the type signature of, say, Python's `pickle.load()`?

This is a different kind of argument, one that relies on the fact that the types of reflective operations may depend on runtime values, which makes them challenging to capture with static types. This argument suggests that static types limit expressiveness because they forbid such operations outright.

Both these arguments are fallacious, but in order to show why, we have to make explicit an implicit claim. The two comments focus primarily on illustrating how static type systems can’t process data of an unknown shape, but they simultaneously advance an implicit belief: that dynamically typed languages *can* process data of an unknown shape. As we’ll see, this belief is misguided; programs are not capable of processing data of a truly unknown shape regardless of typing discipline, and static type systems only make already-present assumptions explicit.

# You can’t process what you don’t know

The claim is simple: in a static type system, you must declare the shape of data ahead of time, but in a dynamic type system, the type can be, well, dynamic! It sounds self-evident, so much so that Rich Hickey has practically built a speaking career upon its emotional appeal. The only problem is it isn’t true.

The hypothetical scenario usually goes like this. Say you have a distributed system, and services in the system emit events that can be consumed by any other service that might need them. Each event is accompanied by a payload, which listening services can use to inform further action. The payload itself is minimally-structured, schemaless data encoded using a generic interchange format such as JSON or [EDN][edn].

As a simple example, a login service might emit an event like this one whenever a new user signs up:

```json
{
  "event_type": "signup",
  "timestamp": "2020-01-19T05:37:09Z",
  "data": {
    "user": {
      "id": 42,
      "name": "Alyssa",
      "email": "alyssa@example.com"
    }
  }
}
```

Some downstream services might listen for these `signup` events and take further action whenever they are emitted. For example, a transactional email service might send a welcome email whenever a new user signs up. If the service were written in JavaScript, the handler might look something like this:

```js
const handleEvent = ({ event_type, data }) => {
  switch (event_type) {
    case 'login':
      /* ... */
      break
    case 'signup':
      sendEmail(data.user.email, `Welcome to Blockchain Emporium, ${data.user.name}!`)
      break
  }
}
```

But what if this service were written in Haskell instead? Being good, reality-fearing Haskell programmers who [parse, not validate][parse-dont-validate], the Haskell code might look something like this, instead:

```haskell
data Event = Login LoginPayload | Signup SignupPayload
data LoginPayload = LoginPayload { userId :: Int }
data SignupPayload = SignupPayload
  { userId :: Int
  , userName :: Text
  , userEmail :: Text }

instance FromJSON Event where
  parseJSON = withObject "Event" \obj -> do
    eventType <- obj .: "event_type"
    case eventType of
      "login" -> Login <$> (obj .: "data")
      "signup" -> Signup <$> (obj .: "signup")
      _ -> fail $ "unknown event_type: " <> eventType

instance FromJSON LoginPayload where { ... }
instance FromJSON SignupPayload where { ... }

handleEvent :: JSON.Value -> IO ()
handleEvent payload = case fromJSON payload of
  Success (Login LoginPayload { userId }) -> {- ... -}
  Success (Signup SignupPayload { userName, userEmail }) ->
    sendEmail userEmail $ "Welcome to Blockchain Emporium, " <> userName <> "!"
  Error message -> fail $ "could not parse event: " <> message
```

It’s definitely more boilerplate, but some extra overhead for type definitions is to be expected (and is greatly exaggerated in such tiny examples), and the arguments we’re discussing aren’t about boilerplate, anyway. The *real* problem with this version of the code, according to the Reddit comment from earlier, is that the Haskell code has to be updated whenever a service adds a new event type! A new case has to be added to the `Event` datatype, and it must be given new parsing logic. And what about when new fields get added to the payload? What a maintenance nightmare.

In comparison, the JavaScript code is much more permissive. If a new event type is added, it will just fall through the `switch` and do nothing. If extra fields are added to the payload, the JavaScript code will just ignore them. Seems like a win for dynamic typing.

Except that no, it isn’t. The only reason the statically typed program fails if we don’t update the `Event` type is that we wrote `handleEvent` that way. We could just have easily done the same thing in the JavaScript code, adding a default case that rejects unknown event types:

```js
const handleEvent = ({ event_type, data }) => {
  switch (event_type) {
    /* ... */
    default:
      throw new Error(`unknown event_type: ${event_type}`)
  }
}
```

We didn’t do that, since in this case it would clearly be silly. If a service receives an event it doesn’t know about, it should just ignore it. This is a case where being permissive is clearly the correct behavior, and we can easily implement that in the Haskell code too:

```haskell
handleEvent :: JSON.Value -> IO ()
handleEvent payload = case fromJSON payload of
  {- ... -}
  Error _ -> pure ()
```

This is still in the spirit of “parse, don’t validate” because we’re still parsing the values we *do* care about as early as possible, so we don’t fall into the double-validation trap. At no point do we take a code path that depends on a value being well-formed without first ensuring (with the help of the type system) that it is, in fact, actually well-formed. We don’t have to respond to an ill-formed value by raising an error! We just have to be explicit about ignoring it.

This illustrates an important point: the `Event` type in this Haskell code doesn’t describe “all possible events,” it describes all the events that the application cares about. Likewise, the code that parses those events’ payloads only worries about the fields the application needs, and it ignores extraneous ones. A static type system doesn’t require you eagerly write a schema for the whole universe, it simply requires you to be up front about the things you need.

This turns out to have a lot of pleasant benefits even though knowledge about inputs is limited:

  - It’s easy to discover the assumptions of the Haskell program just by looking at the type definitions. We know, for example, that this application doesn’t care about the `timestamp` field, since it never appears in any of the payload types. In the dynamically-typed program, we’d have to audit every code path to see whether or not it inspects that field, which would be a lot of error-prone work!

  - What’s more, it turns out the Haskell code doesn’t actually *use* the `userId` field inside the `SignupPayload` type, so that type is overly conservative. If we want to ensure it isn’t actually needed (since, for example, maybe we’re phasing out providing the user ID in that payload entirely), we need only delete that record field; if the code typechecks, we can be confident it really doesn’t depend on that field.

  - Finally, we neatly avoid all the gotchas related to shotgun parsing [mentioned in the previous blog post][parse-dont-validate:shotgun], since we still haven’t compromised on any of those principles.

We’ve already invalidated the first half of the claim: that statically typed languages can’t deal with data where the structure isn’t completely known. Let’s now look at the other half, which states that dynamically typed languages can process data where the structure isn’t known at all. Maybe that still sounds right, but if you slow down and think about it more carefully, you’ll find it can’t be.

The above JavaScript code makes all the same assumptions our Haskell code does: it assumes event payloads are JSON objects with an `event_type` field, and it assumes `signup` payloads include `data.user.name` and `data.user.email` fields. It certainly can’t do anything useful with truly unknown input! If a new event payload is added, our JavaScript code can’t magically adapt to handle it simply because it is dynamically typed. Dynamic typing just means the types of values are carried alongside them at runtime and checked as the program executes; the types are still there, and this program still implicitly relies on them being particular things.

# Keeping opaque data opaque

In the previous section, we debunked the idea that statically typed systems can’t process partially-known data, but if you have been paying close attention, you may have noticed it did not fully refute the original claim.

Although we were able to handle unknown data, we always simply discarded it, which would not fly if we were trying to implement some sort of proxying. For example, suppose we have a forwarding service that broadcasts events over a public network, attaching a signature to each payload to ensure it can’t be spoofed. We might implement this in JavaScript this way:

```js
const handleEvent = (payload) => {
  const signedPayload = { ...payload, signature: signature(payload) }
  retransmitEvent(signedPayload)
}
```

In this case, we don’t care about the structure of the payload at all (the `signature` function just works on any valid JSON object), but we still have to preserve all the information. How could we do that in a statically typed language, since a statically-typed language would have to assign the payload a precise type?

Once again, the answer involves rejecting the premise: there’s no need to give data a type that’s any more precise than the application needs. The same logic could be written in a straightforward way in Haskell:

```haskell
handleEvent :: JSON.Value -> IO ()
handleEvent (Object payload) = do
  let signedPayload = Map.insert "signature" (signature payload) payload
  retransmitEvent signedPayload
handleEvent payload = fail $ "event payload was not an object " <> show payload
```

In this case, since we don’t care about the structure of the payload, we manipulate a value of type `JSON.Value` directly. This type is extremely imprecise compared to our `Event` type from earlier—it can hold any legal JSON value, of any shape—but in this case, we *want* it to be imprecise.

Thanks to that imprecision, the type system helped us here: it caught the fact that we’re assuming the payload is a JSON object, not some other JSON value, and it made us handle the non-object cases explicitly. In this case we chose to raise an error, but of course, as before, you could choose some other form of recovery if you wanted to. You just have to be explicit about it.

Once more, note that the assumption we were forced to make explicit in Haskell is *also* made by the JavaScript code! If our JavaScript `handleEvent` function were called with a string rather than an object, it’s unlikely the behavior would be desirable, since an object spread on a string results in the following surprise:

```js
> { ..."payload", signature: "sig" }
{0: "p", 1: "a", 2: "y", 3: "l", 4: "o", 5: "a", 6: "d", signature: "sig"}
```

Oops. Once again, the parsing style of programming has helped us out, since if we didn’t “parse” the JSON value into an object by matching on the `Object` case explicitly, our code would not compile, and if we left off the fallthrough case, we’d get a warning about inexhaustive patterns.

---

Let’s look at one more example of this phenomenon before moving on. Suppose we’re consuming an API that returns user IDs, and suppose those IDs happen to be UUIDs. A straightforward interpretation of “parse, don’t validate” might suggest we represent user IDs in our Haskell API client using a `UUID` type:

```haskell
type UserId = UUID
```

However, our Reddit commenter would likely take umbrage with this! Unless the API contract explicitly states that all user IDs will be UUIDs, this representation is overstepping our bounds. Although user IDs might be UUIDs today, perhaps they won’t be tomorrow, and then our code would break for no reason! Is this the fault of static type systems?

Again, the answer is no. This is a case of improper data modeling, but the static type system is not at fault—it has simply been misused. The appropriate way to represent a `UserId` is to define a new, opaque type:

```haskell
newtype UserId = UserId Text
  deriving (Eq, FromJSON, ToJSON)
```

Unlike the type alias defined above which simply creates a new name for the existing `UUID` type, this declaration creates a totally new `UserId` type that is distinct from all other types, including `Text`. If we keep the datatype’s constructor private (that is, we don’t export it from the module that defines this type), then the *only* way to produce a `UserId` will be to go through its `FromJSON` parser. Dually, the only things you can do with a `UserId` are compare it with other `UserId`s for equality or serialize it using the `ToJSON` instance. Nothing else is permitted: the type system will prevent you from depending on the remote service’s internal representation of user IDs.

This illustrates another way that static type systems can provide strong, useful guarantees when manipulating completely opaque data. The runtime representation of a `UserId` is really just a string, but the type system does not allow you to accidentally use it like it’s a string, nor does it allow you to forge a new `UserId` out of thin air from an arbitrary string.[^1]

The type system is not a ball and chain forcing you to describe the representation of every value that enters and leaves your program in exquisite detail. Rather, it’s a tool that you can use in whatever way best suits your needs.

# Reflection is not special

We’ve now thoroughly debunked the claims made by the first commenter, but the question posed by the second commenter may still seem like a loophole in our logic. What *is* the type of Python’s `pickle.load()`? For those unfamiliar, [Python’s cutely-named `pickle` library][python:pickle] allows serializing and deserializing entire Python object graphs. Any object can be serialized and stored in a file using `pickle.dump()`, and it can be deserialized at a later point in time using `pickle.load()`.

What makes this appear challenging to our static type system is that the type of value produced by `pickle.load()` is difficult to predict—it depends entirely on whatever happened to be written to that file using `pickle.dump()`. This seems inherently dynamic, since we cannot possibly know what type of value it will produce at compile-time. At first blush, this is something a dynamically typed system can pull off, but a statically-typed one just can’t.

However, it turns out this situation is actually identical to the previous examples using JSON, and the fact that Python’s pickling serializes native Python objects directly does not change things. Why? Well, consider what happens *after* a program calls `pickle.load()`. Say you write the following function:

```python
def load_value(f):
  val = pickle.load(f)
  # do something with `val`
```

The trouble is that `val` can now be of *any* type, and just as you can’t do anything useful with truly unknown, unstructured input, you can’t do anything with a value unless you know at least something about it. If you call any method or access any field on the result, then you’ve already made an assumption about what sort of thing `pickle.load(f)` returned—and it turns out those assumptions *are* `val`’s type!

For example, imagine the only thing you do with `val` is call the `val.foo()` method and return its result, which is expected to be a string. If we were writing Java, then the expected type of `val` would be quite straightforward—we’d expect it to be an instance of the following interface:

```java
interface Foo extends Serializable {
  String foo();
}
```

And indeed, it turns out a `pickle.load()`-like function can be given a perfectly reasonable type in Java:

```java
static <T extends Serializable> Optional<T> load(InputStream in, Class<? extends T> cls);
```

Nitpickers will complain that this isn’t the same as `pickle.load()`, since you have to pass a `Class<T>` token to choose what type of thing you want ahead of time. However, nothing is stopping you from passing `Serializable.class` and branching on the type later, after the object has been loaded. And that’s the key point: the instant you do *anything* with the object, you must know something about its type, even in a dynamically typed language! The statically-typed language just forces you to be more explicit about it, just as it did when we were talking about JSON payloads.

---

Can we do this in Haskell, too? Absolutely—we can use [the `serialise` library][hackage:serialise], which has a similar API to the Java one mentioned above. It also happens to have a very similar interface to [the Haskell JSON library, aeson][hackage:aeson], as it turns out the problem of dealing with unknown JSON data is not terribly different from dealing with an unknown Haskell value—at some point, you have to do a little bit of parsing to do anything with the value.

That said, while you *can* emulate the dynamic typing of `pickle.load()` if you really want to by deferring the type check until the last possible moment, the reality is that doing so is almost never actually useful. At some point, you have to make assumptions about the structure of the value in order to use it, and you know what those assumptions are because *you wrote the code*. While there are extremely rare exceptions to this that require true dynamic code loading (such as, say, implementing a REPL for your programming language), they do not occur in day-to-day programming, and programmers in statically-typed languages are perfectly happy to supply their assumptions up front.

This is one of the fundamental disconnects between the static typing camp and the dynamic typing camp. Programmers working in statically-typed languages are perplexed when a programmer suggests they can do something in a dynamically typed language that a statically-typed language “fundamentally” prevents, since a programmer in a statically-typed language may reply the value has simply not been given a sufficiently precise type. From the perspective of a programmer working in a dynamically-typed language, the type system restricts the space of legal behaviors, but from the perspective of a programmer working in a statically-typed language, the set of legal behaviors *is* a value’s type.

Neither of these perspectives are actually inaccurate, from the appropriate point of view. Static type systems *do* impose restrictions on program structure, as it is provably impossible to reject *all* bad programs in a Turing-complete language without also rejecting some good ones (this is [Rice’s theorem][rices-theorem]). But it is simultaneously true that the impossibility of solving the general problem does not preclude solving a slightly more restricted version of the problem in a useful way, and a lot of the so-called “fundamental” inabilities of static type systems are not fundamental at all.

# Appendix: the reality behind the myths

The key thesis of this blog post has now been delivered: static type systems are not fundamentally worse than dynamic type systems at processing data with an open or partially-known structure. The sorts of claims made in the comments cited at the beginning of this blog post are not accurate depictions of what statically-typed program construction is like, and they misunderstand the limitations of static typing disciplines while exaggerating the capabilities of dynamically typed disciplines.

However, although greatly exaggerated, these myths do have some basis in reality. They appear to have developed at least in part from a misunderstanding about the differences between structural and nominal typing. This difference is unfortunately too big to address in this blog post, as it could likely fill several blog posts of its own. About six months ago I attempted to write a blog post on the subject, but I didn’t think it came out very compelling, so I scrapped it. Maybe someday I’ll find a better way to communicate the ideas.

Although I can’t give it the full treatment it deserves right now, I’d still like to touch on the idea briefly so that interested readers may be able to find other resources on the subject should they wish to do so. The key idea is that many dynamically typed languages idiomatically reuse simple data structures like hashmaps to represent what in statically-typed languages are often represented by bespoke datatypes (usually defined as classes or structs).

These two styles facilitate very different flavors of programming. A JavaScript or Clojure program may represent a record as a hashmap from string or symbol keys to values, written using object or hash literals and manipulated using ordinary functions from the standard library that manipulate keys and values in a generic way. This makes it straightforward to take two records and union their fields or to take an arbitrary (or even dynamic) subselection of fields from an existing record.

In contrast, most static type systems do not allow such free-form manipulation of records because records are not maps at all but unique types distinct from all other types. These types are uniquely identified by their (fully-qualified) name, hence the term *nominal typing*. If you wish to take a subselection of a struct’s fields, you must define an entirely new struct; doing this often creates an explosion of awkward boilerplate.

This is one of the main ideas that Rich Hickey has discussed in many of his talks that criticize static typing. He has advanced the idea that this ability to fluidly merge, separate, and transform records makes dynamic typing particularly suited to the domain of distributed, open systems. Unfortunately, this rhetoric has two significant flaws:

  1. It skirts too close to calling this a fundamental limitation of type systems, suggesting that it is not simply inconvenient but *impossible* to model such systems in a nominal, static type system. Not only is this not true (as this blog post has demonstrated), it misdirects people away from the point of his that actually has value: the practical, pragmatic advantage of a more structural approach to data modeling.

  2. It confuses the structural/nominal distinction with the dynamic/static distinction, incorrectly creating the impression that the fluid merging and splitting of records represented as key-value maps is only possible in a dynamically typed language. In fact, not only can statically-typed languages support structural typing, many dynamically-typed languages also support nominal typing. These axes have historically loosely correlated, but they are theoretically orthogonal.

For counterexamples to these claims, consider Python classes, which are quite nominal despite being dynamic, and TypeScript interfaces, which are structural despite being static. Indeed, modern statically-typed languages are increasingly acquiring native support for structurally-typed records. In these systems, record types work much like hashes in Clojure—they are not distinct, named types but rather anonymous collections of key-value pairs—and they support many of the same expressive manipulation operations that Clojure’s hashes do, all within a statically-typed framework.

If you are interested in exploring static type systems with strong support for structural typing, I would recommend taking a look at any of TypeScript, Flow, PureScript, Elm, OCaml, or Reason, all of which have some sort of support for structurally typed records. What I would *not* recommend for this purpose is Haskell, which has abysmal support for structural typing; Haskell is (for various reasons outside the scope of this blog post) aggressively nominal.[^2]

Does this mean Haskell is bad, or that it cannot be practically used to solve these kinds of problems? No, certainly not; there are many ways to model these problems in Haskell that work well enough, though some of them suffer from significant boilerplate. The core thesis of this blog post applies just as much to Haskell as it does to any of the other languages I mentioned above. However, I would be remiss not to mention this distinction, as it may give programmers from a dynamically-typed background who have historically found statically-typed languages much more frustrating to work with a better understanding of the *real* reason they feel that way. (Essentially all mainstream, statically-typed OOP languages are even more nominal than Haskell!)

As closing thoughts: this blog post is not intended to start a flame war, nor is it intended to be an assault on dynamically typed programming. There are many patterns in dynamically-typed languages that are genuinely difficult to translate into a statically-typed context, and I think discussions of those patterns can be productive. The purpose of this blog post is to clarify why one particular discussion is *not* productive, so please: stop making these arguments. There are much more productive conversations to have about typing than this.


[^1]: Technically, you could abuse the `FromJSON` instance to convert an arbitrary string to a `UserId`, but this would not be as easy as it sounds, since `fromJSON` can fail. This means you’d somehow have to handle that failure case, so this trick would be unlikely to get you very far unless you’re already in a context where you’re doing input parsing… at which point it would be easier to just do the right thing. So yes, the type system doesn’t prevent you from going out of your way to shoot yourself in the foot, but it guides you towards the right solution (and there is no safeguard in existence that can completely protect a programmer from making their own life miserable if they are determined to do so).

[^2]: I consider this to be Haskell’s most significant flaw at the time of this writing.


[parse-dont-validate]: /blog/2019/11/05/parse-don-t-validate/
[parse-dont-validate:shotgun]: /blog/2019/11/05/parse-don-t-validate/#the-danger-of-validation
[edn]: https://github.com/edn-format/edn
[python:pickle]: https://docs.python.org/3/library/pickle.html
[hackage:serialise]: https://hackage.haskell.org/package/serialise
[hackage:aeson]: https://hackage.haskell.org/package/aeson
[rices-theorem]: https://en.wikipedia.org/wiki/Rice's_theorem

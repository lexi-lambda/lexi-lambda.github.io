    Title: Defeating Racket’s separate compilation guarantee
    Date: 2019-04-21T05:30:00
    Tags: racket, macros

Being a self-described [programming-language programming language][manifesto-pl-pl] is an ambitious goal. To preserve predictability while permitting linguistic extension, Racket comes equipped with a module system carefully designed to accommodate [composable and compilable macros][macromod]. One of the module system’s foundational properties is its [*separate compilation guarantee*][separate-compilation-guarantee], which imposes strong, unbreakable limits on the extent of compile-time side-effects. It is *essential* for preserving static guarantees in a world where compiling a module can execute arbitrary code, and despite numerous unsafe trapdoors that have crept into Racket since its birth as PLT Scheme, none have ever given the programmer the ability to cheat it.

Yet today, in this blog post, we’re going to do exactly that.

# What is the separate compilation guarantee?

Before we get to the fun part (i.e. breaking things), let’s go over some fundamentals so we understand what we’re breaking. The authoritative source for the separate compilation guarantee is [the Racket reference][separate-compilation-guarantee], but it is dense, as authoritative sources tend to be. Although I enjoy reading technical manuals for sport, it is my understanding that not all the people who read this blog are as strange as I am, so let’s start with a quick primer, instead. (If you’re already an expert, feel free to [skip to the next section](#section:main-start).)

Racket is a macro-enabled programming language. In Racket, a macro is a user-defined, code-to-code transformation that occurs at compile-time. These transformations cannot make arbitrary changes to the program—in Racket, they are usually required to be *local*, affecting a single expression or definition at a time—but they may be implemented using arbitrary code. This means that a macro can, if it so desires, read the SSH keys off your filesystem and issue an HTTP request to send them someplace.

That kind of attack is bad, admittedly, but it’s also *uninteresting*: Racket allows you do all that and then some, making no attempt to prevent it.[^1] Racket calls these “external effects,” things that affect state outside of the programming language. They sound scary, but in practice, *internal effects*—effects that mutate state inside the programming language—are a much bigger obstacle to practical programming. Let’s take a look at why.

Let’s say we have a module with some global, mutable state. Perhaps it is used to keep track of a set of delicious foods:

```racket
;; foods.rkt
#lang racket
(provide delicious-food? add-delicious-food!)

(define delicious-foods (mutable-set))

(define (delicious-food? food)
  (set-member? delicious-foods food))

(define (add-delicious-food! new-food)
  (set-add! delicious-foods new-food))
```

Using this interface, let’s write a program that checks if a particular food, given as a command-line argument, is delicious:

```racket
;; check-food.rkt
#lang racket
(require "foods.rkt")

(add-delicious-food! "pineapple")
(add-delicious-food! "sushi")
(add-delicious-food! "cheesecake")

(command-line
  #:args [food-to-check]
  (if (delicious-food? food-to-check)
      (printf "~a is a delicious food.\n" food-to-check)
      (printf "~a is not delicious.\n" food-to-check)))
```
```sh
$ racket check-food.rkt cheesecake
cheesecake is a delicious food.
$ racket check-food.rkt licorice
licorice is not delicious.
```

Exhilarating. (Sorry, licorice fans.) But what if a *macro* were to call `add-delicious-food!`? What would happen? For example, what if we wrote a macro to add a lot of foods at once?[^2]

```racket
(require syntax/parse/define)
(define-simple-macro (add-food-combinations! [fst:string ...]
                                             [snd:string ...])
  #:do [(for* ([fst-str (in-list (syntax->datum #'[fst ...]))]
               [snd-str (in-list (syntax->datum #'[snd ...]))])
          (add-delicious-food! (string-append fst-str " " snd-str)))]
  (void))

; should add “fried chicken,” “roasted chicken”, “fried potato,” and “roasted potato”
(add-food-combinations! ["fried" "roasted"] ["chicken" "potato"])
```

Now, what do you think executing `racket check-food.rkt 'fried chicken'` will do?

Clearly, the program should print `fried chicken is a delicious food`, and indeed, many traditional Lisp systems would happily produce such a result. After all, running `racket check-food.rkt 'fried chicken'` must load the source code inside `check-food.rkt`, expand and compile it, then run the result. While the program is being expanded, the compile-time calls to `add-delicious-food!` should add new elements to the `delicious-food` set, so when the program is executed, the string `"fried chicken"` ought to be in it.

But if you actually try this yourself, you will find that *isn’t* what happens. Instead, Racket rejects the program:

```sh
$ racket check-food.rkt 'fried chicken'
check-food.rkt:12:11: add-delicious-food!: reference to an unbound identifier
  at phase: 1; the transformer environment
  in: add-delicious-food!
```

Why does Racket reject this program? Well, consider that Racket allows programs to be pre-compiled using `raco make`, doing all the work of macroexpansion and compilation to bytecode ahead of time. Subsequent runs of the program will use the pre-compiled version, without having to run all the macros again. This is a problem, since expanding the `add-food-combinations!` macro had side-effects that our program depended on!

If Racket allowed the above program, it might do different things depending on whether it was pre-compiled. Running directly from source code might treat `'fried chicken'` as a delicious food, while running from pre-compiled bytecode might not. Racket considers this unacceptable, so it disallows the program entirely.

## Preserving separate compilation via phases

Hopefully, you are now mostly convinced that the above program is a bad one, but you might have some lingering doubts. You might, for example, wonder if Racket disallows mutable compile-time state entirely. That is not the case—Racket really does allow everything that happens at runtime to happen at compile-time—but it does prevent compile-time and run-time state from ever *interacting*. Racket stratifies every program into a compile-time part and a run-time part, and it restricts communication between them to limited, well-defined channels (mainly via expanding to code that does something at run-time).

Racket calls this system of stratification *phases*. Code that executes at run-time belongs to the *run-time phase*, while code that executes at compile-time (i.e. macros) belongs to the *compile-time phase*. When a variable is defined, it is always defined in a particular phase, so bindings declared with `define` can only be used at run-time, while bindings declared with `define-for-syntax` can only be used at compile-time. Since `add-delicious-food!` was declared using `define`, it was not allowed (and in fact was not even visible) in the body of the `add-food-combinations!` macro.

While the whole macro system could work precisely as just described, such a strict stratification would be incredibly rigid. Since every definition would belong to either run-time or compile-time, but never both, reusing run-time code to implement macros would be impossible. While the example in the previous section might make it seem like that’s a good thing, it very often isn’t: imagine if general-purpose functions like `map` and `filter` all needed to be written twice!

To avoid this problem, Racket allows modules to be imported at both run-time and compile-time, so long as it’s done explicitly. Writing `(require "some-library.rkt")` requires `some-library.rkt` for run-time code, but writing `(require (for-syntax "some-library.rkt"))` requires it for compile-time code. Requiring a module `for-syntax` is sort of like implicitly adjusting all of its uses of `define` to be `define-for-syntax`, instead, effectively shifting all the code from run-time to compile-time. This kind of operation is therefore known as *phase shifting* in Racket terminology.

We can use phase shifting to make the program we wrote compile. If we adjust the `require` at the beginning of our program, then we can ensure `add-delicious-food!` is visible to both the run-time and compile-time parts of `check-food.rkt`:

```racket
(require "foods.rkt" (for-syntax "foods.rkt"))
```

Now our program compiles. However, if you’ve been following everything carefully, you should be wondering why! According to the last section, sharing state between run-time and compile-time fundamentally can’t work without introducing inconsistencies between uncompiled and pre-compiled code. And that’s true—such a thing would cause all sorts of problems, and Racket doesn’t allow it. If you run the program, whether pre-compiled or not, you’ll find it always does the same thing:

```sh
$ racket check-food.rkt 'fried chicken'
fried chicken is not delicious.
```

This seems rather confusing. What happened to the calls to `add-delicious-food!` inside our `add-food-combinations!` macro? If we stick a `printf` inside `add-delicious-food!`, we’ll find that it really does get called:

```racket
(define (add-delicious-food! new-food)
  (printf "Registering ~a as a delicious food.\n" new-food)
  (set-add! delicious-foods new-food))
```
```sh
$ racket check-food.rkt 'fried chicken'
Registering fried chicken as a delicious food.
Registering fried potato as a delicious food.
Registering roasted chicken as a delicious food.
Registering roasted potato as a delicious food.
Registering pineapple as a delicious food.
Registering sushi as a delicious food.
Registering cheesecake as a delicious food.
fried chicken is not delicious.
```

And in fact, if we pre-compile `check-food.rkt`, we’ll see that the first four registrations appear at compile-time, exactly as we expect:

```sh
$ raco make check-food.rkt
Registering fried chicken as a delicious food.
Registering fried potato as a delicious food.
Registering roasted chicken as a delicious food.
Registering roasted potato as a delicious food.
$ racket check-food.rkt 'fried chicken'
Registering pineapple as a delicious food.
Registering sushi as a delicious food.
Registering cheesecake as a delicious food.
fried chicken is not delicious.
```

The compile-time registrations really are happening, but Racket is automatically restricting the compile-time side-effects so they only apply at compile-time. After compilation has finished, Racket ensures that compile-time side effects are thrown away, and the run-time code starts over with fresh, untouched state. This guarantees consistent behavior, since it becomes impossible to distinguish at run-time whether a module was just compiled on the fly, or if it was pre-compiled long ago (possibly even on someone else’s computer).

This is the essence of the separate compilation guarantee. To summarize:

  - Run-time and compile-time are distinct *phases* of execution, which cannot interact.

  - Modules can be required at multiple phases via *phase shifting*, but their state is kept separate. Each phase gets its own copy of the state.

  - Ensuring that the state is kept separate ensures predictable program behavior, no matter when the program is compiled.

This summary is a simplification of phases in Racket. The full Racket module system does not have only two phases, since macros can also be *used* at compile-time to implement other macros, effectively creating a separate “compile-time” for the compile-time code. Each compile-time pass is isolated to its own phase, creating a finite but arbitrarily large number of distinct program phases (all but one of which occur at compile-time).

Furthermore, the separate compilation guarantee does not just isolate the state of each phase from the state of other phases but also isolates all compile-time state from the compile-time state of other modules. This ensures that compilation is still deterministic even if modules are compiled in a different *order*, or if several modules are sometimes compiled individually while other times compiled together all at once.

If you want to learn more, the full details of the module system are described at length in the [General Phase Levels][guide-general-phase-levels] section of the Racket Guide, but the abridged summary I’ve described is enough for the purposes of this blog post. If the bulleted list above mostly made sense to you, you’re ready to move on.

<h2 id="section:main-start">How we’re going to break it</h2>

The separate compilation guarantee is a sturdy opponent, but it is not without weaknesses. Although no API in Racket, safe or unsafe, allows arbitrarily disabling phase separation, a couple features of Racket are already known to allow limited forms of cross-phase communication.

The most significant of these, and the one we’ll be using as our vector of attack, is the logger. Unlike many logging systems, which are exclusively string-oriented, Racket’s logging interface allows structured logging by associating an arbitrary Racket value with each and every log message. Since it is possible to set up listeners within Racket that receive log messages sent to a particular “topic,” the logger can be used as a communication channel to send values between different parts of a program.

The following program illustrates how this works. One thread creates a listener for all log messages on the topic `'send-me-a-value` using `make-log-receiver`, then uses `sync` to block until a value is received. Meanwhile, a second thread sends values through the logger using `log-message`. Together, this creates a makeshift buffered, asynchronous channel:

```racket
;; log-comm.rkt
#lang racket

(define t1
  (thread
   (lambda ()
     (define recv (make-log-receiver (current-logger) 'debug 'send-me-a-value))
     (let loop ()
       (println (sync recv))
       (loop)))))

(define t2
  (thread
   (lambda ()
     (let loop ([n 0])
       (log-message (current-logger) 'debug 'send-me-a-value "" n #f)
       (sleep 1)
       (loop (add1 n))))))

(thread-wait t1) ; wait forever
```
```
$ racket log-comm.rkt
'#(debug "" 1 send-me-a-value)
'#(debug "" 2 send-me-a-value)
'#(debug "" 3 send-me-a-value)
'#(debug "" 4 send-me-a-value)
^Cuser break
```

In this program, the value being sent through the logger is just a number, which isn’t very interesting. But the value really can be *any* value, even arbitrary closures or mutable data structures. It’s even possible to send a [channel][guide-channels] through a logger, which can subsequently be used to communicate directly, without having to abuse the logger.

Generally, this feature of loggers isn’t very useful, since Racket has plenty of features for cross-thread communication. What’s special about the logger, however, is that it is global, and it is cross-phase.

The cross-phase nature of the logger makes some sense. If a Racket program creates a namespace (that is, a fresh environment for dynamic evaluation), then uses it to expand and compile a Racket module, the process of compilation might produce some log messages, and the calling thread might wish to receive them. It wouldn’t be a very useful logging system if log messages during compile-time were always lost. However, this convenience is a loophole in the phase separation system, since it allows values to flow—bidirectionally—between phases.

This concept forms the foundation of our exploit, but it alone is not a new technique, and I did not discover it. However, all existing uses I know of that use the logger for cross-phase communication require control of the parent namespace in which modules are being compiled, which means some code must exist “outside” the actual program. That technique does not work for ordinary programs run directly with `racket` or compiled directly with `raco make`, so to get there, we’ll need something more clever.

## The challenge

Our goal, therefore, is to share state between phases *without* controlling the compilation namespace. More precisely, we want to be able to create an arbitrary module-level definition that is *cross-phase persistent*, which means it will be evaluated once and only once no matter how many times its enclosing module is re-instantiated (i.e. given a fresh, untouched state) at various phases. A phase-shifted `require` of the module that contains the definition should share state with an unshifted version of the module, breaking the separate compilation guarantee wide open.

To use the example from the previous section, we should be able to adjust `foods.rkt` very slightly…

```racket
;; foods.rkt
#lang racket
(require "define-cross-phase.rkt")
(provide delicious-food? add-delicious-food!)

; share across phases
(define/cross-phase delicious-foods (mutable-set))

#| ... |#
```

…and the `delicious-foods` mutable state should magically become cross-phase persistent. When running `check-food.rkt` from source, we should see the side-effects persisted from the module’s compilation, while running from pre-compiled bytecode should give us the result with compile-time effects discarded.

We already know the logger is going to be part of our exploit, but implementing `define/cross-phase` on top of it is more subtle than it might seem. In our previous example that used `make-log-receiver`, we had well-defined sender and receiver threads, but who is the “sender” in our multi-phased world? And what exactly is the sender sending?

To answer those questions, allow me to outline the general idea of our approach:

  1. The first time our `foods.rkt` module is instantiated, at any phase, it evaluates the `(mutable-set)` expression to produce a new mutable set. It spawns a sender thread that sends this value via the logger to anyone who will listen, and that thread lingers in the background for the remaining duration of the program.

  2. All subsequent instantiations of `foods.rkt` do *not* evaluate the `(mutable-set)` expression. Instead, they obtain the existing set by creating a log receiver and obtaining the value the sender thread is broadcasting. This ensures that a single value is shared across all instantiations of the module.

This sounds deceptively simple, but the crux of the problem is how to determine whether `foods.rkt` has previously been instantiated or not. Since we can only communicate across phases via the logger, we cannot use any shared state to directly record the first time the module is instantiated. We can listen to a log receiver and wait to see if we get a response, but this introduces a race condition: how long do we wait until giving up and deciding we’re the first instantiation? Worse, what if two threads instantiate the module at the same time, and both threads end up spawning a new sender thread, duplicating the state?

The true challenge, therefore, is to develop a protocol by which we can be *certain* we are the first instantiation of a module, without relying on any unspecified behavior, and without introducing any race conditions. This is possible, but it isn’t obvious, and it requires combining loggers with some extra tools available to the Racket programmer.

## The key idea

It’s finally time to tackle the key idea at the heart of our exploit: garbage collection. In Racket, garbage collection is an observable effect, since Racket allows attaching finalizers to values via [wills and executors][reference-wills-and-executors]. Since a single heap is necessarily shared by the entire VM, behavior happening on other threads (even in other phases) can be indirectly observed by creating a unique value—a “canary”—then sending it to another thread, and waiting to see if it will be garbage collected or not (that is, whether or not the canary “dies”).

Remember that logs and log receivers are effectively buffered, multicast, asynchronous FIFO channels. Since they are buffered, if any thread is already listening to a logger topic when a value is sent, it cannot possibly be garbage collected until that thread either reads it and discards it or the receiver itself is garbage collected. It’s possible to use this mechanism to observe whether or not another thread is already listening on a topic, as the following program demonstrates:[^3]

```racket
;; check-receivers.rkt
#lang racket

(define (check-receivers topic)
  (define executor (make-will-executor))
  ; limit scope of `canary` so we don’t retain a reference
  (let ()
    (define canary (gensym 'canary))
    (will-register executor canary void)
    (log-message (current-logger) 'debug topic "" canary #f))
  (if (begin
        (collect-garbage)
        (collect-garbage)
        (collect-garbage)
        (sync/timeout 0 executor))
      (printf "no receivers for ~v\n" topic)
      (printf "receiver exists for ~v\n" topic)))

; add a receiver on topic 'foo
(define recv (make-log-receiver (current-logger) 'debug 'foo))

(define t1 (thread (λ () (check-receivers 'foo))))
(define t2 (thread (λ () (check-receivers 'bar))))

(thread-wait t1)
(thread-wait t2)
```
```
$ racket check-receivers.rkt
no receivers for 'bar
receiver exists for 'foo
```

However, this program has some problems. For one, it needs to call `collect-garbage` several times to be certain that the canary will be collected if there are no listeners, which can take a second or two, and it also assumes that three calls to `collect-garbage` will be enough to collect the canary, though there is no guarantee that will be true.

A bulletproof solution should be both reasonably performant and guaranteed to work. To get there, we have to combine this idea with something more. Here’s the trick: instead of sending the canary alone, send a [channel][guide-channels] alongside it. Synchronize on both the canary’s executor *and* the channel so that the thread will unblock if either the canary is collected *or* the channel is received and sent a value using `channel-put`. Have the receiver listen for the channel on a separate thread, and when it receives it, send a value back to unblock the waiting thread as quickly as possible, without needing to rely on a timeout or a particular number of calls to `collect-garbage`.

Using that idea, we can revise the program:

```racket
;; check-receivers.rkt
#lang racket

(define (check-receivers topic)
  (define chan (make-channel))
  (define executor (make-will-executor))
  ; limit scope of `canary` so we don’t retain a reference
  (let ()
    (define canary (gensym 'canary))
    (will-register executor canary void)
    (log-message (current-logger) 'debug topic ""
                 ; send the channel + the canary
                 (vector-immutable chan canary) #f))
  (if (let loop ([n 0])
        (sleep) ; yield to try to let the receiver thread work
        (match (sync/timeout 0
                             (wrap-evt chan (λ (v) 'received))
                             (wrap-evt executor (λ (v) 'collected)))
          ['collected #t]
          ['received  #f]
          [_ ; collect garbage and try again
           (collect-garbage (if (< n 3) 'minor 'major))
           (loop (add1 n))]))
      (printf "no receivers for ~v\n" topic)
      (printf "receiver exists for ~v\n" topic)))

; add a receiver on topic 'foo
(define recv (make-log-receiver (current-logger) 'debug 'foo))
(void (thread
       (λ ()
         (let loop ()
           (match (sync recv)
             [(vector _ _ (vector chan _) _)
              (channel-put chan #t)
              (loop)])))))

(define t1 (thread (λ () (check-receivers 'foo))))
(define t2 (thread (λ () (check-receivers 'bar))))

(thread-wait t1)
(thread-wait t2)
```

Now the program completes almost instantly. For this simple program, the explicit `(sleep)` call is effective enough at yielding that, on my machine, `(check-receivers 'foo)` returns without ever calling `collect-garbage`, and `(check-receivers 'bar)` returns after performing a single minor collection.

This is extremely close to a bulletproof solution, but there are two remaining subtle issues:

  1. There is technically a race condition between the `(sync recv)` in the receiver thread and the subsequent `channel-put`, since it’s possible for the canary to be received, discarded, and garbage collected before reaching the call to `channel-put`, which the sending thread would incorrectly interpret as indicating the topic has no receivers.

     To fix that, the receiver thread can send the canary itself back through the channel, which fundamentally has to work, since the value cannot be collected until it has been received by the sending thread, at which point the `sync` has already chosen the channel.

  2. It is possible for the receiver thread to receive the log message and call `channel-put`, but for the sending thread to somehow die in the meantime (which cannot be protected against in general in Racket, since `thread-kill` immediately and forcibly terminates a thread). If this were to happen, the sending thread would never obtain the value from the channel, blocking the receiving thread indefinitely.

     A solution is to spawn a new thread for each `channel-put` instead of calling it directly from the receiving thread. Conveniently, this both ensures the receiving thread never gets stuck and avoids resource leaks, since the Racket runtime is smart enough to GC a thread blocked on a channel that has no other references and therefore can never be unblocked.

With those fixes in place, the program is, to the best of my knowledge, bulletproof. It will always correctly determine whether or not a logger has a listener, with no race conditions or reliance upon unspecified behavior of the Racket runtime. It does, however, make a couple of assumptions.

First, it assumes that the value of `(current-logger)` is shared between the threads. It is true that `(current-logger)` can be changed, and sometimes is, but it’s usually done via `parameterize`, not mutation of the parameter directly. Therefore, this can largely be mitigated by storing the value of `(current-logger)` at module instantiation time.

Second, it assumes that no other receivers are listening on the same topic. Technically, even using a unique, uninterned key for the topic is insufficient to ensure that no receivers are listening to it, since a receiver can choose to listen to all topics. However, in practice, it is highly unlikely that any receiver would willfully choose to listen to all topics at the `'debug` level, since the receiver would be inundated with enormous amounts of useless information. Even if such a receiver were to be created, it is highly likely that it would dequeue the messages as quickly as possible and discard the accompanying payload, since doing otherwise would cause all messages to be retained in memory, leading to a significant memory leak.

Both these problems can be mitigated by using a logger other than the root logger, which is easy in this example. However, for the purpose of subverting the separate compilation guarantee, we would have no way to share the logger object itself across phases, defeating the whole purpose, so we are forced to use the root logger and hope the above two assumptions remain true (but they usually do).

## Preparing the exploit

If you’ve made it here, congratulations! The most difficult part of this blog post is over. All that’s left is the fun part: performing the exploit.

The bulk of our implementation is a slightly adapted version of `check-receivers`:

```racket
;; define-cross-phase.rkt
#lang racket

(define root-logger (current-logger))

(define (make-cross-phase topic thunk)
  (define receiver (make-log-receiver root-logger 'debug topic))
  (define chan (make-channel))
  (define executor (make-will-executor))

  (let ()
    (define canary (gensym 'canary))
    (will-register executor canary (λ (v) 'collected))
    (log-message root-logger 'debug topic ""
                 (vector-immutable canary chan) #f)
    (let loop ()
      (match (sync receiver)
        [(vector _ _ (vector _ (== chan eq?)) _)
         (void)]
        [_
         (loop)])))

  (define execute-evt (wrap-evt executor will-execute))
  (define result (let loop ([n 0])
                   (sleep)
                   (or (sync/timeout 0 chan execute-evt)
                       (begin
                         (collect-garbage (if (< n 3) 'minor 'major))
                         (loop (add1 n))))))

  (match result
    [(vector _ value)
     value]
    ['collected
     (define value (thunk))
     (thread
      (λ ()
        (let loop ()
          (match (sync receiver)
            [(vector _ _ (vector canary chan) _)
             (thread (λ () (channel-put chan (vector-immutable canary value))))
             (loop)]))))
     value]))
```

There are a few minor differences, which I’ll list:

  1. The most obvious difference is that `make-cross-phase` does the work of both checking if a receiver exists—which I’ll call the *manager thread*—and spawning it if it doesn’t. If it does end up spawning a manager thread, it evaluates the given thunk to produce a value, which becomes the cross-phase value that will be sent through the channel alongside the canary.

  2. Once the manager thread is created, subsequent calls to `make-cross-phase` will receive the value through the channel and return it instead of re-invoking `thunk`. This is what ensures the right-hand side of each use of `define/cross-phase` is only ever evaluated once.

  3. Since `make-cross-phase` needs to create a log receiver if no manager thread exists, it does so immediately, before sending the canary through the logger. This avoids a race condition between multiple threads that are simultaneously competing to become the manager thread, where both threads could send a canary through the logger before either was listening, both canaries would get GC’d, and both threads would spawn a new manager.[^4]

     Creating the receiver before sending the canary avoids this problem, but the thread now needs to receive its own canary and discard it before synchronizing on the channel and executor, since otherwise it will retain a reference to the canary. It’s possible that in between creating the receiver and sending the canary, another thread also sent a canary, so it needs to drop any log messages it finds that don’t include its own canary.

     This ends up working out perfectly, since every thread drops all the messages received before the one containing its own canary, but retains all subsequent values. This means that only one thread can ever “win” and become the manager, since the first thread to send a canary is guaranteed to retain all subsequent canaries, yet also guaranteed its canary will be GC’d. Other threads racing to become the manager will remain blocked until the manager thread is created, since its canaries will be retained by the manager-to-be until it dequeues them.

     (This is the most subtle part of the process to get right, but conveniently, it mostly just works out without very much code. If you didn’t understand any of the above three paragraphs, it isn’t a big deal.)

The final piece to this puzzle is to define the `define/cross-phase` macro that wraps `make-cross-phase`. The macro is actually slightly more involved than just generating a call to `make-cross-phase` directly, since we’d like to use an uninterned symbol for the topic instead of an interned one, just to ensure it is unique. Ordinarily, this might seem impossible, since an uninterned symbol is fundamentally a unique value that needs to be communicated across phases, and the whole problem we are solving is creating a communication channel that spans phases. However, Racket actually provides some built-in support for sharing uninterned symbols across phases (plus some other kinds of values, but they must always be immutable). To do this, we need to generate a [cross-phase persistent submodule][reference-cross-phase-persistent-modules] that exports an uninterned symbol, then pass that symbol as the topic to `make-cross-phase`:

```racket
(require (for-syntax racket/syntax)
         syntax/parse/define)

(provide define/cross-phase)

(define-simple-macro (define/cross-phase x:id e:expr)
  #:with topic-mod-name (generate-temporary 'cross-phase-topic-key)
  (begin
    (module topic-mod-name '#%kernel
      (#%declare #:cross-phase-persistent)
      (#%provide topic)
      (define-values [topic] (gensym "cross-phase")))
    (require 'topic-mod-name)
    (define x (make-cross-phase topic (λ () e)))))
```

And that’s really it. We’re done.

## Executing the exploit

With our implementation of `define/cross-phase` in hand, all that’s left to do is run our original `check-foods.rkt` program and see what happens:

```sh
$ racket check-food.rkt 'fried chicken'
set-add!: contract violation:
expected: set?
given: (mutable-set "fried chicken" "roasted chicken" "roasted potato" "fried potato")
argument position: 1st
other arguments...:
  x: "pineapple"
```

Well, I don’t know what you expected. Play stupid games, win stupid prizes.

This error actually makes sense, but it belies one reason (of many) why this whole endeavor is probably a bad idea. Although we’ve managed to make our mutable set cross-phase persistent, our references to set operations like `set-add!` and `set-member?` are not, and every time `racket/set` is instantiated in a fresh phase, it creates an entirely new instance of the `set` structure type. This means that even though we have a bona fide mutable set, it isn’t actually the type of set that this phase’s `set-add!` understands!

Of course, this isn’t a problem that some liberal application of `define/cross-phase` can’t solve:

```racket
;; foods.rkt
#lang racket
(require "define-cross-phase.rkt")
(provide delicious-food? add-delicious-food!)

(define/cross-phase cross:set-member? set-member?)
(define/cross-phase cross:set-add! set-add!)

(define/cross-phase delicious-foods (mutable-set))

(define (delicious-food? food)
  (cross:set-member? delicious-foods food))

(define (add-delicious-food! new-food)
  (cross:set-add! delicious-foods new-food))
```
```sh
$ racket check-food.rkt 'fried chicken'
fried chicken is a delicious food.
$ raco make check-food.rkt
$ racket check-food.rkt 'fried chicken'
fried chicken is not delicious.
```

And thus we find that another so-called “guarantee” isn’t.

# Reflection

Now comes the time in the blog post when I have to step back and think about what I’ve done. Have mercy.

Everything in this blog post is a terrible idea. No, you should not use loggers for anything that isn’t logging, you shouldn’t use wills and executors for critical control flow, and obviously you should absolutely not intentionally break one of the most helpful guarantees the Racket module system affords you.

But I thought it was fun to do all that, anyway.

The meaningful takeaways from this blog post aren’t that the separate compilation guarantee can be broken, nor that any of the particular techniques I used hold, but that

  1. ensuring non-trivial guarantees is really hard,

  2. despite that, the separate compilation guarantee is really, really hard to break,

  3. the separate compilation guarantee is good, and you should appreciate the luxury it affords you while writing Racket macros,

  4. avoiding races in a concurrent environment can be extremely subtle,

  5. and Racket is totally *awesome* for giving me this much rope to hang myself with.

If you want to hang yourself with Racket, too, [runnable code from this blog post is available here][gist-defeating-separate-compilation].

[^1]: This isn’t *strictly* true, since Racket provides sandboxing mechanisms that can compile and execute untrusted code without file system or network access, but this is not the default compilation mode. Usually, it doesn’t matter nearly as much as it might sound: most of the time, if you’re compiling untrusted code, you’re also going to run it, and running untrusted code can do all those things, anyway.

[^2]: This is actually a *terrible* use case for a macro, since an ordinary function would do just fine, but I’m simplifying a little to keep the example small.

[^3]: Racket actually provides this functionality directly via the `log-level?` procedure. However, since `log-level?` provides no way to determine how *many* receivers are listening to a topic, using it to guard against creating a receiver is vulnerable to a race condition that the garbage collection-based approach can avoid, as is discussed later. Furthermore, the GC technique is more likely to be resilient to nosy log receivers listening on all topics at the `'debug` level, since they will almost certainly dequeue and discard the value quickly (as otherwise they would leak large quantities of memory).

[^4]: This race is the one that makes using `log-level?` untenable, since the receiver needs to be created before the topic is checked for listeners to avoid the race, which can’t be done with `log-level?` (since it would always return `#t`).

[doc-log-levelp]: https://docs.racket-lang.org/reference/logging.html#%28def._%28%28quote._~23~25kernel%29._log-level~3f%29%29
[gist-defeating-separate-compilation]: https://gist.github.com/lexi-lambda/f173a84fc9727977bcea657b3bb0cd4f
[guide-channels]: https://docs.racket-lang.org/guide/concurrency.html#%28part._.Channels%29
[guide-general-phase-levels]: https://docs.racket-lang.org/guide/phases.html
[macromod]: https://www.cs.utah.edu/plt/publications/macromod.pdf
[manifesto-pl-pl]: https://felleisen.org/matthias/manifesto/sec_pl-pl.html
[reference-cross-phase-persistent-modules]: https://docs.racket-lang.org/reference/eval-model.html#%28part._cross-phase._persistent-modules%29
[reference-wills-and-executors]: https://docs.racket-lang.org/reference/willexecutor.html
[separate-compilation-guarantee]: https://docs.racket-lang.org/reference/eval-model.html#%28part._separate-compilation%29

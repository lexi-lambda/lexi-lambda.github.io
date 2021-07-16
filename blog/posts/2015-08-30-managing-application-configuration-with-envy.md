    Title: Managing application configuration with Envy
    Date: 2015-08-30T16:05:37
    Tags: envy, racket, 12factor

Application configuration can be a pain. Modern web apps don't live on dedicated boxes, they run on VPSes somewhere in the amorphous "cloud", and keeping configuration out of your application's repository can seem like more trouble than it's worth. Fortunately, [The Twelve-Factor App][12factor] provides a set of standards for keeping web apps sane, and [one of those guidelines advises keeping configuration in the environment][12factor-config].

[Envy][envy] is the declarative bridge between Racket code and the outside world of the environment.

<!-- more -->

# Introducing Envy

I built Envy to distill the common tasks needed when working with environment variables into a single, declarative interface that eliminates boilerplate and makes it easy to see which environment variables an application depends on (instead of having them littered throughout the codebase). Using it is simple. Just require `envy` and you're good to go.

The best way to use Envy is to create a "manifest" module that declares all the environment variables your application might use. For example, the following module is a manifest that describes an application that uses three environment variables:

```racket
; environment.rkt
#lang typed/racket/base

(require envy)

(define/provide-environment
  api-token
  [log-level : Symbol #:default 'info]
  [parallel? : Boolean])
```

When this module is required, Envy will automatically do the following:

  1. Envy will check the values of three environment variables: `API_TOKEN`, `LOG_LEVEL`, and `PARALLEL`.

  2. If either `API_TOKEN` or `PARALLEL` is not set, an error will be raised:

        envy: The required environment variable "API_TOKEN" is not defined.

  3. The values for `LOG_LEVEL` and `PARALLEL` will be parsed to match their type annotations.

  4. If `LOG_LEVEL` is not set, it will use the default value, `'info`.

  5. The values will be stored in `api-token`, `log-level`, and `parallel?`, all of which will be provided by the enclosing module.

Now just `(require (prefix-in env: "environment.rkt"))`, and the environment variables are guaranteed to be available in your application's code.

# Working with Typed Racket

As you may have noticed by the example above, Envy is built with Typed Racket in mind. In fact, `define/provide-environment` will *only* work within a Typed Racket module, but that doesn't mean Envy can't be used with plain Racket—the manifest module can always be required by any kind of Racket module.

However, when using Typed Racket, Envy provides additional bonuses. Environment variables are inherently untyped—they're all just strings—but Envy assigns the proper type to each environment variable automatically, so no casting is necessary.

```
> parallel?
- : Boolean
#t
```

Envy really shines when using optional environment variables with the `#:default` option. The type of the value given to `#:default` doesn't need to be the same type of the environment variable itself, and if it isn't, Envy will assign the value a union type.

```
> (define-environment
    [num-threads : Positive-Integer #:default #f])
> num-threads
- : (U Positive-Integer #f)
#f
```

This added level of type-safety means it's easy to manage optional variables that don't have reasonable defaults: the type system will enforce that all code considers the possibility that such variables do not exist.

# And more...

To see the full set of features that Envy already provides, [take a look at the documentation][envy-docs]. That said, this is just the first release based on my initial use-cases, but I'm sure there are more features Envy could have to accommodate common application configuration patterns. If you have an idea that could make Envy better, [open an issue and make a suggestion][envy-issues]! I already have plans for a `#lang envy` DSL, which will hopefully cut the boilerplate out in its entirety.

And finally, to give credit where credit is due, Envy is heavily inspired by [Envied][envied] (both in name and function), an environment variable manager for Ruby, which I've used to great effect.

Try it out!

- `raco pkg install envy`
- [Envy on GitHub][envy]
- [Envy documentation][envy-docs]

[12factor]: http://12factor.net
[12factor-config]: http://12factor.net/config
[envied]: https://github.com/eval/envied
[envy]: https://github.com/lexi-lambda/envy
[envy-docs]: https://lexi-lambda.github.io/envy/envy.html
[envy-issues]: https://github.com/lexi-lambda/envy/issues

    Title: Climbing the infinite ladder of abstraction
    Date: 2016-08-11T18:03:48
    Tags: programming languages, racket, haskell

I started programming in elementary school.

When I was young, I was fascinated by the idea of automation. I loathed doing the same repetitive task over and over again, and I always yearned for a way to [solve the general problem][xkcd-general-problem]. When I learned about programming, I was immediately hooked: it was *so easy* to turn repetitive tasks into automated pipelines that would free me from ever having to do the same dull, frustrating exercise ever again.

Of course, one of the first things I found out once I’d started was that nothing is ever quite so simple. Before long, my solutions to eliminate repetition grew repetitive, and it became clear I spent a lot of time typing out the same things, over and over again, creating the very problem I had initially set out to destroy. It was through this that I grew interested in functions, classes, and other repetition-reducing aids, and soon enough, I discovered the wonderful world of **abstraction**.

# The brick wall of inexpressiveness

When I started programming, I was mostly playing with ActionScript and Java, just tinkering with things and seeing what I could come up with. I had quite a lot of fun, and the joy of solving problems hooked me almost immediately, but I also ran into frustrations pretty quickly. Specifically, I started writing a lot of code that looked like this:

```java
public String getName() {
  return this.name;
}

public void setName(String name) {
  this.name = name;
}
```

This is a bit of a cheap example, given that Java getters and setters are something of a programming language punching bag at this point, but I really did write them, and I really did get frustrated by them! I learned object-oriented design patterns, and I pored over books, forum threads, blog posts, and Stack Overflow questions about how to structure code to prevent spaghetti, but no matter how hard I tried, I kept having to type things that looked suspiciously similar to each other.

It was really quite frustrating, because no matter how I approached the problem, I ended up with a boilerplate-heavy mess. The *whole reason* I got started programming was to avoid this sort of thing, so what could I do? Well, it became increasingly obvious to me that Java had to go, and I needed to try something else. I started learning two very different programming languages, JavaScript and Objective-C, and I liked them both, for different reasons.

When I learned JavaScript, I discovered the closure, the first-class function, and I was entranced by it. Through jQuery, I learned of its power to design APIs that could be fun to use, dropping the boring, “heavy” feeling that Java carried around everywhere. With Objective-C, on the other hand, I learned about the power of a more dynamic object system, something with interesting syntax and the ability to handle “message passing” at a far higher level than Java ever could.

Both of these languages were flawed, as all languages are, but they opened my mind to the idea that *programming languages* could drastically influence the way I thought about problem solving, and they set me on a quest to find the programming language that would eliminate boilerplate once and for all.

# Discovering Lisp

Over the next few years, I grew to appreciate JavaScript’s small, simple core, despite rather disliking its object system and poor faculties for user-friendly data modeling. I pored over its history, and I found out that its design was heavily influenced by an obscure little language called Scheme, as well as an even more obscure language called Self, and a part of me started to wonder what it would be like to incorporate those languages’ ideas without some of the compromises JavaScript had made.

This idea lingered in the back of my head for a couple years, and while I tried to play with Scheme a couple times, it was simply too inaccessible for me. I was used to languages with powerful, easy to use IDEs, and when I found myself with nothing more than a command-line executable and rather scarce documentation, I was at a loss for how to begin. Even if I could do math in the REPL, where could I go from there? I’d started programming by building games, then websites. What could I possibly do with Scheme?

The language (or rather, its lack of an ecosystem) proved too intimidating for me at that young age, but the idea of Lisp’s homoiconicity stuck with me. Eventually, I started to design my very own programming language, a [highly dynamic Lisp with a prototypal object system called Sol][sol-lang]. I worked on it for about a year, and when I was done with it, it had a not-too-shabby complement of features: it had lambdas, macros, a fully-featured object model, and a CommonJS-esque module system, complete with the ability to dynamically import arbitrary C extensions. It was by far the largest project I’d ever worked on, and when I was done, I was pretty pleased.

Unfortunately, it was also abysmally slow.

I turned to a local college to find some people who could give me feedback and maybe point me in the right direction, and someone told me about another obscure programming language called [Racket][racket-lang]. At about the same time, someone pointed me to a totally different language called [Haskell][haskell-lang]. This was uncharted territory for me, and for a while, I didn’t really explore either of those languages further. Eventually, though, I dove into them in earnest, and what I found has dramatically altered my perspective on programming since then.

# A journey into complexity

Fast forward about three years, and today, I am employed writing Haskell, and I spend most of my free time writing Racket. These languages left a mark on me, and while I’ve learned *so much more* since then, I find myself continually bucking the mainstream and coming back to functional programming, hygienic macros, and possibly the most powerful type system in existence in a production-ready programming language.

I’ve also started realizing something else, though: **the languages I’ve settled into are *really complicated*.**

When I started programming, I thought about things like numbers, text, and shapes on a screen. Before long, I learned about functions, then classes, then message-passing and lambdas. I dove into macros and typeclasses, and now I speak in functors and monads, sets of scopes and internal definition contexts, and parser combinators and domain specific languages.

Why?

Sometimes I talk to fellow programmers, and they are horrified by the types of terms I fling around. “Why would you ever need something called a ‘monad’?” they ask, completely perplexed. “Macros are confusing,” they argue. “Being explicit is better.”

Obviously, I disagree, but why? What have I given up? If my fellow programmers cannot understand what I’m writing, is it actually worth it?

I’ve searched for years to find a programming language that will eliminate boilerplate, that will allow me to express my ideas succinctly and cleanly, that will let me turn hard problems into trivial ones, and I’ve discovered two completely different approaches to tackling those issues. Racket has macros, and Haskell has its fancy type system. Both of these things are lightyears ahead of where I was nearly a decade ago, writing dozens of lines of repetitive Java that ultimately did very little, but I’m still dealing with the same problems.

Racket knows too little about my program—it can’t figure out what I mean based on the type of thing I’m operating on because it is (mostly) dynamically typed. I *still* have to clarify myself and write things that feel redundant because the computer isn’t smart enough to figure out the “obvious”. Similarly, Haskell is too limiting—the compiler cannot deduce constraints I can solve in my head in seconds, and its syntax is not extensible like Racket’s is. Every day, I peer into piles upon piles of monadic computation, and really, what have I gained?

## Improvement, but never mastery

Like almost anything in life, programming is not really a perfectable art. There’s always some unlearned skill or undiscovered technique, and part of this potential for perpetual self-improvement is one of the things that I find so attractive about the field. That said, I this it is reasonable to say that certain languages have higher ceilings than others.

For example I am pretty confident that I *get* JavaScript. The language has lots of nooks and crannies that I don’t completely understand, but I feel pretty confident that I understand its semantics well enough to be able to grasp any piece of JavaScript code without too much incredulity. Now, that’s not to say that JavaScript is a simplistic language—far from it—but most of the ways I improve my JavaScripting abilities are learning new techniques *within* the language, not entirely new linguistic constructs.

On the other hand, languages like Haskell and Racket tend to blur the line. I feel like I have a good grasp of Haskell’s core, but do I have a good intuition for laziness? Do I completely grok type families? What about `TypeInType`? Ultimately, I have to come to the conclusion that I do not fully understand Haskell, much less a lot of the advanced category theory that composes some of its most powerful libraries. Racket manages to blur the line between language and library even further, and while I consider myself a decent Racketeer, I absolutely do *not* have a good grasp on all the intricacies of Racket’s macro system.

This is especially obvious to me at work, given that I write Haskell in a team setting. Just like back when I was writing Java, I end up with solutions that don’t satisfy me, and I reach for increasingly powerful constructs to help alleviate my qualms. Sometimes, I find myself cracking out `DataKinds`, and it might even help my problem, but there’s a cost: my coworkers are sometimes confused.

Every time I climb to the next rung on the ladder of abstraction, those only a couple rungs below me (even if we’re all hundreds of rungs up!) find themselves perplexed. In the worst case, people may even blame their confusion on their own inadequacy or lack of skill. This is *terrible*, especially when I know that, by the time they’ve caught up, I’ll be off playing with some new toy: comonads or type families or classy lenses. The cycle continues, and nobody is ever truly satisfied—I always want to find a new abstraction that will make things simpler, and those just a couple steps behind me struggle to keep up.

Of course, I experience it from the opposite perspective just as often: I delve into Edward Kmett’s fancier libraries or Phil Freeman’s blog posts about category theory, and I recognize that I am rather lost. Sometimes, I find myself understanding things, but just as often, I cannot wrap my head around the concepts being discussed. I may figure them out eventually, sure, but by then everyone else has moved on to even *more* advanced things, and still, none of them truly solve my problems.

# Ultimately, it all has (at least a little) value

It would be nice to think about all that and say, well, “Let’s finally break the cycle. Let’s stop deluding ourselves into thinking our solutions to our self-made problems are actually solving anything.” It would be great if I could tell myself that, but I unfortunately really can’t.

The scariest part of all is that I think it’s completely worthwhile.

So much of these more and more complicated abstractions are trying to do the same basic thing: come up with a better way of modeling the problem. In some sense, that’s all programming really is, modeling a domain in a way that can be leveraged by a digital computer. Our increasingly complicated DSLs *seem* unnecessarily complicated, they *seem* increasingly removed from reality, but that’s only because we’re getting better at creating languages that are closer to our domains without the baggage of preconceptions that came before us.

The downside is that, without an understanding of those preconceptions, a lot of what we come up with seems like patent gibberish to those unaware of our languages’ history.

Most programmers, even those who have never seen BASIC before, can figure out what this snippet does:

```BASIC
10 INPUT "What is your name: "; U$
20 PRINT "Hello "; U$
```

On the other hand, very few would probably understand this one:

```haskell
-- | A class for categories.
--   id and (.) must form a monoid.
class Category cat where
    -- | the identity morphism
    id :: cat a a

    -- | morphism composition
    (.) :: cat b c -> cat a b -> cat a c
```

Yet very few new programs are being written in BASIC, and lots are being written in Haskell.

Even one of the most popular, fastest-growing programming languages in the world, JavaScript, a language considered relatively accessible compared to things like Haskell, would likely be incomprehensible to a programmer not familiar with its syntax:

```js
export const composeWithProps = curry((a, parentProps, b) => {
  const composed = childProps =>
    createElement(a, parentProps, createElement(b, omit(['children'], childProps), childProps.children));
  // give the composed component a pretty display name for debugging
  composed.displayName = `Composed(${getDisplayName(a)}, ${getDisplayName(b)})`;
  return composed;
});
```

Moving towards increasingly specialized syntaxes is not inherently bad—it can often be indicative of a more streamlined, domain-specific way of thinking—but while it may dramatically increase the productivity of a seasoned programmer, it can be nothing short of baffling to a newcomer.

That, specifically, is the crux of my fear: are we always aware of who we are optimizing for? I do not have a moral problem with writing code to optimize concision for seasoned programmers; after all, brevity is one of the primary ways code is made more readable (verbosity is the enemy of understanding). However, when that concision comes at the cost of beginners’ understanding, the picture becomes a bit more grey. It is not wrong to write things that are highly optimized for one’s own knowledge and understanding, and establishing a group of such people can make for an *extremely* productive team. It’s just also important to understand that others will likely be confused, and without being willing to invest the time and money into education, smart, diligent people will still fail to grasp the concepts, and they will likely be wholly uninterested in them.

## Reactionary anti-intellectualism and the search for moderation

I have noticed lately that people close to my circles have started regularly slinging insults at people who work in highly specialized notation. Math, including things like category and type theory, has become an especially acceptable punching bag. [I recently tweeted a picture of some rather dense mathematics from a paper I’d read][math-tweet], and I was frankly disturbed at some of the vitriolic responses. Academia is sometimes described as “masturbatory”, and honestly, that is both offensive and hypocritical.

Mathematical notation is not perfect, no more than dense Haskell, heavily metaprogrammed Ruby, or IIFE-packed JavaScript. Still, it serves a purpose, and sometimes spelling things out is neither practically feasible nor a theoretical improvement. Programmers would not take kindly to being asked to write all their code out as prose, nor would they like being told that using higher-order functions like `map` should be banned because they are too confusing and not immediately self-explanatory.

I am glad that people are focusing on usability and accessibility more than ever, and I think that’s one of the areas I’m the most interested in. I want to get the best of both worlds: I aim to write code in a highly concise, precise style, but I try and produce intuitive interfaces with human-readable errors upon failure. To me, a user-hostile yet technically functional library is a buggy one, and I would happily file a bug report about a confusing API or error message.

Abstraction is what seems to make programming possible, and indeed, it’s what makes most modern *technology* possible. It’s what allows people to drive a car without knowing how an internal combustion engine works, and it’s what allows people to browse the web without having a deep understanding of internet protocol. In programming, abstraction serves a similar purpose. Of course, just like all tools, abstractions can have rather different goals: the average user will not pick up Photoshop in a day, but a power user is not going to be satisfied with Paint.

Programmers are professionals, and we work in a technical domain. I am absolutely of the belief that programming, like any other field, is not always about what comes easiest: sometimes it’s important to sit down and study for a while to grok a particularly complicated concept, and other times, it’s simply important to learn by trying, failing, and asking questions. I strive to find that blend of accessible, concise, and robust, and just like everything else, that target shifts depending on the situation and people I’m working with.

I honestly don’t know if Racket and Haskell are worth their costs in complexity. At the end of the day, maybe what really matters is writing simple, consistent things that other people can understand. I really hope that there is a place for more powerful languages within a team, but there’s something to be said about which languages tend to get the most popular.

Ultimately, though, I am just trying to be aware of the tradeoffs I’m making, the benefits I’m getting, and the impact on those I’m working with. I will continue to search for abstractions that can better fit my needs, and I am sure I will keep on climbing the ladder of abstraction for years to come—I just really hope I’m not wasting my time.

[haskell-lang]: https://www.haskell.org
[math-tweet]: https://twitter.com/lexi_lambda/status/763111451691134976
[racket-lang]: http://racket-lang.org
[sol-lang]: https://github.com/lexi-lambda/libsol
[xkcd-general-problem]: https://xkcd.com/974/

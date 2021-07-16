    Title: Empathy and subjective experience in programming languages
    Date: 2019-10-19T08:35:44
    Tags: haskell, programming languages, philosophy

A stereotype about programmers is that they like to think in black and white. Programmers like things to be good or bad, moral or immoral, responsible or irresponsible. Perhaps there is something romantic in the idea that programmers like to be as binary as the computers they program. Reductionist? Almost certainly, but hey, laugh at yourself a bit: we probably deserve to be made fun of from time to time.

Personally, I have no idea if the trope of the nuance-challenged programmer is accurate, but whether it’s a property of programmers or just humans behind a keyboard, the intensity with which we disagree with one another never ceases to amaze. Ask any group of working programmers what their least favorite programming language is, and there’s a pretty good chance things are going to get heated real fast. Why? What is it about programming that makes us feel so strongly that we are right and others are wrong, even when our experiences contradict those of tens or hundreds of thousands of others?

I think about that question a lot.

# 2015 called, and they want their dress back

Humans have a knack for caring intensely about the most trivial of things. Name almost anything—cats versus dogs, the appropriate way to fasten a necktie, or even which day of the week comes first—and someone somewhere has probably written an essay about it on an internet forum. It would be easy to throw up our hands and give up trying to understand our peers, as sometimes they seem like aliens from another planet.

However, what interests me is how the littlest things seem to get people the most upset. Few people have shouting matches over the best interpretation of quantum mechanics, but friendships will be tested when someone says they just aren’t that into *Star Wars*. One explanation for this phenomenon is simple accessibility: most people aren’t equipped to understand quantum mechanics well enough to argue about it, but almost anyone can have an opinion on which direction the toilet paper is supposed to go.[^1]

There is truth in that explanation, but personally, I don’t think it’s the whole story. Rather, I think we grow so used to the idea that our experiences are universal that discovering someone else experienced the exact same thing we did yet came to a different conclusion is not just frustrating: it’s incomprehensible.

Take 2015’s phenomenon of “[the dress][wiki:the-dress]” as an example. Some people see black and blue, others white and gold, and frankly, whether you see one or the other has no impact on anything remotely meaningful. How did *this*—something so completely irrelevant—become a cross-cultural phenomenon reported on by major news outlets? My guess: people just aren’t used to the idea that vision—the primary way we sense the world—does not provide us with an objective, universal understanding of reality.

## When something objective isn’t

Our culture and society works because, in spite of our differences, we’re still all humans. We eat food, we sleep, we like spending time with each other, and we like feeling connected to those around us. So when we watch a movie, and it tickles us in a way that makes us feel good, we can have an awfully hard time understanding how our best friend—who we largely agree with about everything—didn’t like it at all.

The truth, of course, is that very little of what we experience is in any way objective. Yes, we can be pretty confident that basic arithmetic is true anywhere in the universe, and that if we all agree a table is brown it probably is. There are even things we accept as subjective without a second thought, such as the kinds of food people like or the fashions they find attractive. It’s all the in-betweens that are so pernicious! “The dress” was so unbelievable to most people because, nine hundred and ninety nine times out of of a thousand, when two humans look at a picture, they at least mostly agree on the colors contained within. We do not consider that we are seeing different lenses into the same objective reality, we simply think we are perceiving objective truths directly.

In the case of the dress, whether you [heard “yanny” or “laurel,”][wiki:yanny-laurel] or whether you believe the *Sonic* games were ever any good, subjective disagreement is essentially harmless. But what about when it isn’t? Might incorrect beliefs that our experiences are universal cause genuine harm?

I think the answer is absolutely, unequivocally *yes*.

# Subjectivity in programming, and in programming languages specifically

Quick question: which is better, functional or imperative programming?

My guess, given the usual subject of my blog, most of my readers would pick the former. However, the actual answer you chose doesn’t matter: my guess is you feel like you have a pretty rational argument to back it up. It certainly isn’t simply a matter of taste… right?

Well, no, I hope not. I don’t think the world is so subjective that we cannot ever advocate for one thing over another—we tried that whole “everything is XML” thing for a while, and I think we agreed it really wasn’t a good idea. But if you truly believe your answer to the above question can be completely objectively justified (as many do), how does one explain the average Hacker News comment thread on just about any post about Haskell?

I generally try not to read Hacker News if I can help it, as I find doing it mostly just makes me angry,[^2] but I did happen to find a link to [a recent discussion][hn:haskell-in-production] on a blog post about using Haskell in production. Let’s take a look at a few comments, shall we?

In a [branch of the discussion](https://news.ycombinator.com/item?id=21284383), one user writes:

> > Haskell is great for business and great in production
>
> I disagree. It's a beautiful language but it lacks a lot of features to make it useable at scale. And in my experience Haskell engineers are extremely smart but the environment/culture they create makes it difficult to foster team spirit.
>
> I've been in 2 companies in the last 4 years who initially used Haskell in production. One has transitioned to Go and the other is rewriting the codebase in Rust.

The first paragraph is an assertion without many specifics, but it does sound like it could be reasonable. And although the last two sentences are entirely anecdotal, anecdotes are still better than hunches. Let’s see what someone else has to say in response:

> I’ve met some pretty damn solid engineers who started on Haskell and, even at a junior level in other languages, produce an elegant solution far more easily than a senior engineer in that language. You probably wouldn’t put the code in production verbatim but you can very easily see what’s going on and it isn’t haunted by spectre of early abstraction, which IMO is the biggest flaw of OOP at scale.
>
> […]
>
> From my naive perspective it’s easy to make classes out of everything, and to hold state and put side-effects everywhere, but you don’t want to deal with the trouble of a monad until you need it. So you have an automatic inclination towards cleaner code when you start functional and move on.

Also pretty vague and high-level, but also sounds reasonable. If you read either of these comments, and your first inclination was to grow frustrated and start crafting counter-arguments in your head, I encourage you to step outside your feelings momentarily (rational as they may be!) and try your very hardest to interpret them charitably. The discussion continues:

> Haskell gives one plenty of rope to hang himself on complexity.
>
> So much that developers develop an aversion to it as deep as fear. It's unavoidable, the ones that didn't develop it are still buried at the working of their first Rube Goldberg machine and unavailable.

Whether you think it’s accurate or not, there is definitely a perception held by a great many people that Haskell is a very complicated language. Surely at least some of them must have given it an honest shot, so have they just not “seen the light” yet? What do you think they’re missing? Perhaps a followup commenter can help elucidate things:

> Hi, I find that everything people here are complaining about (and they're valid complaints) has also been true of C++. C++ developed a lot of its complexity (particularly 15-20 yrs ago in the template space) after it got popular, so people were already wed to it.
>
> […]
>
> The C++ community's really gotten good in the last 5 years or so about reigning in the bad impulses and getting people to write clean, clear, efficient code that has reasonable expressiveness.
>
> Coming into Haskell from C++, I have the same instincts. Haskell's been a pure pleasure. The benefits are really there, and they're easy to get. You just have to think of the trade-offs.

That argument seems reasonable, too. Everything in moderation, right? If you disagree, and you think Haskell is just not worth it, what does this person value that you don’t? What are they missing that you see?

## The unsatisfying subjective reality of programming languages

You can probably see where I’m going with all this. These arguments are not built on hard, refutable facts or rigorous real-world evidence, they’re based in gut feelings and personal preferences. Does that mean they’re wrong, invalid, and worthless, and we should do studies to determine which language allows programmers to ship features the fastest and with the fewest bugs, then all agree to use that?

***No!***

These conversations are subjective because, for better or for worse, humans think in different ways and value different things, and programming languages are the medium in which we express ourselves. To many people who write Haskell (myself included), there is an effervescent joy in modeling a problem with the type system—like capturing something in amber—that others just don’t care about. What’s more, some people clearly loathe Haskell’s significant whitespace and plethora of infix operators, but I’ve never really minded. Is one of us wrong? If so, *why?* Talk about reliability all you want, but the few rigorous numbers we have don’t provide much evidence one way or the other.

While [one commenter](https://news.ycombinator.com/item?id=21284317) in the aforementioned Hacker News thread described Haskell as nothing less than “pain and torture,” [another](https://news.ycombinator.com/item?id=21284540) says they “did some Haskell in production and it was delightful.” People push excuses and rationalizations for these differences constantly—they point out that most people are exposed to imperative programming first, while others retort that Haskell is clearly not very widely used despite being around for an awfully long time—but none of their arguments ever seem to change people’s minds.

Often, people walk away from these conversations confused and incensed. To them, their point of view is so obviously apparent that it is hard to fathom anyone else seeing things differently. They rack their brains trying to figure out why their opponents just don’t *get it.* There must be some key point they’ve misunderstood, some joy they haven’t experienced, some sharp edge they haven’t yet been cut by. But no matter how much time they spend trying to reach these people, somehow, it’s never enough.

## Empathy, and how bad results come from good intentions

I’ll admit that these kinds of discussions aren’t *always* fruitless; sometimes they really do manage to change people’s minds or help them see some new idea they had not been able to grasp. When people manage to keep their cool and acknowledge the differences in their mindsets while still helping people learn, everyone benefits.

Sadly, in my experience, this rarely happens. We have a natural tendency to become angry if people don’t see things the way we do; it’s confusing and disorienting, and it can even disgust us. None of those emotions are conducive to empathy. When we fail to account for the ways in which others might think differently, we voluntarily reject any insights we might have otherwise gained from the conversation because we did not allow ourselves to embrace, even just temporarily, someone else’s strange and perhaps uncomfortable set of values and experiences. We refuse to accept that our perception of color might not be as universal as we thought, and we miss out on the amazing insights we could learn about the nature of light, color, and human vision.

Although failing to empathize with those we are arguing with is bad enough, in my mind, this failure to accept the potential subjectivity of one’s own views has even worse, indirect effects. Take this comment for example, again from the same thread:

> Sounds like you've barely programmed in Haskell and don't know what you're talking about. Haskell was the first language I learned. I didn't think this at all and I still don't. It doesn't strike me as any more difficult than learning Java or something.

I have no doubts that this commenter meant what they said: they didn’t find Haskell difficult to learn. The comment they were replying to was vitriolic and combative, so one could almost feel they had a smackdown coming to them… but this isn’t a private conversation. How do you think someone feels when they are learning Haskell, scroll through this thread, and find a comment that tells them they ought to find it easy? If they’ve been struggling, even a little bit, what do you think they might think?

If I were in their place, I might feel a little stupid. I might wonder if I’m really cut out for Haskell or if I should just give up. I definitely wouldn’t feel encouraged and excited to keep trying.

Who knows why this commenter found Haskell straightforward. Maybe they were exposed to certain concepts already, maybe it just fit their style of thinking, perhaps they’re even exceptionally smart. I don’t know. But no matter what the answer is, insulting the intelligence of others, even indirectly in this way, belies a lack of empathy in the face of frustration, and although the intent may not have been to hurt, it can still be seriously harmful.

To be clear, I’m not saying the commenter should have pretended their experiences were different or even kept them to themselves. I don’t believe in being “fake nice”—in my experience, I am best equipped to reach people when speaking genuinely, from the heart. What I would have done is tell my story in a different way, perhaps by writing something like this:

> It’s true that a lot of people find Haskell challenging, and I totally accept that some people just don’t think it’s worth it. It’s fine if you don’t want to write Haskell. But personally, I really enjoy writing it, as do the people I work with, and I think we ship great software with it because it aligns naturally—even joyfully!—with the way we like to think about program construction.
>
> Personally, I didn’t find Haskell as challenging to learn as I think some people have, but it was still work, and in some ways I was just exposed to it at the right time. Other people I know have struggled quite a lot at first, and reasonably so, but they’ve still managed to become great Haskell programmers, and they found it worthwhile. Our team dynamic just wouldn’t be the same in any other language.

When I respond to comments I disagree with, I try to tell a personal story that provides a different perspective **without** invalidating their experiences. Sometimes the result is ungrateful snark anyway (or just no response at all), but you might be surprised how often talking from an emotional place about *your own* experiences—while being neither aggressive nor especially defensive—can go a long way. Perhaps you can even learn something if they return the favor and explain what they find frustrating, beyond the fundamental, subjective disagreements.

It’s okay to have opinions. It’s okay to like and dislike things. It’s okay to be frustrated that others don’t see things the way you do, and to advocate for the technologies and values you believe in. It’s just not okay to tell someone else their reality is wrong.

Learn to embrace the subjective differences between us all, and you won’t just be kinder. You’ll be *happier.*


[^1]: This is where I’m supposed to put a snarky footnote saying something like “obviously, the correct way is *blah*,” but you deserve better. So you, uh, get a *meta* snarky footnote instead.

[^2]: Which, to be entirely fair, may well be as subjective as anything else in this blog post.

[hn:haskell-in-production]: https://news.ycombinator.com/item?id=21282647
[wiki:the-dress]: https://en.wikipedia.org/wiki/The_dress
[wiki:yanny-laurel]: https://en.wikipedia.org/wiki/Yanny_or_Laurel

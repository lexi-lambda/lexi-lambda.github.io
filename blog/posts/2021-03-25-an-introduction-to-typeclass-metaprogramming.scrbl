#lang scribble/base

@(require "../scribble/post-language.rkt")

@title{An introduction to typeclass metaprogramming}
@date+tags["haskell" "types" "functional programming"]

@emph{Typeclass metaprogramming} is a powerful technique available to Haskell programmers to automatically generate term-level code from static type information. It has been used to great effect in several popular Haskell libraries (such as the @hackage-package{servant} ecosystem), and it is the core mechanism used to implement generic programming via @hackage-module*["base-4.14.1.0" "GHC.Generics"]{GHC generics}. Despite this, remarkably little material exists that explains the technique, relegating it to folk knowledge known only to advanced Haskell programmers.

This blog post attempts to remedy that by providing an overview of the foundational concepts behind typeclass metaprogramming. It does @emph{not} attempt to be a complete guide to type-level programming in Haskell—such a task could easily fill a book—but it does provide explanations and illustrations of the most essential components. This is also @emph{not} a blog post for Haskell beginners—familiarity with the essentials of the Haskell type system and several common GHC extensions is assumed—but it does not assume any prior knowledge of type-level programming.

@section{Part 1: Basic building blocks}

Typeclass metaprogramming is a big subject, which makes covering it in a blog post tricky. To break it into more manageable chunks, this post is divided into several parts, each of which introduces new type system features or type-level programming techniques, then presents an example of how they can be applied.

To start, we’ll cover the absolute foundations of typeclass metaprogramming.

@subsection{Typeclasses as functions from types to terms}

As its name implies, typeclass metaprogramming (henceforth TMP@note:cpp-tmp) centers around Haskell’s typeclass construct. Traditionally, typeclasses are viewed as a mechanism for principled operator overloading; for example, they underpin Haskell’s polymorphic @code{==} operator via the @code{Eq} class. Though that is often the most useful way to think about typeclasses, TMP encourages a different perspective: @bold{typeclasses are functions from types to (runtime) terms}.

@define-footnote[cpp-tmp]{Not to be confused with C++’s @wikipedia{@emph{template} metaprogramming}, though there are significant similarities between the two techniques.}

What does that mean? Let’s illustrate with an example. Suppose we define a typeclass called @code{TypeOf}:

@code-block{
class TypeOf a where
  typeOf :: a -> String}

The idea is that this typeclass will accept some value and return the name of its type as a string. To illustrate, here are a couple potential instances:

@code-block{
instance TypeOf Bool where
  typeOf _ = "Bool"

instance TypeOf Char where
  typeOf _ = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf (a, b) = "(" ++ typeOf a ++ ", " ++ typeOf b ++ ")"}

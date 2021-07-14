#lang scribble/base

@(require scribble/core
          "../scribble/post-language.rkt")

@title[#:style (style #f (list (document-date (infer-date))
                               (post-tags (list "haskell" "types" "functional programming"))))]{
  An introduction to typeclass metaprogramming}

@emph{Typeclass metaprogramming} is a powerful technique available to Haskell programmers to automatically generate term-level code from static type information. It has been used to great effect in several popular Haskell libraries (such as the @hackage-package{servant} ecosystem), and it is the core mechanism used to implement generic programming via @hackage-module*["base-4.14.1.0" "GHC.Generics"]{GHC generics}. Despite this, remarkably little material exists that explains the technique, relegating it to folk knowledge known only to advanced Haskell programmers.

This blog post attempts to remedy that by providing an overview of the foundational concepts behind typeclass metaprogramming. It does @emph{not} attempt to be a complete guide to type-level programming in Haskell—such a task could easily fill a book—but it does provide explanations and illustrations of the most essential components. This is also @emph{not} a blog post for Haskell beginners—familiarity with the essentials of the Haskell type system and several common GHC extensions is assumed—but it does not assume any prior knowledge of type-level programming.

@section{Part 1: Basic building blocks}

Typeclass metaprogramming is a big subject, which makes covering it in a blog post tricky. To break it into more manageable chunks, this post is divided into several parts, each of which introduces new type system features or type-level programming techniques, then presents an example of how they can be applied.

To start, we’ll cover the absolute foundations of typeclass metaprogramming.

@subsection{Typeclasses as functions from types to terms}

As its name implies, typeclass metaprogramming (henceforth TMP@note:cpp-tmp) centers around Haskell’s typeclass construct. Traditionally, typeclasses are viewed as a mechanism for principled operator overloading; for example, they underpin Haskell’s polymorphic @code{==} operator via the @code{Eq} class. Though that is often the most useful way to think about typeclasses, TMP encourages a different perspective: @bold{typeclasses are functions from types to (runtime) terms}.

@define-footnote[cpp-tmp]{Not to be confused with C++’s @wikipedia{@emph{template} metaprogramming}, though there are significant similarities between the two techniques.}

What does that mean? Let’s illustrate with an example. Suppose we define a typeclass called @haskell{TypeOf}:

@haskell-block{
class TypeOf a where
  typeOf :: a -> String}

The idea is that this typeclass will accept some value and return the name of its type as a string. To illustrate, here are a couple potential instances:

@haskell-block{
instance TypeOf Bool where
  typeOf _ = "Bool"

instance TypeOf Char where
  typeOf _ = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf (a, b) = "(" ++ typeOf a ++ ", " ++ typeOf b ++ ")"}

Given these instances, we can observe that they do what we expect in GHCi:

@code-block{
ghci> @haskell{typeOf (True, 'a')}
@haskell{"(Bool, Char)"}}

Note that both the @haskell{TypeOf Bool} and @haskell{TypeOf Char} instances ignore the argument to @haskell{typeOf} altogether. This makes sense, as the whole point of the @haskell{TypeOf} class is to get access to @emph{type} information, which is the same regardless of which value is provided. To make this more explicit, we can take advantage of some GHC extensions to eliminate the value-level argument altogether:

@haskell-block|{
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

class TypeOf a where
  typeOf :: String}|

@subsection{Type-level interpreters}

It is impossible to construct any values of these types, but we can nevertheless use them to construct natural numbers at the type level:

@itemlist[
  @item{@haskell{Z} is a type that represents 0.}
  @item{@haskell{S Z} is a type that represents 1.}
  @item{@haskell{S (S Z)} is a type that represents 2.}]

The problem is that a function definition has a closed set of clauses matched from top to bottom, but typeclass instances are open and unordered.@note:instance-chains This means GHC will complain about instance overlap if we try to evaluate @haskell["isUnit @()"]:

@code-block{
ghci> @haskell["isUnit @()"]

error:
    • Overlapping instances for @haskell{IsUnit ()}
        arising from a use of ‘@haskell{isUnit}’
      Matching instances:
        @haskell{instance IsUnit a}
        @haskell{instance IsUnit ()}}

@define-footnote[instance-chains]{There have been proposals to introduce ordered instances, known in the literature as @hyperlink["https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-icfp2010-instances.pdf"]{@emph{instance chains}}, but as of this writing, GHC does not implement them.}


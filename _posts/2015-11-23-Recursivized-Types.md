---
layout: post
title: Recursivized Types
comments: True
---


So, a short break from \\(\varepsilon---\delta\\).

Anyway, I'm going to look at Algebraic types in this section. Specifically, I am going to use Haskell types.

The provided code is all actual Haskell, in fact, I'm going to have to de-import the standard libraries because some of this is actually defined there. You can find all the code [here](/src/2015-11-23-Recursive-Types.hs)

<!--
```haskell
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

import Prelude(Show, show, (++))
```
-->

## Algebraic types

I'll start with a quick go-over of Haskell types. If you already know Haskell, feel free to skip this section.

### Enumeration types

Some types are enumerations of potential values.

A quick example is the trivial singleton type:

```haskell
data Singleton = Single
    deriving Show
```

This is to be read as "there exists a type Singleton, which contains one element named Single." The "deriving show" part is Haskell book keeping for those following along on their interpreters to be able to see things

Another example is

```haskell
data Void
```
This is to be read as "there exists a type Void with no possible elements"

Yet another type is

```haskell
data Bool = False | True
    deriving Show
```

This is to be read as "there exists a type Bool with the possible elements True and False."

### Structure types

Types can be used as as structures, as such:

```haskell
data Wrapper a = Wrap a
    deriving Show
```

This is to be read as "for every type `a` there exists a type `(Wrapper a)` with a single type constructor `Wrap` that takes an `a`"

A more complicated structure is

```haskell
data Pair a b = ConsPair a b
    deriving Show
```

Which is similar to the previous one except that it contains two possible elements which can be of two different types.


### Aside: Kinds

Algebraic types have kinds.

1. `Singleton`, `Void`, and `Bool` are concrete types in their own right.
2. `Wrapper` needs a type before it can become a type (nothing can be a `Wrapper` by itself).
3. `Pair` needs two types before it can become a type (nothing can be a `Pair` or `Pair Bool` by itself).

We refer to the latter two cases as "higher-kinded types."

We say that `Singleton`, `Void`, etc. have kind `*`. Types that need one type added on the end to be complete, such as `Wrapper` or `Pair Bool` have kind `* -> *`. Types such as `Pair`, which needs two types to be complete, have kind `* -> * -> *`.

Note also that types such as

```haskell
data TwoOfSame a = ConsTwoOfSame a a
```

have kind `* -> *` rather than `* -> * -> *` because they only take one type argument. `TwoOfSame Bool Singleton` wouldn't make any sense.

### Mixing Structures and Enumerations

Algebraic types are a generalization of both systems, in which you can have multiple constructors, each of which can contain zero or multiple arguments.

A canonical example combines both forms to create something resembling the concept of `null` from Java or `None` from Python.

```haskell
data Maybe a = Nothing | Just a
    deriving Show
```

Here, there is a combination of the enumeration and structure style. Interestingly, while, for example, `Just True :: Maybe Bool` (`::` is to be read as "is of type"), `Nothing :: Maybe a`, which means that it can take on any `Just` type (this is key to what we will do later).

## Recursive Algebraic types

Recursive types are similar to the examples shown, except that they can also contain themselves. For example, the not particularly efficient definition of the natural numbers ([a natural number is either zero or the successor of another natural number](https://en.wikipedia.org/wiki/Peano_axioms)):

```haskell
data Nat = Zero | Succ Nat
    deriving Show
```
Another algebraic type is a stream, which is defined as follows, which is basically an infinite list.

```haskell
data Stream a = ConsStream a (Stream a)
    deriving Show
```

This only works because you can define values as follows, where in each case the same variable name refers to the same value:

```haskell
trues = ConsStream True trues
falses = ConsStream False falses
alternating = ConsStream True (ConsStream False alternating)

elementAt (ConsStream value rest) Zero = value
elementAt (ConsStream value rest) (Succ x) = elementAt rest x
```

So if you ask for the (fifth element of trues), that reduces to the (fourth element of (the rest of trues)) which reduces to (the fourth element of trues), etc., until you get to the value `True`.

A potentially terminating list can be represented as follows:

```haskell
data List a = Nil | Cons a (List a)
    deriving Show
```

Here the list can possibly terminate because you can stop with `Nil`. For example, you can represent `[True, False, False]` as

```haskell
exampleList = Cons True (Cons False (Cons False Nil))
```

You could also use stream-like circular definition for a list too, so streams are just a subset of lists.

## Higher-kinded type parameters

OK, one final bit of craziness before we get to our actual example. Let's say we want to create a type representing the shape of another type.

```haskell
data Shape f = Structure (f Singleton)
```

If we, for example, look at `Shape List`, we get the values `Structure Nil`, `Structure (Cons Single Nil)`, `Structure (Cons Single (Cons Single Nil))`, etc. Basically, we get a picture of what the type looks like independent of its elements.

Notice that on the right side of that equation we have `(f Singleton)` as a concrete value, meaning that `f` is of kind `* -> *`. Therefore, `Shape` is of kind `(* -> *) -> *`.

Another example of a `(* -> *) -> *` is the `NullOf` type, which filters a type for only its nullary constructors, those which do not take a value.

```haskell
data NullOf f = Null (f Void)
```

For example, the only possible value of `NullOf List` is `Null Nil` and the only possible value of `NullOf Just` is `Null Nothing`.

## A general encoding of Recursive Types

You might notice a correspondence between the types `Maybe` and `Nat`:

```haskell
    data Maybe a = Nothing | Just a
    data Nat = Zero | Succ Nat
```

Basically, `Nat` is just `Maybe Nat`. We can encode the conversion from `Maybe` to `Nat` as follows:

```haskell
data Recursive f = Recurse (f (Recursive f))
```

<!--
```haskell
instance (Show (f (Recursive f))) => Show (Recursive f) where
    show (Recurse x) = "Recurse (" ++ show x ++ ")"
```
-->

This definition means that for all types `f` of kind `* -> *`, there exists a type called `Recursive f` (let's call this `rf` for now) which contains a single element whose type is `f rf`. In other words, `Recursive` takes a type and returns a recursive version of that type.

Constructing a value of `Recursive *` at first seems difficult. For example, let us look at `Recursive Maybe`.

To construct a type of `Recursive Maybe` we need to have an element of type `Maybe (Recursive Maybe)`. The only element of this we have is `Nothing :: Maybe a`. We can therefore construct the value `Recurse Nothing :: Recursive Maybe`. Using this value we can construct the value `Just (Recurse Nothing) :: Maybe (Recursive Maybe)` and therefore the value `Recurse (Just (Recurse Nothing)) :: Recursive Maybe`. We can continue to build values in this manner:

```haskell
zero' = Recurse Nothing
one' = Recurse (Just zero')
two' = Recurse (Just one')
three' = Recurse (Just two')
```

Notice that I used the names for numbers for these. This is to demonstrate their correspondence with the natural numbers:

```haskell
zero = Zero
one = Succ Zero
two = Succ one
three = Succ two
```
Basically, `Zero` is `Nothing` and `Succ` is `Recurse . Just`, where `.` is function composition.

Interestingly, there is another possible value of these types

```haskell
omega' = Recurse (Just omega')
omega = Succ omega
```

The name `omega` is used because this number is similar to [one particular kind of infinity](https://en.wikipedia.org/wiki/Ordinal_number). because of the way that it reacts to this definition of comparison:

```haskell
_ < Zero        = False
Zero < Succ _   = True
Succ x < Succ y = x < y
```

As you can see, `x < omega` is `True` for all `x` defined by `Succ (Succ (Succ ... Succ (Zero)))`. Note that `omega < omega` will never evaluate since `omega < omega` reduces to `Succ omega < Succ omega`, which reduces to `omega < omega`.

## Recursivizing Types

To begin with, let's look at the entirely contrived types:

```haskell
data Constant a = Const
    deriving (Show)
data Empty a
```

We can see that there is one possible value for `Constant (Recursvie Constant)`, which is `Const`. Therefore, the only possible value for `Recursive Constant` is `Recurse Const`. Therefore, `Recursive Constant` is similar to the Singleton:

```haskell
single = Single
single' = Recurse Const
```

Similarly, there is no value `Empty (Recursive Empty)` possible, so there is no possible value of `Recursive Empty`.

Let's now look at a less contrived `* -> *` type, Wrapper, and see how it works. `Recursive Wrapper` requires a value of `Wrapper (Recursive Wrapper)` in order to initialize. There is in fact only one way to do this:

```haskell
wrappedRecurse = Recurse (Wrap wrappedRecurse)
```

This type is also basically `Singleton` since it only has one value

We can also use the partial application of `Pair` to a single type variable `a` and then construct a recursion of the pair. We can then analyze values of `Recurse (Pair Bool)`. At first, this doesn't seem to work any better than `Recurse Wrap` because we need a value of `Pair Bool (RPair Bool)` before we can create a value of `RPair Bool`. However, since in this case each value also has a `Bool`, these form a stream:

```haskell
trues' = Recurse (ConsPair True trues')
falses' = Recurse (ConsPair False falses')
alternating' = Recurse (ConsPair True (Recurse (ConsPair False alternating')))
```

Which is remarkably similar to our previous definitions:

```haskell
trues = ConsStream True trues
falses = ConsStream False falses
alternating = ConsStream True (ConsStream False alternating)
```

## Finding the base type for recursive types

So now we know (I'm using \\(\equiv\\) to mean that two types have the same structure):

- `Recurse Maybe` \\(\equiv\\) `Nat`
- `Recurse Constant` \\(\equiv\\) `Singleton`
- `Recurse Wrapper` \\(\equiv\\) `Singleton`
- `Recurse Empty` \\(\equiv\\) `Void`
- `Recurse Constant` \\(\equiv\\) `Stream`

But what is `List` equivalent to? We can look again at the definition of a list:

```haskell
data List a = Nil | Cons a (List a)
```

OK, so we need to have a type parameter representing the `a` and another to be eaten by `Recurse`.

```haskell
data PreList a self = ???
```

And we need to have some element `Nil`.

```haskell
data PreList a self = PreNil | ???
```

Now, we just need something to combine both the first and rest of the list

```haskell
data PreList a self = PreNil | PreCons a self
```

Hey, what's on the right of the list looks a lot like a `Maybe` with two values instead of one! Here's a final implementation

```haskell
data PreList a self = PreList (Maybe (Pair a self))
    deriving (Show)

type RecurseList a = Recursive (PreList a)
```

Redoing our `[True, False, False]` example, we get:

```haskell
exampleList'
    = Recurse (PreList (Just (ConsPair
        True
        (Recurse (PreList (Just (ConsPair
            False
            (Recurse (PreList (Just (ConsPair
                False
                (Recurse (PreList Nothing)))))))))))))
```

No reasonable person (except (maybe (a (Lisp (fanatic))))) would say that the above code is pretty, but it is clear that `Recursive (PreList a)` \\(\equiv\\) `List`.

## Exercises

 - Find the de-recursified forms of

```haskell
data BinaryTree a = BNode (BinaryTree a) (BinaryTree a) | BLeaf a
data ListTree a = ListNode a (List (ListTree a))
```

 - What is a general algorithm for de-recursification?

The answers are given at the end of [the attached source code](/src/2015-11-23-Recursive-Types.hs).

<!--

```haskell
--------ANSWERS TO EXERCISES--------

data PreBinaryTree a self = PBNode self self | PBLeaf a
--- Recursive (PreBinaryTree a) === BinaryTree a

data PreListTree a self = PListNode (Pair a (List self))
    -- equivalent to: data PreListTree a self = PListNode a (List self)
--- Recursive (PreListTree a) === ListTree a

{-
    General solution:
        Add a self type parameter.
        Replace any recursive calls with self.
-}

```
-->

## Conclusion

OK, so this probably doesn't have any practical applications. And given how ugly the output of such code is, you're unlikely to ever see this in real life code, even in an academic language like Haskell.

On the other hand, I hope this hurt your brain a little; it certainly hurt mine. Really, to me, the best part is that with only a simple type system (e.g., non-recursive types), and a single recursive type, you can construct all other simply recursive types. This example shows to me how out of simplicity can grow complexity.

Also, abstract nonsense is `Just fun` :-).

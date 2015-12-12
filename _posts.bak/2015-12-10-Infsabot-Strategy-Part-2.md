preprocess:
    pass ../_scripts/codetosources.py
    replace "haskellcomment" -> "haskell"
    replace "<!--_-->" -> "<!--_-->"
---
layout: post
title: Infsabot Strategy Part 2
comments: True
---

dump: haskell as hs

OK, so to continue our Infsaboting.

### Correction

I made a mistake last time. I included `SwitchInt`, a way to switch on values of `ExprDir`, which is ridiculous since an `ExprDir` can only be constructed as a constant or an `if` branch to begin with.

So just imagine that I never did that.

## Guess and Check

OK, so how should our AI construct these syntax trees? At a very high level, we want to be able to 1) assess trees against collections of trees and 2) modify them randomly. We can represent this as a pair of types:

```haskell
asses :: [RP] -> RP -> Double
modify :: RP -> StdGen -> (RP, StdGen)
```

I think the implementation of `asses` should be pretty clear: simply find the win rate (given that we can simulate an unlimited number of games).

Modify, on the other hand, is a little more complicated. There are a few ways a tree can be modified:

 - Modify the value of a constant to be something else.
 - Modify the value of a field to point to something else or a field
 - Add leaves to the tree
 - Remove leaves as a simplification step

It might seem as if some of these factors might be aided by adding a field to everything that tracks typical values of the course of the application at different points within the tree. In this way, one can tell how big a difference between two strategies are. We can actually pretty easily keep track of this separately by recording a list of `KnownState`s from our simulations. For now, we'll just keep of all states or some random subset.

```haskell
type HistoricalStates = [KnownState]
```

## Shopping List for Functions

Overall, we want to generate simpler strategies. But what do we mean by "simple"? Well, we can define complexity as the number of leaves of an expression tree:

```haskell
complexity :: RP -> Int
```

We might also want to be able to check what happens with small changes to individual parameters. To do this, we return a vector of slightly modified values where each parameter is a dimension.

```haskell
getDeltas :: Ratio -> RP -> [RP]
```

We also want to be able to apply perturbations to an `RP`:

```haskell
applyDeltas :: [Ratio Int] -> RP -> RP
```

Using these two techniques we can find a local maximum by something akin to Euler's method for differential equations.

We also want to be able to switch between constants and parameters that make sense given context easily. For this, we can provide a typical set of `KnownState`s that can be used to find some parameter that would be similar and substitute it in. For simplicity, we'll only change one at a time.

```haskell
constantToParameter :: HistoricalStates -> RP -> StdGen -> (RP, StdGen)
```

This is a little out of order, but simplification should work in a similar manner, replacing complex sections of tree with constants or parameters.

```haskell
simplify :: HistoricalStates -> RP -> StdGen -> (RP, StdGen)
```

In fact, `simplify` can be seen as a more general form of `constantToParameter`; we can simply require some threshold determining whether or not a parameter or constant is "close enough" to be considered the same as a tree or not.

```haskell
data GeneratorParameters = GeneratorParameters {
    closeEnoughThreshold :: Ratio Int
}

simplify :: GeneratorParameters -> HistoricalStates -> RP -> StdGen -> (RP, StdGen)
```

Finally, we want the ability to complicate a given expression tree.

This can be accomplished most atomically by defining a function `complicate` that is allowed to make structural but non-effective changes, e.g.,
 - `x -> 1 * x, x / 1, 0 + x, x - 0`
 - `x -> if <some bool expression> then x else x`
 - `b -> b && True, b || False`

This will accept an integer argument of how much to complicate its tree.

```haskell
complicate :: Int -> RP -> StdGen -> (RP, StdGen)
```

Now, in mentioning everything above, glossed over some things. For example, most parameters accept some sort of argument, so generating them requires said argument. Additionally, `<some bool expression>` will need to actually be filled in in the redundant branch seen above. So we need some way to randomly generate parts of a tree

```haskell
randomlyGenerate :: StdGen -> (RP, StdGen)
```

In fact, this function is provided by the `Random` typeclass, so we can instead say

```haskell
instance Random RP where
    ...
```

## An `Expr` type class

OK, so if you were looking carefully, that last type didn't make much sense. I said we needed to be able to generate random elements of various types, but the definition I gave was specifically for `RP`. In fact, all the definitions I gave apply equally well to any of the various `Expr*` types.

Let's therefore collate all these function definitions into a single typeclass:

```haskell
class (Random a) => Expr a where
    complexity :: a -> Int
    getDeltas :: Ratio Int -> a -> [a]
    applyDeltas :: [Ratio Int] -> a -> a
    constantToParameter :: HistoricalStates -> a -> StdGen -> (a, StdGen)
    simplify :: HistoricalStates -> a -> StdGen -> (a, StdGen)
    complicate :: Int -> a -> StdGen -> (RP, StdGen)
```

OK, so that''s it for now. I'll cover implementation tomorrow after I typecheck this mess!

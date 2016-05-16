preprocess:
    pass ../_scripts/codetosources.py
    replace "haskellcomment" -> "haskell"
    replace "<!--_-->" -> ""
---
layout: post
title: A Differential Approach to Erosion
comments: True
---

dump: haskell as hs

<!--
```haskell
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, FlexibleInstances, FunctionalDependencies, DeriveFunctor #-}

import Data.Vector.Mutable
--import Control.Monad.ST.Lazy

```
-->

I have covered erosion [in the past](/2016/04/23/The-Autogeneration-Of-Landscapes/) with a single drop model. However, I thought a more hollistic model would be helpful.

## The variables

We can define a function of two variables representing the elevation of the land at a given (x, y) location. \\(E :: \mathbb R^2 \to \mathbb R\\). We can also define the depth of the water at any given point. \\(D :: \mathbb R^2 \to \mathbb R\\). We can also define the velocity of the water field to be a vector at every point. \\(\vec v :: \mathbb R^2 \to \mathbb R^2\\).

## The equations

We can derive the following relations.

 1. The velocity of the water at any point can be considered proportional to the gradient of the total water level, in a downward direction:
        \\[\vec v = - k_{\text{steep}} \vec\nabla (E + D)\\]
 2. The level of erosion is proportional to the knetic energy of the water at that point, that is its depth multiplied by the square of its velocity:
        \\[\frac{\text d E}{\text d t} = -k_{\text{erosion}} D \\|\vec v\\|^2\\]
 3. The change in the water level over the water level is proportional to the divergence of the velocity field (this is the geometric interpretation of divergence).
        \\[\frac{\text d D}{\text d t} = -D\nabla \cdot \vec v\\]

## A basic library for two dimensional derivatives

OK, so this is to some extent an introduction to the ST monad in Haskell. The ST Monad allows us to work with mutable state and arrays in Haskell. In this case, it simplifies matters somewhat to use an interface rather than a concrete implementation.

We have the data:

```haskell

data Params a = Params {
        steep :: a,
        erosion :: a
    }

data Arrays m a = Arrays {
        land :: m a,
        water :: m a
    }

```

We can also define a generic vector class as well as one that is specifically targeted at functions from `R2`.

```haskell
class Halvable a where
    half :: a
    negativeOne :: a

class (Halvable a) => Vector u a where
    (.+) :: u a -> u a -> u a
    (.*) :: a -> u a -> u a
    (*.*) :: u a -> u a -> u a

data Direction = L | R | U | D deriving (Show)

class (Vector v x) => VectorField f v x | f -> v, v -> f where
    first :: f x -> v x
    second :: f x -> v x
    cons :: v x -> v x -> f x
    shift :: Direction -> v x ->  v x
    (..*) :: x -> f x -> f x


data R2 a = (:.:) a a
```

We can then define the time derivatives of \\(E\\) and \\(D\\).

```haskell

dE :: forall f m a. (VectorField f m a) => Params a -> Arrays m a -> m a
dE k arrays = negativeOne .* (erosion k .* dot v v)
    where
    v = velocity k arrays

dD :: forall f m a. (VectorField f m a) => Params a -> Arrays m a -> m a
dD k arrays = nDivV *.* d
    where
    d = water arrays
    v = velocity k arrays
    divV = diverge v
    nDivV = negativeOne .* divV

velocity :: (VectorField f m a) => Params a -> Arrays m a -> f a
velocity k arrays = negativeOne ..* (steep k ..* grad w)
    where
    w = land arrays .+ water arrays

dot :: (VectorField f u x) => f x -> f x -> u x
dot u v = (first u *.* first v) .+ (second u *.* second v)

```

With the helper functions

```haskell

diverge :: (VectorField f m a) => f a -> m a
diverge field = dx (first field) .+ dy (second field)

grad :: (VectorField f m a) => m a -> f a
grad v = cons (dx v) (dy v)

dx, dy :: (VectorField f m a) => m a -> m a
dx = diff (L, R)
dy = diff (U, D)

diff :: forall f m a. (VectorField f m a) => (Direction, Direction) -> m a -> m a
diff (a, b) v = half .* (shift b v .- shift a v)

(.-) :: (Vector m a) => m a -> m a -> m a
u .- v = u .+ (negativeOne .* v)

data Matrix s a = MVector s (MVector s a)

```

So now we can implement a high-level symbolic implementation of `VectorField`.

```haskell
data Numbers = KSteep | KErode | Half | NegativeOne deriving (Show)

instance Halvable Numbers where
    half = Half
    negativeOne = NegativeOne

data Atom = Elev | Depth deriving (Show)

data Symbolic a = Atom Shift Scale Atom
    | Add [Symbolic a]
    | PointwiseMult (Symbolic a) (Symbolic a)
        deriving (Show, Functor)

data Symbolic2 a = Cons (Symbolic a) (Symbolic a) deriving (Show)

instance Vector Symbolic Numbers where
    (Add xs) .+ (Add ys) = Add $ xs ++ ys
    (Add xs) .+ y = Add $ y : xs
    x .+ (Add ys) = Add $ x : ys
    x .+ y = Add [x, y]
    k .* (Atom s ss atom) = Atom s (scale k ss) atom
    k .* Add us = Add $ fmap (k .*) us
    k .* PointwiseMult x y = PointwiseMult (k .* x) y
    (*.*) = PointwiseMult

instance VectorField Symbolic2 Symbolic Numbers where
    first (Cons x _) = x
    second (Cons _ y) = y
    cons = Cons
    shift s (Atom ss scale atom) = Atom (shifted s ss) scale atom
    shift s (Add us) = Add $ fmap (shift s) us
    shift s (PointwiseMult a b) = PointwiseMult (shift s a) (shift s b)
    u ..* (Cons x y) = Cons (u .* x) (u .* y)

type Shift = (Int, Int)
type Scale = (Bool, Int, Int, Int)

scale :: Numbers -> Scale -> Scale
scale KSteep (u,a,b,c) = (u,a+1,b,c)
scale KErode (u,a,b,c) = (u,a,b+1,c)
scale Half (u,a,b,c) = (u,a,b,c+1)
scale NegativeOne (u,a,b,c) = (not u,a,b,c)

shifted :: Direction -> Shift -> Shift
shifted L (x, y) = (x-1, y)
shifted R (x, y) = (x+1, y)
shifted D (x, y) = (x, y-1)
shifted U (x, y) = (x, y+1)
```

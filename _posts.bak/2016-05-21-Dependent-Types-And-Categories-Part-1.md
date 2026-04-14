preprocess:
    replace "```coq" -> "```python"
---
layout: post
title: Dependent Types and Categories, Part 1
comments: True
---

So I've been trying to go through [_Category Theory for Scientists_](http://math.mit.edu/~dspivak/teaching/sp13/) by writing the functions as code. So I obviously decided to use Haskell. Unfortunately, I ran into a wall: Haskell is actually pretty bad at representing categories.

## Haskell is *a* Category

Now, at first this was incredibly surprising. Frankly, I had started learning about category theory because of its association with Haskell. Unfortunately, I didn't realize that Haskell isn't category theory, it's a category.

For instance, look at this definition of a category in math:

<!--end excerpt-->

> A category is composed of
> 1. A set of objects \\(O\\)
> 2. A set of morphisms \\(M a b\\) for any two objects \\(a\\) and \\(b\\).
> 3. A morphism \\(\text{id} :: M o o\\) for every object \\(o :: O\\)
> 4. A composition function \\(\circ :: M b c \times M a b -> M b c\\)
> and follows the laws:
> 5. \\(\text{id} \circ f = f \circ \text{id} = f\\)
> 6. \\((f\circ g)\circ h = f \circ (g \circ h)\\)

Kinda a complicated description, but it boils down to the idea of arrows (2) between objects (1) where any two arrows can be combined head to tail (4) and there is a special arrow from every object to itself (3). Also, arrow combination head to tail is associative (6) and has as an identity that special arrow (5). It's basically a [generalized monoid](http://kavigupta.github.io/2016/05/07/Monoids-Bioids-And-Beyond/). Or if you prefer, a more general form for functions.

Here's the Haskell definition of a `Category`.

```haskell
class Category m where
    id :: forall o. m o o
    (.) :: forall a b c. m b c -> m a b -> m a c
```

What? That's like 2 things. What happened to the other 4?

Well, 5 and 6 are proofs. They can't be represented in Haskell any more than the proof that `a + b = b + a`. Haskell just isn't a proof language. But I can get over that. I generally like to write the laws anyway even if they are unenforcable:

```haskell
law1 :: (Category m, Eq (m a b)) => m a b -> Bool
law1 f = f . id == f && id . f == f
law2 :: (Category m, Eq (m a d)) => m a b -> m b c -> m c d -> Bool
law2 f g h = (f . g) . h == f . (g . h)
```

But what about 1 and 2? We can see our morphism set generator function, creatively named `m`. Unfortunately, `m` isn't a function, it's a type constructor. And we don't even have an `O`.

Well, the answer to this question is why Haskell is unsuitable. `m` is a constructor, which means it maps Haskell types to Haskell types: e.g., it is of "kind" (the type of a type) `* -> * -> *`. This means that `O` is fixed: it has to be the set of Haskell types.

So, that means that `Category m` defines a category over the set of Haskell types.

And therefore, Haskell is a category. And so it can't be used to represent all the categories.

## Dependent Types

A Dependent type is a type that is related to some value. In essence, it is the combination of values at the type and value level. This is all a bit abstract, so imagine that this was valid Haskell:

```haskell
data Nat = Z | S Nat

data Vector :: (n :: Nat) -> (a :: Type) -> Vector n a :: Type
data Vector = Nil :: Vector Z a
    | Cons :: a -> Vector n a -> Vector (S n) a
```

In this case, we can have functions with the following type signatures:

```haskell
(+) :: Nat -> Nat -> Nat
(++) :: Vector x a -> Vector y a -> Vector (x + y) a
```

Notice how the type signature encodes the idea that appended lists should have appended lengths.

Now in reality, Haskell [does have dependent types](https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell). But their syntax is ugly and they are limited in their capacity. Also, it would be nice to have a way to require proofs of our properties.


## The Coq proof assistant.

A language that supports these features is Coq. It's an interesting language that combines Prolog's declarative style with Haskell's functional style and concept of types. However, Coq also has complex dependent types as well as proofs in classes. For example, a category in Coq can be represented as follows.

First, we declare a class named `Category` of two parameters, a type `O` and a higher kinded type `M`.

```coq
Class Category {O : Type} {M : O -> O -> Type}
```

Notice that unlike Haskell's higher-kinded types, `M` is restricted; rather than being `* -> * -> *`, it has type `O -> O -> *` (`Type` is rather similar to `*`). Also notice that `O` is used as a type of a type in `M`: in Coq, there are an infinite number of types: types, kinds, types of kinds, types of types of kinds, etc., which are all represented by `Type` to avoid unecessary verbosity. In reality. `O` doesn't have to be a kind, it can be `nat` or something: types can include values.

We then specify the methods

```coq
        (id : forall x : O, M x x)
        (comp : forall a b c : O, M b c -> M a b -> M a c)
            : Prop
```

note the similarity between these declarations and the Haskell equivalents. Note that Coq doesn't allow naming functions with symbols so we can't define `comp` as `.`, but we can define a new "syntax" in a much more flexible manner later. Also note that `a, b, c` rather than just being implicitly members of `Type` like they are in Haskell, are in fact members of the restricted type `O`.

Also, the `Prop` at the end means that a class is Coq is in fact a proposition, or law. The next section is a list of laws.

```coq
    := Build_Category {
        comp_assoc : forall (a b c d : O) (x : M a b) (y : M b c) (z : M c d), (comp a c d z (comp a b c y x) =  comp a b d (comp b c d z y) x);
        id_left : forall (a b : O) (f : M a b), comp a b b (id b) f = f;
        id_right : forall (a b : O) (f : M a b), comp a a b f (id a) = f
    }.
```

It starts with the name of the set of laws. Theoretically, we could have multiple alternative sets of laws, any of which would work. In this case, there is only one.

In any case, we have the same three laws. However, there is one important distance. In Haskell, for example, `id 2` makes sense because of implicit typing. However the equivalent Coq notation would be `id Int 2`, we need to pass in the type to the function to specify which `id` we mean: in essence `id` is a function from objects to morphisms, as represented by `forall x, M x x` which is equivalent to `(x : O) -> M x x`. This leads to a little more visual cruft in the laws. However, the fact that you *can* represent laws as such is probably more important.

In Haskell, we implement the standard category as follows:

```haskell
instance Category (->) where
    id x = x
    (f . g) x = f (g x)
```

Of course, we never actually prove any of the laws. In Coq, we don't get away so easily. First, we have to define function composition with explicit types (Coq can be implicit too if you're willing to give up some flexibility):

```coq
Definition comp_fn (A B C : Type) (f : arrow B C) (g : arrow A B) : arrow A C
    := compose f g.
```

And then we can instance the category class:

```coq
Instance FunCat : Category (fun (X : Type) (x : X) => x) comp_fn.
    split.
        trivial.
        trivial.
        trivial.
Qed.
```

We use the `FunCat` constructor with the arguments of the identity function, here implemented as a lambda from two variables, `X : Type` and `(x : X)` to the result `x`, and our earlier defined `comp_fn`. (We could have used `comp_fn` inline, but it would have been a little too complicated).

We then need to prove all the laws. First, we split the "goal" of proving all three into the three "subgoals". Since all the laws basically reduce to `x = x` with a little simplification, all of them can be solved by the `trivial` strategy.

And there you have it, a proof that types and functions form a category, as well as a more general framework that works in other situations! The code can be found [here](https://github.com/kavigupta/ct4s-examples).
---
layout: post
title: Nontrivial Elements of Trivial Types
comments: True
---


<!--
```haskell
{-# LANGUAGE RankNTypes #-}
import Prelude hiding (undefined, error, succ)
```
-->
We know that types can be used to encode how to make useful data structures.

For example, a vehicle can be encoded as:

```haskell
data Vehicle = Car { vin :: Integer, licensePlate :: String }
        | Bike
```

In this case, valid values include cars with VINs and LPNs, or Bikes, which have neither of these data points.

However, some data types can seem far less useful.

<!--end excerpt-->

## Elements of the Void type

The void type is defined as

```haskell
data Void
```

This type is defined by being non-constructible. However, we can try to construct a value such that

```haskell
void :: Void
void =
```

So, how do we fill that in? We can construct a value of type `Void` by using `void`.

```haskell
void :: Void
void = void
```

In Haskell, we can define this in two ways:

```haskell
void2, void3 :: Void
void2 = undefined
void3 = error "any string here"
```

In fact, Haskell defines in the standard prelude,

```haskell
undefined = undefined
error _ = undefined
```

Which makes the definitions for `void` and `void2` equivalent.

## What is `undefined`?

The definition of `undefined` is kind of the functional equivalent of `x = x + 1`: it has a mathematical meaning that is inconsistent with its programming meaning. However, mathematical meaning:

\\(x = x\\)

actually does apply to the Hindley Milner process, which will end up with this equation. What this means is that `undefined` can be of any type:

```haskell
undefined :: forall a. a
```

Interestingly, such a definition is impossible to make illegal in a Turing-Complete language like Haskell. It corresponds to any function that runs infinitely. In fact, predicting whether a function can return the value `undefined` is equivalent to the impossible [Halting Problem](https://en.wikipedia.org/wiki/Halting_problem).

## The properties of `undefined`

`undefined` has a few interesting properties. For example, any time `undefined` is evaluated or pattern-matched against, it makes the entire expression `undefined`. However, because Haskell is lazy, it can still be used. For example, these evaluate to undefined:

```haskell
un0 = undefined + 2
un1 = sin undefined
un2 = let f x = x * 2 in f undefined
un3 = head [undefined, 2, 3, 4]
un4 = undefined == 2 -- (==) is just a function
```

Note that the last statement is particularly important: we can't actually define `undefined` within Haskell, since any test on `undefined` produces `undefined`.

The following evaluate to a value that is undefined-less.

```haskell
const_ignores_arguments = const 2 undefined
head_only_requires_head = head [2, undefined, undefined]
len_of_value = length [undefined, undefined]
variable_binding_doesn't_discriminate = let f x = 2 in f undefined
```

The following examples contain `undefined` (e.g., if you try to print them, they will error out), but are not `undefined` in and of themselves -- there is a possible function that discriminates between the value and `undefined`. (A tuple of such functions is given to the right.)

```haskell
undefined_can_be_in_a_list = [undefined] -- (length, tail, \[x] -> 2)
a_tuple_of_undefined = (,) undefined undefined -- (\(x,x) -> 2)
list_containing_undefined = tail [2, undefined, 2, 4] -- (length, !! 1)
wrapper_type = return undefined :: Maybe a -- (\(Just x) -> 2)
evaluation_to_undefined_is_local = map (*2) [undefined, undefined] -- (length, tail . tail)
```

## The self-wrapping data type

```haskell
data Wrap = W Wrap
```

We have the potential values of

 1. `undefined` (of course)
 2. `W undefined`
 3. `W (W undefined)`
 4. `W (W (W undefined))`
 5. ...
 6. `let x = W x in x`

These are all distinct because we can define functions as so:

```haskell
data Nat = Z | S Nat
f :: Nat -> Wrap -> ()
f Z _ = ()
f (S n) (W x) = f n x
```

`f Z u` will return `()` for any `u` (because it doesn't actually examine it's argument). Any other second argument will lead to the evaluation of the inner `undefined` and will result in `undefined`.

`f (S Z) u` will return `()` for `W undefined`, `W (W undefined)`, etc.

`f (S (S Z)) u` will return `()` for `W (W undefined)`, `W (W (undefined))`, etc.

So in general, all of these are wrappers around `undefined` with a given number of wrappers, except for the last. However, if we unwrap this value, we get the same value. In other words,

```haskell
--last_value = W (W (W (W (W (W (W (W (W (W (W (W (W ...))))))))))))
last_value :: Wrap
last_value = W last_value
```

It would at first seem as if `f n last_value` is `()` for any non-undefined-containing `n`. However, `Nat` is itself a fairly interesting type.

## The Natural Numbers (to a bad approximation)

From the Peano axioms, we know that a set of natural numbers can be represented as `(zero, successor) :: (Nat, Nat -> Nat)`

```haskell
class Peano a where
    zero :: a
    succ :: a -> a

peano_struct :: (Peano a) => (a, a -> a)
peano_struct = (zero, succ)
```

The remaining Peano axioms are properties which are difficult to express in Haskell's type system.

In any case, the upshot is that the values of `(Peano a) => a` should be of the form:

```haskell
one, two, three, four, five :: (Peano a) => a
one = succ zero
two = succ one -- = succ (succ zero)
three = succ two -- = succ (succ (succ zero))
four = succ three -- = succ (succ (succ succ (zero)))
five = succ four -- = succ (succ (succ (succ succ (zero))))
```

And, for any arbitrary implementation of the `Peano` class, we should only have these values. However, Haskell can't actually enforce this restriction, leaving us with the values:

```haskell
alt_zero, alt_one, alt_two, alt_three :: (Peano a) => a
alt_zero = undefined
alt_one = succ alt_zero -- = succ undefined
alt_two = succ alt_one -- = succ (succ undefined)
alt_three = succ alt_two -- = succ (succ (succ undefined))
```

Which correspond more or less to our wrapper values. However, these don't solve our puzzle of the `f n last_value` which evaluates to undefined while `n` is undefined-free.

However, we have one more value

```haskell
omega :: (Peano a) => a
omega = succ omega
```

This corresponds to the equation \\(x = x + 1\\), which in the theory of ordinals has the solution \\(\omega\\), or the first trans-infinite ordinal.

This value structurally resembles `last_value :: Wrap`, which means that when `f` is called, it will step through the two of them in sync, never returning, and thus it is equivalent to `undefined`.

The trivial connection between `Peano` and `Nat` is given below.

```haskell
instance (Peano Nat) where
    zero = Z
    succ = S
```

## The values of the Peano Integers.

In Haskell, `Nat` is a very inefficient type, using unary. A more efficient type is `Integer`, which uses binary.

```haskell
instance (Peano Integer) where
    zero = 0
    succ = (+1)
```

Of course, the primary issue with using the `Integer` type to represent a natural number is that it can have negative values.

More relevantly, because `(+1)` maps `undefined`s to `undefined`s, we don't have any of our `alt_` values.

Finally, the definition of `omega` specified to `Integer` values implies the following declaration:

```haskell
omega_int :: Integer
omega_int = omega_int + 1
```

Which ends up being equivalent to `undefined` as well, since again `(+1)` is not a constructor that can allow for the creation of a structure.

## Conclusion

I hope that introduction to nonstandard elements was interesting. In case you want to test out these code samples (all of which are runnable), you can find the source code [here](/src/2016-04-29-Nontrivial-Elements-Of-Trivial-Types.hs).

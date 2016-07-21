---
layout: post
title: Counting Monoids
comments: True
---

## What's a Monoid?

A monoid is a set \\(M\\) with an identity element \\(e : M\\) and an operation \\(* : M \to M \to M\\), where we have the laws:

 - \\(x * e = x\\)
 - \\(e * x = x\\)
 - \\(x * (y * z) = (x * y) * z\\)

We write a monoid as a tuple \\((M, e, * )\\). Some example monoids (written in pseudo-Haskell) are

```haskell
(Int, 0, (+))
(Int, 1, (*))
(Bool, True, (||))
((), (), \x y -> ())
([a], [], (++))
```

## Counting Pseudomonoids

We define a pseudomonoid as a pair \\((e, * )\\) that doesn't necessarily follow any laws:

```haskell
type Pseudomonoid a = (a, (a, a) -> a)
```

Counting the number of Pseudomonoids is not particularly difficult. If we define \\(\|A\|\\) to be the number of elements of a given type, we know that

 - \\(\|(A, B)\| = \|A\|\|B\|\\)
 - \\(\|A \to B\| = \|B\| ^ {\|A\|}\\)

(The first one is a fairly standard result in combinatorics, the second one might require some work to prove. I'm going to leave both as an exercise. Yeah, I know, [proof by logic...](http://jwilson.coe.uga.edu/emt668/emat6680.f99/challen/proof/proof.html)).

In any case, using these facts, we can easily calculate

```haskell
| Pseudomonoid a | = a * a ^ (a * a) = a ^ (a^2 + 1)
```

## Counting Monoids (Try 1)

OK, so that was easy, and useless. Our goal is to count *monoids*, not some lawless structure resembling a monoid.

So, let's define some laws:

```haskell
monoidLaws :: forall a. (Enumerable a, Eq a) => Pseudomonoid a -> Bool
monoidLaws (e, val) = idLaws && assocLaw
    where
    idLaws = forAll $ \x -> x +++ e == x && e +++ x == x
    assocLaw = forAll $ \x y z -> x +++ (y +++ z) == (x +++ y) +++ z
    (+++) = curry val
```

where we define the helper functions:

```haskell
class ForAll b where
    forAll :: (Enumerable a) => (a -> b) -> Bool

instance ForAll Bool where
    forAll f = all f enumerate

instance (Enumerable a, ForAll b) => ForAll (a -> b) where
    forAll f = forAll $ \x -> forAll $ \y -> f x y
```

We can then define a list of monoids over a set:

```haskell
monoids :: (Eq a, FinitelyEnumerable a) => [Pseudomonoid a]
monoids = filter monoidLaws enumerate
```

We then define some test types representing finite numbers of elements (a little more systematic than `data Four = A | B | C | D`):

```haskell
type Z = Void
data S x = Z | S x deriving(Eq, Show)

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three

instance Enumerable Void where enumerate = []
instance FinitelyEnumerable Void where cardinality = 0
instance (Enumerable a) => Enumerable (S a) where enumerate = Z : (S <$> enumerate)
instance (FinitelyEnumerable a) => FinitelyEnumerable (S a) where
    cardinality = Tagged . (+1) . unTagged $ (cardinality :: Tagged a Integer)
```

And some testing functions:

```haskell
timeCount :: String -> Int -> IO ()
timeCount label value = do
    start <- getPOSIXTime
    putStrLn $ label ++ ": Counted " ++ " " ++ show value ++ " monoids"
    end <- getPOSIXTime
    putStrLn . ("Completed in " ++) . show $ end - start

firstTry :: IO ()
firstTry = do
    timeCount "1" . length $ (monoids :: [Pseudomonoid One])
    timeCount "2" . length $ (monoids :: [Pseudomonoid Two])
    timeCount "3" . length $ (monoids :: [Pseudomonoid Three])
    timeCount "4" . length $ (monoids :: [Pseudomonoid Four])
```

The result is the following output:

```
1: Counted  1 monoids
Completed in 0.000051s
2: Counted  4 monoids
Completed in 0.000062s
3: Counted  33 monoids
Completed in 0.017308s
<wait 2 minutes>
^C
```

OK, so why is this happening? Well, we know that the total number of Pseudomonoids is \\(a ^ {a ^2 + 1}\\), which is 1 for 1, 32 for 2, 59049 for 3, and *17179869184 for 4*! Since my computer can't easily search through 17 billion values testing each of 64 possible values (checking associativity), this takes a long amount of time.

## Identity Pseudomonoids

OK, so that's way too many monoids to go through. Let's try to see if we can eliminate some of them. The identity restriction seems to be an easy one to ensure, because it only contains one variable. We therefore define an identity pseudomonoid as a pseudomonoid \\(e, * \\) satisfying \\(a * e = e * a = a\\).

To see how the identity restriction affects the pseudomonoid, we can look at a "multiplication table" of the monoid.

<img src="/resources/2016-07-20/identity.png" />

Since we know the value of the function on green squares, if we know the value of the function on orange squares, we know the value of the function everywhere. We then have the definition `IdentityPsuedomonoid x = x * x ^ ((x-1)^2)`. Since subtracting one from a type isn't well-defined, we instead use the definition `IdentityPsuedomonoid (x + 1) = (x + 1) * (x + 1) ^ (x^2)`, which can be encoded in Haskell as follows:

```haskell
type family IdentityPsuedomonoid m :: *
type instance IdentityPsuedomonoid (S a) = (S a, (a, a) -> S a)
```

We can now define a function from IdentityPsuedomonoids to Psuedomonoids:

```haskell
extend :: forall a. (Enumerable a, Eq a) => IdentityPsuedomonoid (S a) -> Pseudomonoid (S a)
extend (e, op) = (e, op')
    where
    op' :: (S a, S a) -> S a
    op' (u, v)
        | u == e    = v
        | v == e    = u
        | otherwise = op (inject u, inject v)
    inject :: S a -> a
    inject = match (delete e enumerate) enumerate
    match :: forall u v. (Eq u, Eq v) => [u] -> [v] -> u -> v
    match [] _ _ = error "match: nonenumeration provided"
    match _ [] _ = error "match: not enough matchables"
    match (x:xs) (y:ys) v = if x == v then y else match xs ys v
```

Wow, that's ugly! We can see that a lot of the issue is the arbitrary choice of a monoid identity element. Since the choice of an identity element is arbitrary (monoids are still monoids if you permute the labels), we can simply fix it to a specific value (say Z), count the resulting monoids, and then multiply by the number of elements in the set.

A simpler data type can be used (since there is no need to specify an identity).

```haskell
type family IdentityPsuedomonoid' m :: *
type instance IdentityPsuedomonoid' (S a) = (a, a) -> S a
```

And extend is heavily simplified!

```haskell
extend' :: forall a. (Enumerable a, Eq a) => IdentityPsuedomonoid' (S a) -> Pseudomonoid (S a)
extend' op = (Z, op')
    where
    op' :: (S a, S a) -> S a
    op' (Z, y) = y
    op' (x, Z) = x
    op' (S x, S y) = op (x, y)
```

Finalizing the code (note the multiplicative factor in each case):

```haskell
monoids' :: (Eq a, FinitelyEnumerable a) => [Pseudomonoid (S a)]
monoids' = filter monoidLaws (map extend' enumerate)

secondTry :: IO ()
secondTry = do
    timeCount "1" . (*1) . length $ (monoids' :: [Pseudomonoid One])
    timeCount "2" . (*2) . length $ (monoids' :: [Pseudomonoid Two])
    timeCount "3" . (*3) . length $ (monoids' :: [Pseudomonoid Three])
    timeCount "4" . (*4) . length $ (monoids' :: [Pseudomonoid Four])
    timeCount "5" . (*5) . length $ (monoids' :: [Pseudomonoid Five])
```

And it works for 4! Unfortunately not as well for 5.

```
1: Counted  1 monoids
Completed in 0.000141s
2: Counted  4 monoids
Completed in 0.000047s
3: Counted  33 monoids
Completed in 0.001292s
4: Counted  624 monoids
Completed in 1.273906s
<wait 2 minutes>
^C
```

## A closed form solution, perhaps?

Well, there might be one, but I'm certainly not one to provide it. I didn't realize while writing this post that the counting the number of monoids over a set is an open problem! (Also, I posed the problem slightly differently, in the standard formulation, we count *isomorphic* monoids).

But in any case, I hope the trip down Haskell lane was fun!


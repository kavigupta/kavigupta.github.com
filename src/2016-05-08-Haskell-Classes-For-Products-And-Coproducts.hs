{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, FlexibleInstances #-}
import Prelude hiding (fst, snd)
import Data.Void

data IntPair = IntPair Int Int

data P a b = P a b

class Span s where
    fst :: s a b -> a
    snd :: s a b -> b

instance (Span (,)) where
    fst (x, _) = x
    snd (_, y) = y

instance (Span ((,,) a)) where
    fst (_, x, _) = x
    snd (_, _, y) = y

class (Span s) => Product s where
    pFactor :: (Span s') => s' a b -> s a b

lawProduct :: forall s s' a b. (Eq a, Eq b, Span s', Product s) => s' a b -> Bool
lawProduct val' = fst val == fst val' && snd val == snd val'
    where
    val :: s a b
    val = pFactor val'

instance Product (,) where
    pFactor val' = (fst val', snd val')

instance Product ((,,) ()) where
    pFactor val' = ((), fst val', snd val')

class Cospan s where
    left :: a -> s a b
    right :: b -> s a b

instance Cospan Either where
    left = Left
    right = Right

data TripleEither a b c = A a | B b | C c
    deriving (Show, Eq)

instance Cospan (TripleEither a) where
    left = B
    right = C

class (Cospan s) => Coproduct s where
    cpFactor :: (Cospan s') => s a b -> s' a b

lawCoproduct :: forall s s' a b. (Eq (s' a b), Cospan s', Coproduct s) => a -> b -> Bool
lawCoproduct a b = cpFactor lhsA == rhsA && cpFactor lhsB == rhsB
    where
    rhsA, rhsB :: s' a b
    rhsA = left a
    rhsB = right b
    lhsA, lhsB :: s a b
    lhsA = left a
    lhsB = right b

instance Coproduct Either where
    cpFactor (Left a) = left a
    cpFactor (Right a) = right a

instance Coproduct (TripleEither Void) where
    cpFactor (A v) = absurd v
    cpFactor (B b) = left b
    cpFactor (C c) = right c


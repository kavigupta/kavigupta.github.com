{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}
import Prelude hiding (fst, snd)
import Data.Void

data Diagram1 a = Diagram1 [a -> a]
data Diagram2 a b = Diagram2 [a -> a] [a -> b] [b -> a] [b -> b]
data Diagram3 a b c = Diagram3 [a -> a] [a -> b] [a -> c] [b -> a] [b -> b] [b -> c] [c -> a] [c -> b] [c -> c]

twoStarCons :: (a -> b) -> (b -> a) -> Diagram2 a b
twoStarCons f g = Diagram2 [id] [f] [g] [id]

class Cone1 n a | n -> a where
    phi1 :: n -> a

class Cone2 n a b | n -> a, n -> b where
    phi2_a :: n -> a
    phi2_b :: n -> b

class Cone3 n a b c | n -> a, n -> b, n -> c where
    phi3_a :: n -> a
    phi3_b :: n -> b
    phi3_c :: n -> c

cone1Law :: (Cone1 n a, Eq (a -> a)) => Diagram1 a -> Bool
cone1Law (Diagram1 a) = all (== id) a

cone2Law :: forall n a b. (Cone2 n a b, Eq a, Eq b, Eq (a -> a), Eq (b -> b)) => Diagram2 a b -> n -> Bool
cone2Law (Diagram2 aa ab ba bb) n = and [aasat, bbsat, absat, basat]
    where
    f === g = f n == g n
    aasat = all (== id) aa
    bbsat = all (== id) bb
    absat = all (=== phi2_b) $ map (. phi2_a) ab
    basat = all (=== phi2_a) $ map (. phi2_b) ba

-- You get the picture.


class Span s where
    fst :: s a b -> a
    snd :: s a b -> b

class Iso a b where
    cast :: a -> b
    cocast :: b -> a

isoLaw1 :: (Iso a b, Eq a, Eq b) => a -> b -> Bool
isoLaw1 x y = cast x == y && cocast y == x

class (Iso a b) => Twostar a b where

twoStar :: (Twostar a b) => Diagram2 a b
twoStar = twoStarCons cast cocast

instance Iso (Either a a) (a, Bool) where
    cast (Left x) = (x, True)
    cast (Right x) = (x, False)
    cocast (x, True) = Left x
    cocast (x, False) = Right x

newtype EithMay2Star a = EM2S a
newtype EithMay2Star' a = EM2S' a
instance Cone2 (EithMay2Star a) (Either a a) (a, Bool) where
    phi2_a (EM2S n) = Left n
    phi2_b (EM2S n) = (n, True)

instance Cone2 (EithMay2Star' a) (Either a a) (a, Bool) where
    phi2_a (EM2S' n) = Right n
    phi2_b (EM2S' n) = (n, False)

class (Cone1 n a) => Limit1 n a where
    cFactor1 :: (Cone1 n' a) => n' -> n

class (Cone2 n a b) => Limit2 n a b where
    cFactor2 :: (Cone2 n' a b) => n' -> n

class (Cone3 n a b c) => Limit3 n a b c where
    cFactor3 :: (Cone3 n' a b c) => n' -> n

data Limit2Star a b = L2S a b

instance Cone2 (Limit2Star a b) a b where
    phi2_a (L2S x _) = x
    phi2_b (L2S _ y) = y

instance Limit2 (Limit2Star a b) a b where
    cFactor2 other = L2S (phi2_a other) (phi2_b other)

pullbackCons :: (a -> c) -> (b -> c) -> Diagram3 a b c
pullbackCons f g = Diagram3 [id] [] [f] [] [id] [g] [] [] [id]

data Label x = L

class PullbackCone n a b c | n -> a, n -> b, n -> c where
    -- unfortunately have to have a label because otherwise
        -- Haskell doesn't know what we're talking about
    ac :: Label n -> a -> c
    bc :: Label n -> b -> c
    na :: n -> a
    nb :: n -> b
    cons :: (a -> c) -> (b -> c) -> (n -> a) -> (n -> b) -> n

nc, nc' :: forall n a b c. (PullbackCone n a b c) => n -> c
nc = ac (L :: Label n) . na
nc' = bc (L :: Label n) . nb

pullbackLaw :: (PullbackCone n a b c, Eq c) => n -> Bool
pullbackLaw n = nc n == nc' n

instance (PullbackCone n a b c) => Cone3 n a b c where
    phi3_a = na
    phi3_b = nb
    phi3_c = nc

pullback :: (PullbackCone n a b c) => Label n -> Diagram3 a b c
pullback label = pullbackCons (ac label) (bc label)

instance (PullbackCone n a b c) => Limit3 n a b c where
    cFactor3 other = cons ac bc na nb


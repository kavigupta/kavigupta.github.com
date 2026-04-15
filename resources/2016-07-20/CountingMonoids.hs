{-# LANGUAGE TypeOperators, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import System.Environment
import Data.Enumerable
import Data.Time.Clock.POSIX
import Data.Void
import Data.Tagged

instance (Enumerable a, Enumerable b, Show a, Show b) => Show (a -> b) where
    show f = ("generate " ++) . show . map (pair f) $ enumerate

data a :-> b = a :-> b deriving (Show, Eq)

pair :: (a -> b) -> a -> a :-> b
pair f a = a :-> f a

generate :: (Eq a) => [a :-> b] -> a -> b
generate [] _ = error "Partial function"
generate (x :-> y:xs) a = if a == x then y else generate xs a

type Pseudomonoid a = (a, (a, a) -> a)

class ForAll b where
    forAll :: (Enumerable a) => (a -> b) -> Bool

instance ForAll Bool where
    forAll f = all f enumerate

instance (Enumerable a, ForAll b) => ForAll (a -> b) where
    forAll f = forAll $ \x -> forAll $ \y -> f x y

monoidLaws :: forall a. (Enumerable a, Eq a) => Pseudomonoid a -> Bool
monoidLaws (e, val) = idLaws && assocLaw
    where
    idLaws = forAll $ \x -> x +++ e == x && e +++ x == x
    assocLaw = forAll $ \x y z -> x +++ (y +++ z) == (x +++ y) +++ z
    (+++) = curry val

monoids :: (Eq a, FinitelyEnumerable a) => [Pseudomonoid a]
monoids = filter monoidLaws enumerate

type Z = Void
data S x = Z | S x deriving(Eq, Show)

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three
type Five = S Four

instance Enumerable Void where enumerate = []
instance FinitelyEnumerable Void where cardinality = 0
instance (Enumerable a) => Enumerable (S a) where enumerate = Z : (S <$> enumerate)
instance (FinitelyEnumerable a) => FinitelyEnumerable (S a) where
    cardinality = Tagged . (+1) . unTagged $ (cardinality :: Tagged a Integer)

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

type family IdentityPsuedomonoid m :: *
type instance IdentityPsuedomonoid (S a) = (S a, (a, a) -> S a)

-- extend :: forall a. (Enumerable a, Eq a) => IdentityPsuedomonoid (S a) -> Pseudomonoid (S a)
-- extend (e, op) = (e, op')
--     where
--     op' :: (S a, S a) -> S a
--     op' (u, v)
--         | u == e    = v
--         | v == e    = u
--         | otherwise = op (inject u, inject v)
--     inject :: S a -> a
--     inject = match (delete e enumerate) enumerate
--     match :: forall u v. (Eq u, Eq v) => [u] -> [v] -> u -> v
--     match [] _ _ = error "match: nonenumeration provided"
--     match _ [] _ = error "match: not enough matchables"
--     match (x:xs) (y:ys) v = if x == v then y else match xs ys v

type family IdentityPsuedomonoid' m :: *
type instance IdentityPsuedomonoid' (S a) = (a, a) -> S a

extend' :: forall a. (Enumerable a, Eq a) => IdentityPsuedomonoid' (S a) -> Pseudomonoid (S a)
extend' op = (Z, op')
    where
    op' :: (S a, S a) -> S a
    op' (Z, y) = y
    op' (x, Z) = x
    op' (S x, S y) = op (x, y)

monoids' :: (Eq a, FinitelyEnumerable a) => [Pseudomonoid (S a)]
monoids' = filter monoidLaws (map extend' enumerate)

secondTry :: IO ()
secondTry = do
    timeCount "1" . (*1) . length $ (monoids' :: [Pseudomonoid One])
    timeCount "2" . (*2) . length $ (monoids' :: [Pseudomonoid Two])
    timeCount "3" . (*3) . length $ (monoids' :: [Pseudomonoid Three])
    timeCount "4" . (*4) . length $ (monoids' :: [Pseudomonoid Four])
    timeCount "5" . (*5) . length $ (monoids' :: [Pseudomonoid Five])

main :: IO ()
main = do
    [arg] <- getArgs
    case arg of
        "first" -> firstTry
        "second" -> secondTry
        _ -> error "Usage: countingmonoids [first|second]"

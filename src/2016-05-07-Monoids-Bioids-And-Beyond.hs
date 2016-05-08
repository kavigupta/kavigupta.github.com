{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, FunctionalDependencies #-}
import Prelude hiding(Monoid, (.), (++), id, (^^))

class Monoid a where
    m_id :: a
    (++) :: a -> a -> a

monoidLaw1, monoidLaw2 :: (Monoid a, Eq a) => a -> Bool
monoidLaw1 x = x ++ m_id == x
monoidLaw2 x = m_id ++ x == x

monoidLaw3 :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidLaw3 x y z = (x ++ y) ++ z == x ++ (y ++ z)

class (Morphism f) where
    id :: f x x
    (.) :: f b c -> f a b -> f a c

morphismLaw1, morphismLaw2 :: (Morphism f, Eq (f a b)) => f a b -> Bool
morphismLaw1 f = f . id == f
morphismLaw2 f = id . f == f

morphismLaw3 :: (Morphism f, Eq (f a d)) => f c d -> f b c -> f a b -> Bool
morphismLaw3 f g h = (f . g) . h == f . (g . h)

instance (Morphism (->)) where
    id x = x
    (f . g) x = f (g x)

data Endo morph set = Endo (morph set set)

instance (Morphism morph) => (Monoid (Endo morph set)) where
    m_id = Endo id
    Endo f ++ Endo g = Endo (f . g)

class (Monoid aa, Monoid bb) => Bioid aa bb ab ba
        | aa -> ab, aa -> ba, bb -> ab, bb -> ba, ab -> aa, ab -> bb, ba -> aa, ba -> bb, aa -> bb, bb -> aa, ab -> ba, ba -> ab where
    id_a :: aa
    id_b :: bb
    (%%) :: ab -> ba -> aa
    (^^) :: ba -> ab -> bb

    (#>) :: ab -> aa -> ab
    (<#) :: aa -> ba -> ba
    ($>) :: bb -> ab -> ab
    (<$) :: ba -> bb -> ba


module Coor(Coor((:$:)), (*$)) where

import Data.Monoid

infixr 1 :$:
infixr 5 *$
data Coor x = x :$: x
    deriving (Eq, Ord, Show)

(*$) :: (Num a) => a -> Coor a -> Coor a
(*$) k = fmap (*k)

instance Functor Coor where
    fmap f (x:$:y) = f x:$:f y

instance (Num a) => Monoid (Coor a) where
    mempty = 0:$:0
    (x:$:y) `mappend` (w:$:z) = x+w:$:y+z

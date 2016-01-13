module Coor(Coor((:$:)), (*$), ($-), sqMag, squareSpiral) where

import Data.Monoid

infixr 1 :$:
infixr 5 *$
data Coor x = x :$: x
    deriving (Eq, Ord, Show)

(*$) :: (Num a) => a -> Coor a -> Coor a
(*$) k = fmap (*k)

($-) :: (Num a) => Coor a -> Coor a -> Coor a
(x:$:y) $- (w:$:z) = x-w:$:y-z

instance Functor Coor where
    fmap f (x:$:y) = f x:$:f y

instance (Num a) => Monoid (Coor a) where
    mempty = 0:$:0
    (x:$:y) `mappend` (w:$:z) = x+w:$:y+z

sqMag :: (Num a) => Coor a -> a
sqMag (x:$:y) = x * x + y * y

squareSpiral :: Int -> Int -> Int -> Coor Int -> [Coor Int]
squareSpiral limit w h (x:$:y) = concat $ takeWhile (not . null) $ [x:$:y] : map square [1..limit]
    where
    square :: Int -> [Coor Int]
    square n = right ++ top ++ left ++ bottom
        where
        left = let x' = x - n in
                    if x' < 0 then []
                        else map (x':$:) [-n + y..n + y]
        right = let x' = x + n in
                    if x' >= w then []
                        else map (x':$:) [-n + y..n + y]
        bottom = let y' = y - n in
                    if y' < 0 then []
                        else map (:$:y') [-n + x..n + x]
        top = let y' = y + n in
                    if y' >= h then []
                        else map (:$:y') [-n + x..n + x]

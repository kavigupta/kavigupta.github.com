module Transformer where

import Coor
import StarIdentification

import Data.Monoid

import Numeric.LinearAlgebra

import Statistics.Regression

import Control.Arrow

data Transformer = Transformer { a :: Double, b :: Double, c :: Double, d :: Double, e :: Double, f :: Double}

act :: Transformer -> Coor Double -> Coor Double
act stats (x:$:y) = x * a stats + y * b stats + c stats :$: x * d stats + y * e stats + f stats

{-
    Correlations between [(x1, y1, b1), (x2, y2, b2)]
    Make that  [(x1, y1, b, dx, dy)] where b = (b1 + b2) / 2; dx = x2 - x1; dy = y2 - y1
-}
getTransformation :: [(Star, Star)] -> Transformer
getTransformation starps = getTransform . (getLSRP . fst &&& getLSRP . snd) $ endingStats
    where
    sps = map getStarPair starps
    endingStats = foldr applyStarPair startingStats sps



instance Monoid Transformer where
    mempty
        = Transformer
            1 0 0
            0 1 0
    {-
    a1 b1 c1       a2 b2 c2
    d1 e1 f1   *   d2 e2 f2
    0  0  1        0  0  1

    -}
    (Transformer a1 b1 c1 d1 e1 f1) `mappend` (Transformer a2 b2 c2 d2 e2 f2)
        = Transformer
            (a1 * a2 + b1 * d2) (a1 * b2 + b1 * e2) (a1 * c2 + b1 * f2 + c1)
            (d1 * a2 + e1 * d2) (d1 * b2 + e1 * e2) (d1 * b2 + e1 * e2 + f1)

type Data3d = (Double, Double, Double, Double)

getStarPair :: (Star, Star) -> (Data3d, Data3d)
getStarPair (Star (x1:$:y1) b1, Star (x2:$:y2) b2)
        = ((x1, y1, b, dx), (x1, y1, b, dy))
    where
    b = (b1 + b2) / 2
    dx = x2 - x1
    dy = y2 - y1

data LSRPStats = LSRPStats {
    sumX :: Double,
    sumY :: Double,
    sumZ :: Double,
    sumXX :: Double,
    sumXY :: Double,
    sumXZ :: Double,
    sumYY :: Double,
    sumYZ :: Double,
    sumWeight :: Double
}

startingStats :: LSRPStats
startingStats = LSRPStats 0 0 0 0 0 0 0 0 0

applyStarPair :: (LSRPStats, LSRPStats) -> (Data3d, Data3d) -> (LSRPStats, LSRPStats)
applyStarPair (u, v) (w, z) = (applyDataPoint u w, applyDataPoint v z)

applyDataPoint :: LSRPStats -> Data3d -> LSRPStats
applyDataPoint
    (LSRPStats sx sy sz sxx sxy sxz syy syz sw)
    (x,y,b,z)
        = LSRPStats
            (sx + b * x)
            (sy + b * y)
            (sz + b * z)
            (sxx + b * x * x)
            (sxy + b * x * y)
            (syy + b * y * y)
            (sxz + b * x * z)
            (syz + b * y * z)
            (sw + b)

data PlaneFunction = PlaneFunction Double Double Double

data Point3d = Point3d Double Double Double

fromGeometricForm :: Matrix Double -> Point3d -> PlaneFunction
fromGeometricForm n (Point3d x0 y0 z0)
        = PlaneFunction (-alph/gam) (-bet/gam) (x0 * alph/gam + y0 * bet/gam + z0)
    where
    [alph, bet, gam] = fromList n
    -- <alph, bet, gam> <.> <x-x0, y-y0, z-z0> = 0
    -- <alph, bet> <.> <x-x0, y-y0> = - gam (z - z0)
    -- z    = z0 - (alph * (x - x0) + beta * (y - y0)) / gam
    --      = -alph/gam * x - bet/gam * y + x0 * alph/gam + y0 * bet/gam + z0

getLSRP :: LSRPStats -> PlaneFunction
getLSRP (LSRPStats sx sy sz sxx sxy sxz syy syz sw)
        = fromGeometricForm n centroid
    where
    u = matrix 3 [sx, sxy, sx, sxy, syy, sy, sx, sy, sw]
    v = fromList [sxz, syz, sz]
    -- u n = v
    -- n = u^-1 v
    n = inv u <> v
    centroid = Point3d (sx / sw) (sy / sw) (sz / sw)

getTransform :: (LSRPStats, LSRPStats) -> Transformer
getTransform (u, v) = Transformer a b c d e f
    where
    (PlaneFunction a b c) = getLSRP u
    (PlaneFunction d e f) = getLSRP v

xOf, yOf :: Coor a -> a
xOf (x:$:_) = x
yOf (_:$:y) = y

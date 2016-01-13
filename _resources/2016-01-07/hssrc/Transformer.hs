module Regresser where

import Coor
import StarIdentification

import Statistics.Regression

import Data.Vector.Unboxed hiding (map)

import Control.Arrow

data Transformer = Transformer { a :: Double, b :: Double, c :: Double, d :: Double, e :: Double, f :: Double}

apply :: Transformer -> Coor Double -> Coor Double
apply stats (x:$:y) = x * a stats + y * b stats + c stats :$: x * d stats + y * e stats + f stats

getTransformation :: [(Star, Star)] -> (Transformer, Double, Double)
getTransformation starps = (Transformer {a=a,b=b,c=c,d=d,e=e,f=f}, r2x, r2y)
    where
    ([a, b, c], r2x) = regress xs'
    ([d, e, f], r2y) = regress ys'
    predictors = map (\c -> extract (c.fst)) [xOf, yOf]
    xs' = extract $ xOf.snd
    ys' = extract $ yOf.snd
    extract :: ((Coor Double, Coor Double) -> Double) -> Vector Double
    extract f = fromList $ map (f . (location . fst &&& location . snd)) starps
    regress :: Vector Double -> ([Double], Double)
    regress response = toList . fst &&& snd $ olsRegress predictors response

xOf, yOf :: Coor a -> a
xOf (x:$:_) = x
yOf (_:$:y) = y

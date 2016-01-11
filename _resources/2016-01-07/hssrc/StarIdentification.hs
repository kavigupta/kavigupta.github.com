module StarIdentification where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow
import Data.Monoid

import Control.Monad.ST.Safe

import BitMatrix

import Coor

import Codec.Picture

data Star = Star { location :: Coor Double, brightness :: Double } deriving (Eq, Show)

data IdentifiedStars = IdentifiedStars (M.Map (Coor Int) Star)

identifyStars :: [S.Set (Coor Int)] -> Image PixelRGB8 -> IdentifiedStars
identifyStars clusters img = IdentifiedStars $ foldr (.) id (zipWith M.insert starCentInt stars) (M.fromList [])
    where
    stars = map (star img) clusters
    starCentInt = map (fmap round . location) stars

star :: Image PixelRGB8 -> S.Set (Coor Int) -> Star
star img coorset = Star {location=averageloc, brightness=bright}
    where
    brightlocs = map (lightness . extract img &&& fmap fromIntegral) . S.toList $ coorset
    bright =  sum (map fst brightlocs)
    averageloc = (1 / bright) *$ mconcat (map (uncurry (*$)) brightlocs)

allClusters :: (Fractional a, Ord a) => a -> Image PixelRGB8 -> [S.Set (Coor Int)]
allClusters thresh img
        = runST $ do
            used <- newBitMatrix (imageWidth img) (imageHeight img)
            partialIdentify
                [x :$: y | x <- [0..imageWidth img - 1], y <- [0..imageWidth img - 1]]
                used
                []
    where
    partialIdentify ::
            [Coor Int] ->
            BitMatrix s ->
            [S.Set (Coor Int)] ->
            ST s [S.Set (Coor Int)]
    partialIdentify [] _ clusters = return clusters
    partialIdentify (current:rest) used clusters = do
        hasBeenUsed <- at used current
        if hasBeenUsed then
            partialIdentify rest used clusters
        else do
            let {
                cluster = getCluster (whiterThan thresh) img current;
                clusters' = if S.null cluster then clusters else cluster:clusters
            } in do
                setAll used True $ S.toList cluster
                partialIdentify rest used clusters'

whiterThan :: (Fractional a, Ord a) => a -> PixelRGB8 -> Bool
whiterThan thresh pixel = lightness pixel > thresh

lightness :: (Fractional a, Ord a) => PixelRGB8 -> a
lightness (PixelRGB8 r g b) = (fromIntegral r + fromIntegral g + fromIntegral b) / (3 * 256)

getCluster :: (Pixel a) => (a -> Bool) -> Image a -> Coor Int -> S.Set (Coor Int)
getCluster filt img loc0
        = partialCluster [loc0] (S.fromList [])
    where
    notInRange :: Coor Int -> Bool
    notInRange (x :$: y) = x < 0 || y < 0 || x >= imageWidth img || y >= imageHeight img
    partialCluster :: [Coor Int] -> S.Set (Coor Int) -> S.Set (Coor Int)
    partialCluster [] seen = seen
    partialCluster (loc:rest) seen
        | notInRange loc                = partialCluster rest seen
        | not $ filt (extract img loc)  = partialCluster rest seen
        | loc `S.member` seen           = partialCluster rest seen
        | otherwise                     = seen''
            where
            seen' = S.insert loc seen
            seen'' = partialCluster (offsetsOf loc ++ rest) seen'

offsetsOf :: (Num a) => Coor a -> [Coor a]
offsetsOf (x:$:y) = [x:$:y-1, x:$:y+1, x-1:$:y, x+1:$:y]

extract :: (Pixel a) => Image a -> Coor Int -> a
extract img (x:$:y) = pixelAt img x y

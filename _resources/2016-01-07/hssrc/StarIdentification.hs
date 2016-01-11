module StarIdentification where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow
import Data.Monoid

import Codec.Picture

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

data Star = Star { location :: Coor Double, brightness :: Double } deriving (Eq, Show)

data IdentifiedStars = IdentifiedStars (M.Map (Coor Int) Star)

identifyStars :: [S.Set (Coor Int)] -> Image PixelRGB8 -> IdentifiedStars
identifyStars clusters img = IdentifiedStars $ foldr (.) id (zipWith M.insert starCentInt stars) (M.fromList [])
    where
    stars = map (star img) clusters
    starCentInt = map (fmap round . location) stars

star :: Image PixelRGB8 -> S.Set (Coor Int) -> Star
star img set = Star {location=averageloc, brightness=bright}
    where
    brightlocs = map (lightness . extract img &&& fmap fromIntegral) . S.toList $ set
    bright =  sum (map fst brightlocs)
    averageloc = (1 / bright) *$ mconcat (map (uncurry (*$)) brightlocs)

allClusters :: (Fractional a, Ord a) => a -> Image PixelRGB8 -> [S.Set (Coor Int)]
allClusters thresh img
        = partialIdentify
            [x :$: y | x <- [0..imageWidth img - 1], y <- [0..imageWidth img - 1]]
            (S.fromList [])
            []
    where
    partialIdentify ::
            [Coor Int] ->
            S.Set (Coor Int) ->
            [S.Set (Coor Int)] ->
            [S.Set (Coor Int)]
    partialIdentify [] _ clusters = clusters
    partialIdentify (current:rest) used clusters
        | current `S.member` used
            = partialIdentify rest used clusters
        | otherwise
            = let {
                cluster = getCluster (whiterThan thresh) img current;
                used' = S.union used cluster;
                clusters' = if S.null cluster then clusters else cluster:clusters
            } in partialIdentify rest used' clusters'

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

module Aligner where

import qualified Data.Map as M

import Control.Arrow((&&&))
import Data.List
import Data.Monoid
import Data.Function

import StarIdentification
import Coor
import Tools

import Debug.Trace

(!) :: (Ord k) => M.Map k v -> k -> v
(!) = (M.!)

align :: Double -> Int -> IdentifiedStars -> IdentifiedStars -> [(Star, Star)]
align diff radius this other = trace ("OFFSET = " ++ show offset) $ allMatches diff radius this offset other
    where
    offset = getOffset diff radius this other

allMatches :: Double -> Int -> IdentifiedStars -> Coor Int -> IdentifiedStars -> [(Star, Star)]
allMatches diff radius (IdentifiedStars _ _ mapthis) off other
        = rmNothing $ map (join . (snd &&& findMatch diff radius other off)) $ M.toList mapthis
    where
    join (_, Nothing) = Nothing
    join (x, Just y) = Just (x, y)

findMatch :: Double -> Int -> IdentifiedStars -> Coor Int -> (Coor Int, Star) -> Maybe Star
findMatch diff _ (IdentifiedStars w h other) off (loc0, Star {brightness=bright0, location = _})
        = case relevantStars of
            [] -> Nothing
            (x:_) -> Just x
    where
    refine_radius = case off of (x:$:y) -> abs x + abs y
    starSpiral = map (flip M.lookup other) $ sqspr
    sqspr = squareSpiral (refine_radius `div` 4) w h (loc0 `mappend` off)
    relevantStars = rmNothing $ filter similar starSpiral
    similar (Just (Star _ bright)) = abs (bright - bright0) / bright0 < diff
    similar Nothing = False


getOffset :: Double -> Int -> IdentifiedStars -> IdentifiedStars -> Coor Int
getOffset diff radius (IdentifiedStars w h smap) (IdentifiedStars _ _ other)
        = case filterOffs diff other (potentialOffs, rest) of
            Nothing -> error (show sortedStars)
            (Just x) -> x
    where
    sortedStars :: [(Coor Int, Double)]
    sortedStars = sortBy (flip (compare `on` snd)) $ map (fst &&& brightness . snd) $ M.toList smap
    rest :: [(Coor Int, Double)]
    ((x0:$:y0, bright0): rest) = sortedStars
    potentialOffs = similarStars
            diff
            [x-x0:$:y-y0 |
                x <- [(max 0 (x0 - radius))..(min (w - 1) (x0 + radius))],
                y <- [(max 0 (y0 - radius))..(min (h-1) (y0 + radius))]]
            other
            (x0:$:y0, bright0)

filterOffs :: Double -> (M.Map (Coor Int) Star) -> ([Coor Int], [(Coor Int, Double)]) -> Maybe (Coor Int)
filterOffs _    _     ([], _) = Nothing
filterOffs _    _     ([x], _) = Just x
filterOffs _ _ (xs, []) = justSelect xs
filterOffs diff other (xs, (loc,bright):rest)
        = maybe (justSelect xs) Just $ filterOffs diff other (simstr, rest)
    where
    simstr = similarStars diff xs other (loc, bright)

justSelect :: (Num a, Ord a) => [Coor a] -> Maybe (Coor a)
justSelect = Just . minimumBy (compare `on` sqMag)



similarStars :: Double -> [Coor Int] -> M.Map (Coor Int) Star -> (Coor Int, Double) -> [Coor Int]
similarStars diff offs stars (loc0, bright0)
        = map (($- loc0) . fst) $ filter similar $ collapse $ map (id &&& flip M.lookup stars) locs
    where
    locs = map (mappend loc0) offs
    similar (_, Star _ bright) = abs (bright - bright0) / bright0 < diff
    collapse [] = []
    collapse ((_, Nothing):rest) = collapse rest
    collapse ((y, Just x): rest) = (y, x):collapse rest

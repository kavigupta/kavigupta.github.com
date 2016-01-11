module Aligner where

import qualified Data.Map as M

import Control.Applicative

align :: Int -> IdentifiedStars -> IdentifiedStars -> [(Star, Star)]
align potentialDistance a b = undefined

similarStars :: Double -> [Coor Int] -> IdentifiedStars -> Coor Int -> [Star]
similarStars diff offs (IdentifiedStars stars) loc0
        = filter similar $ collapse $ map (flip lookup stars . mappend loc0) offs
    where
    (Star _ bright0) = stars ! 0
    similar loc = case stars ! loc of (Star _ bright) -> abs (bright - bright0) / bright0 < diff
    collapse [] = []
    collapse (Nothing:rest) = collapse rest
    collapse (Just x: rest) = x:collapse rest

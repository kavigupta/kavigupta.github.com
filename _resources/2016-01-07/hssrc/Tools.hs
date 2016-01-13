module Tools where

rmNothing :: [Maybe a] -> [a]
rmNothing [] = []
rmNothing (Nothing:xs) = rmNothing xs
rmNothing (Just y:xs) = y:rmNothing xs

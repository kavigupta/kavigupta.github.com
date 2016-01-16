module BitMatrix(BitMatrix, newBitMatrix, at, BitMatrix.set, setAll) where

import Prelude hiding (read, replicate)

import Data.Vector.Mutable
import Control.Monad.ST.Safe

import Coor

import Control.Applicative

data BitMatrix s = BitMatrix { width :: Int, height :: Int, contents :: MVector s (MVector s Bool) }

newBitMatrix :: Int -> Int -> ST s (BitMatrix s)
newBitMatrix w h = BitMatrix w h <$> replicateM w (replicate h False)

at :: BitMatrix s -> Coor Int -> ST s Bool
at bitm@(BitMatrix {contents=mat}) (x:$:y)
    | notInBounds bitm (x:$:y) = return False
    | otherwise = do
        column <- read mat x
        read column y

set :: BitMatrix s -> Bool -> Coor Int -> ST s ()
set bitm@(BitMatrix {contents=mat}) b (x:$:y)
    | notInBounds bitm (x:$:y) = return ()
    | otherwise = do
            column <- read mat x
            write column y b

setAll :: BitMatrix s -> Bool -> [Coor Int] -> ST s ()
setAll bmatrix b = mapM_ $ BitMatrix.set bmatrix b

notInBounds :: BitMatrix s -> Coor Int -> Bool
notInBounds (BitMatrix{width=w,height=h}) (x:$:y) = x < 0 || y < 0 || x >= w || y >= h

module Main(main, standard_main) where

import ImageManip

import StarIdentification
import Coor
import Data.Ratio

import Codec.Picture

inset :: (Int, Int, Int, Int)
inset = (900, 1200, 400, 400)

main :: IO ()
main = prof_main

standard_main :: IO ()
standard_main = do
    raws_ <- raws
    let clusters = map (allClusters ((5 :: Int)%10)) raws_
    --let idstars = map identifyStars clusters
    writeImage "original-cropped" $ crop inset (head raws_)
    writeImage "original-avgd" $ average $ map (crop inset) raws_
    putStrLn "Done with averaging. Identifying stars"
    let visualization n = visualizeStars (clusters !! n) $ empty (raws_ !! n)
    let vis0 = visualization 0
    writeImage "identified-stars" $ crop inset $ average [vis0, head raws_]
    putStrLn "Single identification of stars"
    writeImage "identified-stars-two"
        $ combine (crop inset (head raws_)) redBlueLayerer
        $ fmap (crop inset)
        $ vis0 :$: visualization 1
    putStrLn "Second identification of stars"
    return ()

prof_main :: IO ()
prof_main = do
    imgs <- raws
    let img = head imgs
    let cluster = allClusters (0.5 :: Double) img
    writeImage "temp" $ visualizeStars cluster $ empty img

redBlueLayerer :: Coor PixelRGB8 -> PixelRGB8
redBlueLayerer (a:$:b)
    = layerMonochrome
        (PixelRGB8 0 0 0)
        (PixelRGB8 0xFF 0 0)
        (PixelRGB8 0 0xFF 0)
        (PixelRGB8 0xFF 0xFF 0xFF)
        a
        b

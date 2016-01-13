{-# LANGUAGE CPP #-}
module Main(main, demo_main, prof_main) where

import ImageManip

import StarIdentification
import Aligner
import Coor

import Codec.Picture

inset :: (Int, Int, Int, Int)
#ifdef ENTIRE_IMAGE
inset = originalInset
#else
inset = (0, 0, 400, 400)
#endif

main :: IO ()
main = demo_main

demo_main :: IO ()
demo_main = do
    raws_ <- raws
    let clusters = map (allClusters 0x20) raws_
    let idstars = zipWith identifyStars clusters raws_
    --let idstars = map identifyStars clusters
    writeImage "original-cropped" $ crop inset (head raws_)
    writeImage "original-avgd" $ average $ map (crop inset) raws_
    putStrLn "Done with averaging. Identifying stars"
    let outlinedStars = map (\n -> visualizeStars (clusters !! n) $ empty (raws_ !! n)) [0..length raws_ - 1]
    let allId = average [head outlinedStars, head raws_]
    writeImage "all-identified-stars" $ allId
    writeImage "identified-stars" $ crop inset $ allId
    putStrLn "Single identification of stars"
    writeImage "identified-stars-two"
        $ combine (crop inset (head raws_)) redBlueLayerer
        $ fmap (crop inset)
        $ (head outlinedStars) :$: (outlinedStars !! 1)
    putStrLn "Second identification of stars"
    print $ idstars !! 0
    print $ idstars !! 1
    let alignments = align 0.7 50 (idstars !! 0) (idstars !! 1)
    print alignments
    let alignImg = (visualizeAlignments alignments) (raws_ !! 0) (raws_ !! 1)
    writeImage "alignments" alignImg
    return ()

prof_main :: IO ()
prof_main = do
    imgs <- raws
    let img = head imgs
    let cluster = allClusters (0x80) img
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

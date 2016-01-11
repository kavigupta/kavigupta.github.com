module ImageManip where

import System.Directory
import System.Exit

import Control.Monad
import Data.List
import GHC.Word

import qualified Data.Map as M
import qualified Data.Set as S

import Coor

import Codec.Picture
import Codec.Picture.Types

projectRoot :: FilePath
projectRoot = "../"

raws :: IO [Image PixelRGB8]
raws = do
    files <- getDirectoryContents $ projectRoot ++ "raw"
    let jpgs = sort
            . map ((projectRoot ++ "raw/") ++)
            . filter (".png" `isSuffixOf`)
            $ files
    maybeImages <- forM jpgs readPng
    images <- case sequence maybeImages of
        Left err -> putStrLn err >> exitFailure
        Right imgs -> return (map toRGB8 imgs)
    let uniqueWidth = nub $ map imageWidth images
    let uniqueHeight = nub $ map imageHeight images
    when (length uniqueWidth /= 1 || length uniqueHeight /= 1) $
        putStrLn ("Widths should be unique, but are "
            ++ show uniqueWidth
            ++ " and heights are "
            ++ show uniqueHeight)
                >> exitFailure
    putStrLn "Loaded images"
    return images

writeImage :: String -> Image PixelRGB8 -> IO ()
writeImage name = writePng ("../gen/" ++ name ++ ".png")

toRGB8 :: DynamicImage -> Image PixelRGB8
toRGB8 (ImageRGB8 x) = x
toRGB8 (ImageY8 _) = error "Y8"
toRGB8 (ImageY16 _) = error "Y16"
toRGB8 (ImageYF _) = error "YF"
toRGB8 (ImageYA8 _) = error "YA8"
toRGB8 (ImageYA16 _) = error "YA16"
toRGB8 (ImageRGB16 _) = error "RGB16"
toRGB8 (ImageRGBF _) = error "RGBF"
toRGB8 (ImageYCbCr8 x) = convertImage x
toRGB8 (ImageRGBA8  _) = error "RGBA8"
toRGB8 (ImageRGBA16 _) = error "RGBA16"
toRGB8 (ImageCMYK8 _) = error "CMYK8"
toRGB8 (ImageCMYK16 _) = error "CMYK16"

luminanceOf :: PixelRGB8 -> Word8
luminanceOf (PixelRGB8 x _ _) = x

chromaBlueOf :: PixelRGB8 -> Word8
chromaBlueOf (PixelRGB8 _ x _) = x

chromaGreenOf :: PixelRGB8 -> Word8
chromaGreenOf (PixelRGB8 _ _ x) = x


combine :: (Functor f, Pixel p) => Image p -> (f p -> p) -> f (Image p) -> Image p
combine model f imgs = generateImage pixelF (imageWidth model) (imageHeight model)
    where
    pixelF x y = f $ fmap (\img -> pixelAt img x y) imgs

average :: [Image PixelRGB8] -> Image PixelRGB8
average [] = error "cannot find average of no images"
average imgs@(model:_) = combine model modifier imgs
    where
    modifier = liftM3 PixelRGB8 (avg . map luminanceOf) (avg . map chromaBlueOf) (avg . map chromaGreenOf)
    avg :: [Word8] -> Word8
    avg us = fromIntegral $ sum (map fromIntegral us) `div` length us

crop :: (Pixel p) => (Int, Int, Int, Int) -> Image p -> Image p
crop (x0, y0, w, h) img = generateImage (\x y -> pixelAt img (x + x0) (y + y0)) w h

layerMonochrome :: (Pixel p) => p -> p -> p -> p -> p -> p -> p
layerMonochrome neither onlyA onlyB both color1 color2
    | color1 == neither = if color2 == neither then neither else onlyA
    | otherwise         = if color2 == neither then onlyB else both

empty :: Image PixelRGB8 -> Image PixelRGB8
empty img = generateImage (const $ const $ PixelRGB8 0 0 0) (imageWidth img) (imageHeight img)

visualizeStars :: [S.Set (Coor Int)] -> Image PixelRGB8 -> Image PixelRGB8
visualizeStars stars img
        = generateImage colorer (imageWidth img) (imageHeight img)
    where
    colorer x y = M.findWithDefault (pixelAt img x y) (x:$:y) colorlocs
    colorlocs :: M.Map (Coor Int) PixelRGB8
    colorlocs = (foldr (.) id $ zipWith addToMap colors stars) $ M.empty
    colors = cycle [
            PixelRGB8 0xFF 0 0,
            PixelRGB8 0xFF 0x88 0,
            PixelRGB8 0xFF 0xFF 0,
            PixelRGB8 0 0xFF 0,
            PixelRGB8 0 0xFF 0xFF,
            PixelRGB8 0 0 0xFF,
            PixelRGB8 0xFF 0 0xFF
        ]
    addToMap :: PixelRGB8 -> S.Set (Coor Int) -> M.Map (Coor Int) PixelRGB8 -> M.Map (Coor Int) PixelRGB8
    addToMap pixel = foldr (.) id . map (flip M.insert pixel) . S.toList

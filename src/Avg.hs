{-# LANGUAGE Trustworthy #-}

module Avg
  ( avgDigest
  , AvgDigest
  , grayScaleRGBA8
  , avgHash
  , mean
  , prepare
  ) where

import           Codec.Picture
import qualified Codec.Picture.Extra as Transform
import           Codec.Picture.Types
import           Data.Bits
import           Data.Typeable
import           Format
import           Types

instance Show AvgDigest where
  show (AvgDigest a) = show $ leadingBinary a

newtype AvgDigest =
  AvgDigest Int
  deriving (Eq, Bits)

avgDigest :: DynamicImage -> AvgDigest
avgDigest original = do
  let img = prepare original
  avgHash img (mean img)

prepare :: DynamicImage -> Image Pixel8
prepare dynImage =
  case dynImage of
    ImageY8 img     -> undefined
    ImageY16 img    -> undefined
    ImageY32 img    -> undefined
    ImageYF img     -> undefined
    ImageYA8 img    -> undefined
    ImageYA16 img   -> undefined
    ImageRGB8 img   -> undefined
    ImageRGB16 img  -> undefined
    ImageRGBF img   -> undefined
    ImageRGBA8 img  -> grayScaleRGBA8 $ Transform.scaleBilinear 16 16 img
    ImageRGBA16 img -> undefined
    ImageYCbCr8 img -> undefined
    ImageCMYK8 img  -> undefined
    ImageCMYK16 img -> undefined

mean :: Image Pixel8 -> Int
mean img = colorSum `div` area
  where
    colorSum = pixelFold reducer 0 img :: Int
    area = imageWidth img * imageHeight img :: Int
    reducer :: Int -> Int -> Int -> Pixel8 -> Int
    reducer acc _ _ r = acc + fromIntegral r

avgHash :: Image Pixel8 -> Int -> AvgDigest
avgHash img mean = AvgDigest . fst $ pixelFold (reducer mean) (0, 1) img
  where
    reducer :: Int -> ((Int, Int) -> Int -> Int -> Pixel8 -> (Int, Int))
    reducer mean (hash, p) _ _ r
      | fromIntegral r > mean = (hash .|. p, shiftL p 1)
      | otherwise = (hash, shiftL p 1)

-- These coefficients (the fractions 0.299, 0.587 and 0.114) are the same
-- as those given by the JFIF specification and used by func RGBToYCbCr in
-- ycbcr.go.
--
-- Note that 77 + 150 + 29  equals 256.
-- Note that 19595 + 38470 + 7471 equals 65536.
--
-- The 16 is 8 + 16
-- The 8 is because the return value is 8 bit color.
-- y := (77*r + 150*g + 29*b + 1<<15) >> 8
pixelToGray :: PixelRGBA8 -> Pixel8
pixelToGray (PixelRGBA8 r g b _) = gray
  where
    r' = fromIntegral r :: Integer
    g' = fromIntegral g :: Integer
    b' = fromIntegral b :: Integer
    gray = fromInteger $ (77 * r' + 150 * g' + 29 * b' + 1 `shiftL` 15) `shiftR` 8

grayScaleRGBA8 :: Image PixelRGBA8 -> Image Pixel8
grayScaleRGBA8 img@(Image w h _) = generateImage (\x y -> pixelToGray $ pixelAt img x y) w h

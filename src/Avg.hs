{-# LANGUAGE Trustworthy            #-}

module Avg where

import           Codec.Picture
import qualified Codec.Picture.Extra as Transform
import           Codec.Picture.Types (dropAlphaLayer, pixelFold)
import           Control.Applicative (pure)
import           Control.Exception   (Exception, throw)
import           Data.Bits
import           Format
import           Types

instance ImgDigest AvgDigest

instance Show AvgDigest where
  show (AvgDigest a) = show $ leadingBinary a

newtype AvgDigest =
  AvgDigest Int
  deriving (Eq, Bits)

instance Algo AvgDigest where
  hash original = do
    let img = grayScale $ resize $ convertRGB8 original
    avgHash img (mean img)

resize = Transform.scaleBilinear 16 16

mean :: Image PixelRGB8 -> Int
mean img = colorSum `div` area
  where
    colorSum = pixelFold reducer 0 img :: Int
    area = imageWidth img * imageHeight img :: Int
    reducer :: Int -> Int -> Int -> PixelRGB8 -> Int
    reducer acc _ _ (PixelRGB8 r g b) = acc + fromIntegral r

avgHash :: Image PixelRGB8 -> Int -> AvgDigest
avgHash img mean = AvgDigest . fst $ pixelFold (reducer mean) (0, 1) img
  where
    reducer :: Int -> ((Int, Int) -> Int -> Int -> PixelRGB8 -> (Int, Int))
    reducer mean (hash, p) _ _ (PixelRGB8 r g b)
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
pixelToGray :: PixelRGB8 -> PixelRGB8
pixelToGray (PixelRGB8 r g b) = PixelRGB8 gray gray gray
  where
    r' = fromIntegral r :: Integer
    g' = fromIntegral g :: Integer
    b' = fromIntegral b :: Integer
    gray = fromInteger $ (77 * r' + 150 * g' + 29 * b' + 1 `shiftL` 15) `shiftR` 8

grayScale :: Image PixelRGB8 -> Image PixelRGB8
grayScale img@(Image w h _) = generateImage (\x y -> pixelToGray $ pixelAt img x y) w h

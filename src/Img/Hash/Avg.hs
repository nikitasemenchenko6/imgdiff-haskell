{-# LANGUAGE Trustworthy #-}

module Img.Hash.Avg
  ( avgDigest
  , AvgDigest
  , grayScaleRGBA8
  , avgHash
  , mean
  , prepare
  )
where

import RIO

import qualified Codec.Picture.Extra as Transform
import Codec.Picture.Types
import Data.Bits
import qualified Format

instance Show AvgDigest where
  show (AvgDigest a) = show $ Format.leadingBinary a

newtype AvgDigest =
  AvgDigest Int
  deriving (Eq, Bits)

avgDigest :: DynamicImage -> AvgDigest
avgDigest original = do
  let img = prepare original
  avgHash img (mean img)

prepare :: DynamicImage -> Image Pixel8
prepare dynImage = case dynImage of
  ImageY8     _   -> undefined
  ImageY16    _   -> undefined
  ImageY32    _   -> undefined
  ImageYF     _   -> undefined
  ImageYA8    _   -> undefined
  ImageYA16   _   -> undefined
  ImageRGB8   _   -> undefined
  ImageRGB16  _   -> undefined
  ImageRGBF   _   -> undefined
  ImageRGBA8  img -> grayScaleRGBA8 $ Transform.scaleBilinear 16 16 img
  ImageRGBA16 _   -> undefined
  ImageYCbCr8 _   -> undefined
  ImageCMYK8  _   -> undefined
  ImageCMYK16 _   -> undefined

mean :: Image Pixel8 -> Int
mean img = colorSum `div` area
 where
  colorSum = pixelFold reducer 0 img :: Int
  area     = imageWidth img * imageHeight img :: Int
  reducer :: Int -> Int -> Int -> Pixel8 -> Int
  reducer acc _ _ r = acc + fromIntegral r

avgHash :: Image Pixel8 -> Int -> AvgDigest
avgHash img meanOfImg = AvgDigest . fst $ pixelFold reducer (0, 1) img
 where
  reducer :: ((Int, Int) -> Int -> Int -> Pixel8 -> (Int, Int))
  reducer (hash, p) _ _ r
    | fromIntegral r > meanOfImg = (hash .|. p, shiftL p 1)
    | otherwise                  = (hash, shiftL p 1)

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
pixelToGray (PixelRGBA8 r g b _) = fromInteger gray
 where
  r'   = toInteger r
  g'   = toInteger g
  b'   = toInteger b
  gray = (77 * r' + 150 * g' + 29 * b' + 1 `shiftL` 15) `shiftR` 8

grayScaleRGBA8 :: Image PixelRGBA8 -> Image Pixel8
grayScaleRGBA8 img@(Image w h _) = generateImage (\x y -> pixelToGray $ pixelAt img x y) w h

{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Lib
  ( distance
  , LibException(..)
  ) where

import           Codec.Picture
import qualified Codec.Picture.Extra as Transform
import           Codec.Picture.Types (dropAlphaLayer, pixelFold)
import           Control.Applicative (liftA2, pure)
import           Control.Exception   (Exception, throw)
import           Data.Bits
import qualified Data.ByteString     as BS
import           Data.List           (length)
import           Data.Typeable       (Typeable)
import           Debug.Trace         (trace, traceShow)
import           Format
import           Network.URI         (URI, isURI, parseURI)
import qualified System.IO           as IO

newtype Digest =
  Digest Int
  deriving (Eq, Bits)

data LibException
  = WrongFile !String
  | Other
  deriving (Show, Typeable, Eq, Ord)

instance Exception LibException

instance Show Digest where
  show (Digest a) = leadingBinary a

digestDistance :: Digest -> Digest -> Int
digestDistance a b = toPercent . popCount $ a `xor` b
  where
    toPercent :: Int -> Int
    toPercent a = round $ (100 * fromIntegral a) / 64

mean :: Image PixelRGB16 -> Int
mean img = colorSum `div` area
  where
    colorSum = pixelFold reducer 0 img :: Int
    area = imageWidth img * imageHeight img :: Int
    reducer :: Int -> Int -> Int -> PixelRGB16 -> Int
    reducer acc _ _ (PixelRGB16 r g b) = acc + fromIntegral r

avgHash :: Image PixelRGB16 -> Int -> Digest
avgHash img mean = Digest . fst $ pixelFold (reducer mean) (0, 1) img
  where
    reducer :: Int -> ((Int, Int) -> Int -> Int -> PixelRGB16 -> (Int, Int))
    reducer mean (hash, p) _ _ (PixelRGB16 r g b)
      | fromIntegral r > mean = (hash .|. p, shiftL p 1)
      | otherwise = (hash, shiftL p 1)

-- These coefficients (the fractions 0.299, 0.587 and 0.114) are the same
-- as those given by the JFIF specification and used by func RGBToYCbCr in
-- ycbcr.go.
--
-- Note that 19595 + 38470 + 7471 equals 65536.
--
-- The 16 is 8 + 8
-- The 8 is because the return value is 16 bit color.
-- Use 24 if will want return 8 bit color.
-- y := (19595*r + 38470*g + 7471*b + 1<<15) >> 24
pixelToGray :: PixelRGB16 -> PixelRGB16
pixelToGray (PixelRGB16 r g b) = PixelRGB16 gray gray gray
  where
    r' = fromIntegral r :: Integer
    g' = fromIntegral g :: Integer
    b' = fromIntegral b :: Integer
    gray = fromInteger $ (19595 * r' + 38470 * g' + 7471 * b' + 1 `shiftL` 15) `shiftR` 16

grayScale :: Image PixelRGB16 -> Image PixelRGB16
grayScale img@(Image w h _) = generateImage (\x y -> pixelToGray $ pixelAt img x y) w h

convertRGB16 :: DynamicImage -> Image PixelRGB16
convertRGB16 dynImage =
  case dynImage of
    ImageRGB16 img -> img
    ImageRGBA16 img -> dropAlphaLayer img
    _ -> do
      let img = convertRGB8 dynImage -- yeah, no method in lib to convert to RGB16 yet, will add later
      let w = imageWidth img
      let h = imageHeight img
      generateImage (\x y -> p8to16 $ pixelAt img x y) w h
  where
    p8to16 :: PixelRGB8 -> PixelRGB16
    p8to16 (PixelRGB8 r g b) = PixelRGB16 (fromIntegral r * 256) (fromIntegral g * 256) (fromIntegral b * 256)

distance :: String -> String -> IO Int
distance file1 file2 = liftA2 digestDistance (calcHash <$> readPng file1) (calcHash <$> readPng file2)

calcHash :: Either String DynamicImage -> Digest
calcHash eitherImg =
  case eitherImg of
    Left e         -> throw (WrongFile e)
    Right original -> hash $ grayScale $ resize $ convertRGB16 original
  where
    resize = Transform.scaleBilinear 16 16
    hash :: Image PixelRGB16 -> Digest
    hash img = avgHash img (mean img)

traceShow' a = traceShow a a

trace' a = trace $ typeWithVal a

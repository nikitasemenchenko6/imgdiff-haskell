{-# LANGUAGE Trustworthy          #-}

module Types where

import           Codec.Picture       (DynamicImage)
import           Control.Applicative (pure)
import           Control.Exception   (Exception, throw)
import           Data.Bits
import           Data.Typeable       (Typeable)
import           Format

data LibException
  = WrongFile !String
  | Other
  deriving (Show, Typeable, Eq, Ord)

instance Exception LibException

class Algo a where
  hash :: DynamicImage -> a

class (Bits a, Show a, Eq a) =>
      ImgDigest a
  where
  similarity :: a -> a -> Int
  similarity a b = toPercent . popCount $ a `xor` b
    where
      toPercent :: Int -> Int
      toPercent a = round $ (100 * fromIntegral a) / 64

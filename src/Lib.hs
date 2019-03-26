{-# LANGUAGE Trustworthy #-}

module Lib
  ( avgDistance
  , avgDistancePure
  , LibException(..)
  )
where

import RIO

import Codec.Picture
import Data.Bits
import Img.Hash.Avg
import Types

avgDistance :: ValidFilePath -> ValidFilePath -> IO (Either String Percent)
avgDistance (ValidFilePath file1) (ValidFilePath file2) = do
  f1 <- readPng file1
  f2 <- readPng file2
  let res = pure avgDistancePure <*> f1 <*> f2
  return res

avgDistancePure :: DynamicImage -> DynamicImage -> Percent
avgDistancePure f1 f2 = avgDigest f1 `similarity` avgDigest f2
 where
  similarity :: (Bits a) => a -> a -> Percent
  similarity a b = toPercent . popCount $ a `xor` b
  toPercent :: Int -> Percent
  toPercent a = Percent $ (100 * fromIntegral a) / 64

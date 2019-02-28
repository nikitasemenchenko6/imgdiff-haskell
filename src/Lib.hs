{-# LANGUAGE Trustworthy #-}

module Lib
  ( avgDistance
  , avgDistancePure
  , LibException(..)
  ) where

import           Avg
import           Codec.Picture
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Data.Bits
import           Format
import           Network.URI
import qualified System.IO                as IO
import           Types

avgDistance :: FilePath -> FilePath -> IO Int
avgDistance file1 file2 = do
  (f1, f2) <- concurrently (readPng file1) (readPng file2)
  pure $ avgDistancePure f1 f2

avgDistancePure :: Either String DynamicImage -> Either String DynamicImage -> Int
avgDistancePure f1 f2 = calcAvg f1 `similarity` calcAvg f2

calcAvg :: Either String DynamicImage -> AvgDigest
calcAvg eitherImg =
  case eitherImg of
    Left e         -> throw (WrongFile e)
    Right original -> avgDigest original

similarity :: (Bits a) => a -> a -> Int
similarity a b = toPercent . popCount $ a `xor` b
  where
    toPercent :: Int -> Int
    toPercent a = round $ (100 * fromIntegral a) / 64

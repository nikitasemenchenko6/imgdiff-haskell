{-# LANGUAGE Trustworthy #-}

module Lib
  ( avgDistance
  , avgDistancePure
  , LibException(..)
  ) where

import           Avg
import           Codec.Picture       (DynamicImage, readPng)
import           Control.Applicative (liftA2, pure)
import           Control.Exception   (Exception, throw)
import           Data.Bits
import           Format
import           Network.URI         (URI, isURI, parseURI)
import qualified System.IO           as IO
import           Types


avgDistance :: FilePath -> FilePath -> IO Int
avgDistance file1 file2 = do
  f1 <- readPng file1
  f2 <- readPng file2
  pure $ avgDistancePure f1 f2

avgDistancePure :: Either String DynamicImage -> Either String DynamicImage -> Int
avgDistancePure f1 f2 = calcAvg f1 `similarity` calcAvg f2

calcAvg :: Either String DynamicImage -> AvgDigest
calcAvg eitherImg =
  case eitherImg of
    Left e         -> throw (WrongFile e)
    Right original -> hash original

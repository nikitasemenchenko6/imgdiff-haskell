{-# LANGUAGE Trustworthy          #-}

module Lib
  ( avgDistance
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
  r1 <- calc <$> readPng file1 :: IO AvgDigest
  r2 <- calc <$> readPng file2 :: IO AvgDigest
  pure $ r1 `similarity` r2

calc :: (Algo a) => Either String DynamicImage -> a
calc eitherImg =
  case eitherImg of
    Left e         -> throw (WrongFile e)
    Right original -> hash original

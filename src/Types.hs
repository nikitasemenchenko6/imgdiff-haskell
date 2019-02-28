{-# LANGUAGE Trustworthy          #-}

module Types where

import           Codec.Picture       (DynamicImage)
import           Control.Applicative
import           Control.Exception
import           Data.Bits
import           Data.Typeable
import           Format

data LibException
  = WrongFile !String
  | Other
  deriving (Show, Typeable, Eq, Ord)

instance Exception LibException

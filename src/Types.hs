{-# LANGUAGE Trustworthy #-}

module Types where

import           Codec.Picture       (DynamicImage)
import           Control.Applicative
import           Control.Exception
import           Data.Bits
import           Data.Typeable

data LibException
  = WrongFile !String
  | Other
  deriving (Show, Typeable, Eq, Ord)

instance Exception LibException

newtype Percent =
  Percent Double
  deriving (Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat)

instance Show Percent where
  show (Percent t) = show t ++ "%"

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia #-}

module Types where

import           Control.Exception
import           Data.Typeable
import           Data.String

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

------ Validation -------
data VError
  = MustNotBeEmpty
  | IllegalExtension
  | MustContainPeriod
  deriving (Show)

newtype ValidFilePath =
  ValidFilePath FilePath
  deriving (IsString, Show) via FilePath

newtype CorrectExtension =
  CorrectExtension String
  deriving (Show)

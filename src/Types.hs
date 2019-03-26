{-# LANGUAGE Trustworthy #-}

module Types where

import RIO

import Control.Exception
import Data.Typeable
import Data.String
import RIO.Process

-- | Command line arguments
data Options = Options
  {
    optionsVerbose :: !Bool,
    f1 :: !String,
    f2 :: !String
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })



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
  deriving (IsString, Show)

newtype CorrectExtension =
  CorrectExtension String
  deriving (Show)



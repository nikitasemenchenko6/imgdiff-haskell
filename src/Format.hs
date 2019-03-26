{-# LANGUAGE Trustworthy #-}

module Format where

import RIO

import Data.Typeable
import Formatting
import qualified RIO.Text.Lazy as TL
import Types

leadingBinary :: Integral a => a -> TL.Text
leadingBinary = format prefixBin

typeWithVal :: (Show a, Typeable a) => a -> TL.Text
typeWithVal a =
  format ("Type: " % shown % ", Val: " % shown) (show $ typeOf a) a

differenceToString :: Percent -> TL.Text
differenceToString = format ("Diffeernce: " % shown % "%")

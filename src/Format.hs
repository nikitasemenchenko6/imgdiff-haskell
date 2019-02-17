{-# LANGUAGE OverloadedStrings #-}

module Format where

import           Data.Text.Lazy
import           Data.Text.Lazy.Builder (Builder)
import           Formatting
import           Formatting.Formatters
import           Data.Typeable

leadingBinary :: Integral a => a -> String
leadingBinary a = show $ format prefixBin a

typeWithVal :: (Show a, Typeable a) => a -> String
typeWithVal a = show $ format ("Type: " % shown % ", Val: " % shown) (show $ typeOf a) a

differenceToString :: Integral a => a -> String
differenceToString diff = show $ format ("Diffeernce: " % int % "%") diff
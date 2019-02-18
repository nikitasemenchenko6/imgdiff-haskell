{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module Format where

import           Data.Text.Lazy
import           Data.Text.Lazy.Builder (Builder)
import           Data.Typeable
import           Formatting
import           Formatting.Formatters

leadingBinary :: Integral a => a -> Text
leadingBinary = format prefixBin

typeWithVal :: (Show a, Typeable a) => a -> Text
typeWithVal a = format ("Type: " % shown % ", Val: " % shown) (show $ typeOf a) a

differenceToString :: Integral a => a -> Text
differenceToString = format ("Diffeernce: " % int % "%")

{-# LANGUAGE Trustworthy #-}

module Main where

import Control.Lens
import Data.List as List
import Data.Validation
import Format
import Lib
import System.Environment (getArgs)
import System.FilePath
import Types

main :: IO ()
main = do
  a <- run
  return ()

run :: IO ()
run = do
  args <- getArgs
  let (file1, file2) =
        if | List.length args == 2 -> (List.head args, List.head $ List.tail args)
           | otherwise -> error "2 args required: imgdiff ./file1.jpg ./file2.jpeg"
  f1 <- mustValidFilePath file1
  f2 <- mustValidFilePath file2
  eitherDist <- avgDistance f1 f2
  printResult eitherDist
  where

    mustValidFilePath :: String -> IO ValidFilePath
    mustValidFilePath x = forceValid $ validateFilePath x
    forceValid :: Validation [VError] ValidFilePath -> IO ValidFilePath
    forceValid (Success x) = pure x
    forceValid (Failure (e:_)) = error $ show e
    forceValid (Failure []) = error "some error"

    printResult :: Either String Percent -> IO ()
    printResult (Left e) = error e
    printResult (Right dist) = print $ show $ differenceToString dist

validateFilePath :: String -> Validation [VError] ValidFilePath
validateFilePath x = ValidFilePath x <$ validate [IllegalExtension] checkExt x
  where
    checkExt x = snd (splitExtension x) == "png"


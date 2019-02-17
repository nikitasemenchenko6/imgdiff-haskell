{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf    #-}

module Main where

import           Control.Exception
import           Data.List
import           Format
import           Lib
import           System.Environment (getArgs)
import           System.IO          (putStrLn)

main :: IO ()
main = do
  args <- getArgs
  let (file1, file2) =
        if | length args == 2 -> (head args, head $ tail args)
           | otherwise -> error "2 args required: imgdiff ./file1.jpg ./file2.jpeg"
  dist <- distance file1 file2
  putStrLn $ differenceToString dist
  pure ()

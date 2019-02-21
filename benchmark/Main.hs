module Main where

import           Codec.Picture  (DynamicImage, readPng)
import           Criterion
import           Criterion.Main (defaultMain)
import           Lib
import           System.Random
import           Weigh

main :: IO ()
main = do
  f1 <- readPng "test-png-original.png"
  f2 <- readPng "test-png-scaled-down.png"
  let toBench = avgDistancePure f1
-- bench time
  defaultMain [bgroup "lib" [bench "avg" $ whnf toBench f2]]
-- bench memory
  mainWith (wgroup "lib" (func "avg" toBench f2))

module Main where

import           Codec.Picture
import           Criterion
import           Criterion.Main (defaultMain)
import           Lib
import           System.Random

main :: IO ()
main = do
-- bench time
  defaultMain [bgroup "lib" [
    bench "avg" $ whnfAppIO toBench 1
    ]]

toBench _ = avgDistance "test-png-original.png" "test-png-scaled-down.png"

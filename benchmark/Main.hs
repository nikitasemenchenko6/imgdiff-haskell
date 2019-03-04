module Main where

import           Codec.Picture
import           Criterion
import           Criterion.Main (defaultMain)
import           Lib
import           System.Random

main :: IO ()
main = do
  let toBenchAvgDistance = avgDistance "test-png-original.png"

-- bench time
  defaultMain [bgroup "lib" [
    bench "avg" $ whnfAppIO toBenchAvgDistance "test-png-scaled-down.png"
    ]]


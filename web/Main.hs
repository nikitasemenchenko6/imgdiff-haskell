{-# LANGUAGE Trustworthy #-}

module Main where

import RIO

import System.IO
import qualified Network.Wai.Handler.Warp as Warp
import qualified Web.Server

main :: IO ()
main = Warp.runSettings settings Web.Server.app
 where
  logSettings = hPutStrLn stderr ("listening on port " ++ show port ++ ": " ++ show host)
  port        = 3000
  host        = "localhost"
  settings    = Warp.setPort port $ Warp.setHost host $ Warp.setBeforeMainLoop logSettings Warp.defaultSettings


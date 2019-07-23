{-# LANGUAGE Trustworthy #-}

module Main where

import RIO

import           System.IO
import           Network.Wai.Handler.Warp
import qualified Web.Server

main :: IO ()
main = runSettings settings Web.Server.app
 where
  logSettings = hPutStrLn stderr ("listening on port " ++ show port ++ ": " ++ show host)
  port        = 3000
  host        = "localhost"
  settings    = setPort port $ setHost host $ setBeforeMainLoop (logSettings) $ defaultSettings


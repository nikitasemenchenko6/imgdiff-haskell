{-# LANGUAGE Trustworthy #-}

module Main where

import           RIO

import qualified Network.Wai.Handler.Warp as Warp
import System.IO (hPutStrLn, stderr)
import qualified Web.Server

main :: IO ()
main = Warp.run 3000 Web.Server.app
  where
    logSettings = hPutStrLn stderr ("listening on port " ++ show port ++ ": " ++ show host)
    port = 3000
    host = "localhost"
    settings = Warp.setPort port $ Warp.setHost host $ Warp.setBeforeMainLoop logSettings Warp.defaultSettings

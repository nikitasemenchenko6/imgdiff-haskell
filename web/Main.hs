{-# LANGUAGE Trustworthy #-}

module Main where

import RIO

import qualified Web.Server

main :: IO ()
main = Web.Server.listen

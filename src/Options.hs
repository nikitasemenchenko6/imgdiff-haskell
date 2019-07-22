{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy     #-}

module Options
  ( options
  )
where

import RIO

import Options.Applicative.Simple (empty, help, long, short, simpleOptions, simpleVersion, strOption, switch)
import Paths_imgdiff_haskell (version)
import Types

options :: IO Options
options = do
  (opts, ()) <- simpleOptions
    $(simpleVersion version)
    title
    description
    (   Options
    <$> switch (long "verbose" <> short 'v' <> help "Verbose output?")
    <*> strOption (long "f1" <> help "File 1")
    <*> strOption (long "f2" <> help "File 2")
    )
    empty
  return opts
 where
  title       = "Header for command line arguments"
  description = "Program description, also for command line arguments"

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy     #-}

module Options
  ( options
  ) where

import           Options.Applicative.Simple (empty, help, long, short,
                                             simpleOptions, simpleVersion,
                                             strOption, switch)
import           Paths_imgdiff_haskell      (version)
import           RIO
import           Types

options :: IO Options
options = do
  (opts, ()) <- simpleOptions $(simpleVersion version) title description (Options <$> getVersion <*> f1 <*> f2) empty
  return opts
  where
    getVersion = switch (long "verbose" <> short 'v' <> help "Verbose output?")
    f1 = strOption (long "f1" <> help "File 1")
    f2 = strOption (long "f2" <> help "File 2")
    title = "Header for command line arguments"
    description = "Program description, also for command line arguments"

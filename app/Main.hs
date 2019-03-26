{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy     #-}

module Main where

import           RIO

import           Data.Validation
import           Format
import           Lib
import           Options.Applicative.Simple (empty, help, long, short,
                                             simpleOptions, simpleVersion,
                                             strOption, switch)
import           Paths_imgdiff_haskell      (version)
import           RIO.Process
import           System.FilePath
import           Types

main :: IO ()
main = do
  opts <- options
  lo <- logOptionsHandle stderr (optionsVerbose opts)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = opts}
     in runRIO app run
  where
    options :: IO Options
    options = do
      (opts, ()) <-
        simpleOptions
          $(simpleVersion version)
          "Header for command line arguments"
          "Program description, also for command line arguments"
          (Options <$> switch (long "verbose" <> short 'v' <> help "Verbose output?") <*>
           strOption (long "f1" <> help "File 1") <*>
           strOption (long "f2" <> help "File 2"))
          empty
      return opts

--      $(simpleVersion "1.2.1" :: Version)
run :: RIO App ()
run = do
  app <- ask
  f1 <- mustValidFilePath $ f1 $ appOptions app
  f2 <- mustValidFilePath $ f2 $ appOptions app
  eitherDist <- liftIO $ avgDistance f1 f2
  printResult eitherDist
  where
    mustValidFilePath :: String -> RIO env ValidFilePath
    mustValidFilePath x = forceValid $ validateFilePath x
    forceValid (Success x)     = pure x
    forceValid (Failure (e:_)) = error $ show e
    forceValid (Failure [])    = error "some error"
    printResult :: Either String Percent -> RIO App ()
    printResult (Left e)     = error e
    printResult (Right dist) = logInfo $ display $ differenceToString dist

validateFilePath :: String -> Validation [VError] ValidFilePath
validateFilePath x = ValidFilePath x <$ validate [IllegalExtension] checkExt x
  where
    checkExt file = snd (splitExtension file) == ".png"

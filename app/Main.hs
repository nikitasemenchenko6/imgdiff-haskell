{-# LANGUAGE Trustworthy #-}

module Main where

import           RIO

import           Data.Validation
import           Format
import           Lib
import           Options         (options)
import           RIO.FilePath
import           RIO.Process
import           Types

main :: IO ()
main = do
  opts <- options
  lo <- logOptionsHandle stderr (optionsVerbose opts)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = opts}
     in runRIO app run

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
    getExt = snd . splitExtension
    checkExt file
      | getExt file == ".png" = Just (getExt file)
      | otherwise = Nothing

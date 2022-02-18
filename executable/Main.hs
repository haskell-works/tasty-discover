-- | Main executable module.

module Main where

import Control.Monad       (when)
import Data.Maybe          (fromMaybe)
import System.Environment  (getArgs, getProgName)
import System.Exit         (exitFailure)
import System.IO           (IOMode(ReadMode), hGetContents, hPutStrLn, withFile, stderr)
import Test.Tasty.Config   (Config (..), parseConfig)
import Test.Tasty.Discover (findTests, generateTestDriver)

-- | Main function.
main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    src:_:dst:opts ->
      case parseConfig name opts of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right config -> do
          tests <- findTests src config
          let ingredients = tastyIngredients config
              moduleName  = fromMaybe "Main" (generatedModuleName config)
          header <- readHeader src
          let output = generateTestDriver config moduleName ingredients src tests
          when (debug config) $ hPutStrLn stderr output
          when (inplace config) $ writeFile src $ unlines $ header ++ [marker, output]
          writeFile dst $
            "{-# LINE " ++ show (length header + 2) ++ " " ++ show src ++ " #-}\n"
            ++ output
    _ -> do
      hPutStrLn stderr "Usage: tasty-discover src _ dst [OPTION...]"
      exitFailure
  where
    marker = "-- GENERATED BY tasty-discover"
    readHeader src = withFile src ReadMode $ \h -> do
      header <- takeWhile (marker /=) . lines <$> hGetContents h
      seq (length header) (return header)

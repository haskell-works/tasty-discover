module Main where

import qualified Tests
import qualified Test.Tasty as T

main :: IO ()
main = do
  putStrLn "=== No-Main Feature Test ==="
  putStrLn "This test demonstrates the --no-main option"
  putStrLn "where Tests module has no main function.\n"

  -- Get the discovered tests from the Tests module
  discoveredTests <- Tests.tests

  -- Apply a custom wrapper to demonstrate custom main functionality
  let wrappedTests = T.testGroup "Custom No-Main Wrapper" [discoveredTests]

  -- Run tests using ingredients from Tests module
  T.defaultMainWithIngredients Tests.ingredients wrappedTests

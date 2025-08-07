{-# LINE 3 "test/ModulesGlob/TestRunner.hs" #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main, ingredients, tests) where

import Prelude
import qualified Sub.OneTest
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.QuickCheck as QC
import qualified TwoTest

{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Use let" -}


tests :: IO T.TestTree
tests = do
  t0 <- pure $ QC.testProperty "subTest" Sub.OneTest.prop_subTest

  t1 <- pure $ QC.testProperty "topLevelTest" TwoTest.prop_topLevelTest

  pure $ T.testGroup "test/ModulesGlob/TestRunner.hs" [t0,t1]
ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients
main :: IO ()
main = do
  args <- E.getArgs
  E.withArgs ([] ++ args) $    tests >>= T.defaultMainWithIngredients ingredients

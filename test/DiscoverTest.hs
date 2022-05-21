{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module DiscoverTest where

import Data.ByteString.Lazy (ByteString)
import Data.List
import Test.Tasty.Discover (TestGroup(..))
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec (shouldBe)

import qualified Hedgehog       as H
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

unit_listCompare :: IO ()
unit_listCompare = [1 :: Int, 2, 3] `compare` [1,2] @?= GT

prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

spec_prelude :: Spec
spec_prelude = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)

test_addition :: TestTree
test_addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a

test_multiplication :: [TestTree]
test_multiplication =
  [ testProperty "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
  , testProperty "One is identity" $ \(a :: Int) -> a == a
  ]

test_generateTree :: IO TestTree
test_generateTree = do
  let input = "Some input"
  pure $ testCase input $ pure ()

test_generateTrees :: IO [TestTree]
test_generateTrees = pure (map (\s -> testCase s $ pure ()) ["First input", "Second input"])

{- HLINT ignore "Avoid reverse" -}
hprop_reverse :: H.Property
hprop_reverse = H.property $ do
  xs <- H.forAll $ G.list (R.linear 0 100) G.alpha
  reverse (reverse xs) H.=== xs

data GoldenTest = GoldenTest FilePath (IO ByteString)
instance TestGroup GoldenTest where
  testGroup description _name (GoldenTest fp act) = pure $ goldenVsString description fp act

custom_goldenTest :: GoldenTest
custom_goldenTest = GoldenTest "test/SubMod/example.golden" $ return "test"

module Tests.Simple where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

-- Simple test cases for the no-main feature demo
unit_simpleTest :: IO ()
unit_simpleTest = pure ()

prop_alwaysTrue :: Bool -> Bool
prop_alwaysTrue _ = True

tasty_customGroup :: T.TestTree
tasty_customGroup = T.testGroup "Custom Test Group"
  [ HU.testCase "custom test 1" $ return ()
  , HU.testCase "custom test 2" $ (1 + 1 :: Int) HU.@?= 2
  ]

module Test.Tasty.Discover.Custom
  ( FromCustomTest (..)
  ) where

import Test.Tasty (TestTree)

class FromCustomTest a where
  fromCustomTest :: String -> a -> IO TestTree

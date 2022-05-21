{-# LANGUAGE FlexibleInstances #-}

module Test.Tasty.Discover
  ( TestGroup(..)
  , TestCase(..)
  ) where

import qualified Test.Tasty as TT

class TestGroup a where
  testGroup :: String -> a -> IO TT.TestTree

instance TestGroup TT.TestTree where
  testGroup _ a = pure a

instance TestGroup [TT.TestTree] where
  testGroup n a = pure $ TT.testGroup n a

instance TestGroup (IO TT.TestTree) where
  testGroup _ a = a

instance TestGroup (IO [TT.TestTree]) where
  testGroup n a = TT.testGroup n <$> a

class TestCase a where
  testCase :: String -> a -> IO TT.TestTree

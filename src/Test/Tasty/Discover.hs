{-# LANGUAGE FlexibleInstances #-}

module Test.Tasty.Discover
  ( TestGroup(..)
  , TestCase(..)
  ) where

import qualified Test.Tasty as TT

class TestGroup a where
  testGroup :: String -> String -> a -> IO TT.TestTree

instance TestGroup TT.TestTree where
  testGroup _ _ a = pure a

instance TestGroup [TT.TestTree] where
  testGroup n _ a = pure $ TT.testGroup n a

instance TestGroup (IO TT.TestTree) where
  testGroup _ _ a = a

instance TestGroup (IO [TT.TestTree]) where
  testGroup n _ a = TT.testGroup n <$> a

class TestCase a where
  testCase :: String -> String -> a -> IO TT.TestTree

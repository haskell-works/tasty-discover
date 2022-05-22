{-# LANGUAGE FlexibleInstances #-}

module Test.Tasty.Discover
  ( Tasty(..)
  , TastyInfo
  , name
  , description
  , nameOf
  , descriptionOf
  ) where

import Data.Maybe
import Data.Monoid
import Test.Tasty.Discover.TastyInfo (TastyInfo)

import qualified Test.Tasty as TT
import qualified Test.Tasty.Discover.TastyInfo as TI

class Tasty a where
  tasty :: TastyInfo -> a -> IO TT.TestTree

instance Tasty TT.TestTree where
  tasty _ a = pure a

instance Tasty [TT.TestTree] where
  tasty info a = pure $ TT.testGroup (descriptionOf info) a

instance Tasty (IO TT.TestTree) where
  tasty _ a = a

instance Tasty (IO [TT.TestTree]) where
  tasty info a = TT.testGroup (descriptionOf info) <$> a

nameOf :: TastyInfo -> String
nameOf info = (fromMaybe "<unnamed>" (getLast (TI.name info)))

descriptionOf :: TastyInfo -> String
descriptionOf info = (fromMaybe "<undescribed>" (getLast (TI.description info)))

name :: String -> TastyInfo
name n = mempty
  { TI.name = Last $ Just n
  }

description :: String -> TastyInfo
description n = mempty
  { TI.description = Last $ Just n
  }

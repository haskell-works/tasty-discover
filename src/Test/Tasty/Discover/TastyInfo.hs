module Test.Tasty.Discover.TastyInfo
  ( TastyInfo(..)
  ) where

import Data.Monoid

data TastyInfo = TastyInfo
  { name        :: Last String
  , description :: Last String
  } deriving (Eq, Show)

instance Semigroup TastyInfo where
  a <> b = TastyInfo
    { name        = name a        <> name b
    , description = description a <> description b
    }

instance Monoid TastyInfo where
  mempty = TastyInfo
    { name        = Last Nothing
    , description = Last Nothing
    }

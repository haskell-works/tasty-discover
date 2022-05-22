module Test.Tasty.Discover.Version
  ( version
  ) where

import Data.Version (Version(..))

import qualified Paths_tasty_discover as P

version :: Version
version = P.version

-- | The test generator boilerplate module.
--
-- Any test that is supported (HUnit, HSpec, etc.) provides here, a
-- generator type with all the context necessary for outputting the
-- necessary boilerplate for the generated main function that will
-- run all the tests.

module Test.Tasty.Discover.Internal.Generator
  ( -- * Types
    Generator (..)
  , Test (..)

    -- * Generators
  , generators
  , getGenerator
  , getGenerators

    -- * Boilerplate Formatter
  , showSetup

    -- * Type Constructor
  , mkTest
  ) where

import Data.Function   (on)
import Data.List       (find, groupBy, isPrefixOf, sortOn)
import Data.Maybe      (fromJust)
import System.FilePath (dropExtension, isPathSeparator)

-- | The test type.
data Test = Test
  { testModule   :: String -- ^ Module name.
  , testFunction :: String -- ^ Function name.
  } deriving (Eq, Show, Ord)

-- | 'Test' constructor.
mkTest :: FilePath -> String -> Test
mkTest = Test . replacePathSepTo '.' . dropExtension
  where replacePathSepTo c1 = map $ \c2 -> if isPathSeparator c2 then c1 else c2

-- | The generator type.
data Generator = Generator
  { generatorPrefix   :: String          -- ^ Generator prefix.
  , generatorImports  :: [String]        -- ^ Module import path.
  , generatorClass    :: String          -- ^ Generator class.
  , generatorSetup    :: Test -> String  -- ^ Generator setup.
  }

-- | Module import qualifier.
qualifyFunction :: Test -> String
qualifyFunction t = testModule t ++ "." ++ testFunction t

-- | Function namer.
name :: Test -> String
name = chooser '_' ' ' . tail . dropWhile (/= '_') . testFunction
  where chooser c1 c2 = map $ \c3 -> if c3 == c1 then c2 else c3

-- | Generator retriever (single).
getGenerator :: Test -> Generator
getGenerator t = fromJust $ getPrefix generators
  where getPrefix = find ((`isPrefixOf` testFunction t) . generatorPrefix)

-- | Generator retriever (many).
getGenerators :: [Test] -> [Generator]
getGenerators =
  map head .
  groupBy  ((==) `on` generatorPrefix) .
  sortOn generatorPrefix .
  map getGenerator

-- | Boilerplate formatter.
showSetup :: Test -> String -> String
showSetup t var = "  " ++ var ++ " <- " ++ setup ++ "\n"
  where setup = generatorSetup (getGenerator t) t

-- | All types of tests supported for boilerplate generation.
generators :: [Generator]
generators =
  [ quickCheckPropertyGenerator
  , smallCheckPropertyGenerator
  , hedgehogPropertyGenerator
  , hunitTestCaseGenerator
  , hspecTestCaseGenerator
  , customGroupGenerator
  , tastyTestGroupGenerator
  ]

-- | Quickcheck group generator prefix.
hedgehogPropertyGenerator :: Generator
hedgehogPropertyGenerator = Generator
  { generatorPrefix   = "hprop_"
  , generatorImports  = ["import qualified Test.Tasty.Hedgehog as H"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "pure $ H.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Quickcheck group generator prefix.
quickCheckPropertyGenerator :: Generator
quickCheckPropertyGenerator = Generator
  { generatorPrefix   = "prop_"
  , generatorImports  = ["import qualified Test.Tasty.QuickCheck as QC"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "pure $ QC.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Smallcheck group generator prefix.
smallCheckPropertyGenerator :: Generator
smallCheckPropertyGenerator = Generator
  { generatorPrefix   = "scprop_"
  , generatorImports  = ["import qualified Test.Tasty.SmallCheck as SC"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "pure $ SC.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | HUnit generator prefix.
hunitTestCaseGenerator :: Generator
hunitTestCaseGenerator = Generator
  { generatorPrefix   = "unit_"
  , generatorImports  =
      [ "import qualified Test.Tasty.HUnit as HU"
      , "import qualified Test.Tasty.Discover as TD" 
      ]
  , generatorClass    = concat
    [ "instance TD.TestCase (IO ())                      where testCase d _ = pure . HU.testCase      d\n"
    , "instance TD.TestCase (IO String)                  where testCase d _ = pure . HU.testCaseInfo  d\n"
    , "instance TD.TestCase ((String -> IO ()) -> IO ()) where testCase d _ = pure . HU.testCaseSteps d\n"
    ]
  , generatorSetup  = \t -> "TD.testCase \"" ++ name t ++ "\" \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Hspec generator prefix.
hspecTestCaseGenerator :: Generator
hspecTestCaseGenerator = Generator
  { generatorPrefix   = "spec_"
  , generatorImports  = ["import qualified Test.Tasty.Hspec as HS"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "HS.testSpec \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

-- | Custom test generator prefix.
customGroupGenerator :: Generator
customGroupGenerator = Generator
  { generatorPrefix   = "custom_"
  , generatorImports  = ["import qualified Test.Tasty.Discover as TD"]
  , generatorClass    = ""
  , generatorSetup    = \t -> "TD.testGroup \"" ++ name t ++ "\" \"" ++ qualifyFunction t ++ "\" " ++ qualifyFunction t
  }

-- | Tasty group generator prefix.
tastyTestGroupGenerator :: Generator
tastyTestGroupGenerator = Generator
  { generatorPrefix   = "test_"
  , generatorImports  = ["import qualified Test.Tasty.Discover as TD"]
  , generatorClass    = []
  , generatorSetup  = \t -> "TD.testGroup \"" ++ name t ++ "\" \"" ++ qualifyFunction t ++ "\" " ++ qualifyFunction t
  }

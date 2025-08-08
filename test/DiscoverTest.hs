{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Avoid reverse" -}

module DiscoverTest where

import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Maybe (listToMaybe)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGRCode)
import System.Info (os)
import Test.Hspec (shouldBe)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Tasty
import Test.Tasty.Discover (Flavored, flavored, skip, platform, evaluatePlatformExpression)
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Property, property)

import qualified Hedgehog            as H
import qualified Hedgehog.Gen        as G
import qualified Hedgehog.Range      as R
import qualified Test.Tasty.Discover as TD
import qualified Test.Tasty.Hedgehog as TH

------------------------------------------------------------------------------------------------

unit_listCompare :: IO ()
unit_listCompare = [1 :: Int, 2, 3] `compare` [1,2] @?= GT

------------------------------------------------------------------------------------------------

prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

------------------------------------------------------------------------------------------------

scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

------------------------------------------------------------------------------------------------

spec_prelude :: Spec
spec_prelude = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    listToMaybe [23 ..] `shouldBe` Just (23 :: Int)

------------------------------------------------------------------------------------------------

test_addition :: TestTree
test_addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a

------------------------------------------------------------------------------------------------

test_multiplication :: [TestTree]
test_multiplication =
  [ testProperty "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
  , testProperty "One is identity" $ \(a :: Int) -> a == a
  ]

------------------------------------------------------------------------------------------------

test_generateTree :: IO TestTree
test_generateTree = do
  let input = "Some input"
  pure $ testCase input $ pure ()

------------------------------------------------------------------------------------------------

test_generateTrees :: IO [TestTree]
test_generateTrees = pure (map (\s -> testCase s $ pure ()) ["First input", "Second input"])

------------------------------------------------------------------------------------------------
-- How to simultaneously support tasty-hedgehog <1.2 and ^>1.2 using a custom test

newtype Property = Property
  { unProperty :: H.Property
  }

instance TD.Tasty Property where
  tasty info (Property p) = do
    let name = TD.nameOf info
    let mkTestTree =
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
          TH.testPropertyNamed name (fromString (TD.descriptionOf info))
#else
          TH.testProperty name
#endif
        yellowText text = setSGRCode [SetColor Foreground Vivid Yellow] ++ text ++ setSGRCode [Reset]
    -- Apply skip functionality if SkipTest option is True
    pure $ askOption $ \(TD.SkipTest shouldSkip) ->
      if shouldSkip
        then testCase (TD.nameOf info ++ " " ++ yellowText "[SKIPPED]") (pure ())
        else mkTestTree p

property :: HasCallStack => H.PropertyT IO () -> Property
property = Property . H.property

tasty_reverse :: Property
tasty_reverse = property $ do
  xs <- H.forAll $ G.list (R.linear 0 100) G.alpha
  reverse (reverse xs) H.=== xs

tasty_skip_me :: Flavored Property
tasty_skip_me =
  flavored skip $ property $ do
    H.failure

------------------------------------------------------------------------------------------------
-- Platform expression tests

-- Test basic platform matching
unit_platformExpression_linux :: Assertion  
unit_platformExpression_linux =
  evaluatePlatformExpression "linux" "linux" @?= True

unit_platformExpression_darwin :: Assertion
unit_platformExpression_darwin =
  evaluatePlatformExpression "darwin" "darwin" @?= True

unit_platformExpression_windows :: Assertion
unit_platformExpression_windows =
  evaluatePlatformExpression "windows" "mingw32" @?= True

unit_platformExpression_mingw32 :: Assertion
unit_platformExpression_mingw32 =
  evaluatePlatformExpression "mingw32" "mingw32" @?= True

-- Test negation
unit_platformExpression_not_linux :: Assertion
unit_platformExpression_not_linux =
  evaluatePlatformExpression "!linux" "darwin" @?= True

unit_platformExpression_not_windows :: Assertion
unit_platformExpression_not_windows =
  evaluatePlatformExpression "!windows" "linux" @?= True

-- Test conjunction (AND)
unit_platformExpression_and_true :: Assertion
unit_platformExpression_and_true =
  evaluatePlatformExpression "!windows & !darwin" "linux" @?= True

unit_platformExpression_and_false :: Assertion
unit_platformExpression_and_false =
  evaluatePlatformExpression "!windows & !darwin" "darwin" @?= False

-- Test disjunction (OR)  
unit_platformExpression_or_true :: Assertion
unit_platformExpression_or_true =
  evaluatePlatformExpression "linux | darwin" "linux" @?= True

unit_platformExpression_or_false :: Assertion
unit_platformExpression_or_false =
  evaluatePlatformExpression "linux | darwin" "mingw32" @?= False

-- Test unix special case
unit_platformExpression_unix_linux :: Assertion
unit_platformExpression_unix_linux =
  evaluatePlatformExpression "unix" "linux" @?= True

unit_platformExpression_unix_darwin :: Assertion
unit_platformExpression_unix_darwin =
  evaluatePlatformExpression "unix" "darwin" @?= True

unit_platformExpression_unix_windows :: Assertion
unit_platformExpression_unix_windows =
  evaluatePlatformExpression "unix" "mingw32" @?= False

-- Test complex expressions
unit_platformExpression_complex1 :: Assertion
unit_platformExpression_complex1 =
  evaluatePlatformExpression "!windows & !darwin" "linux" @?= True

unit_platformExpression_complex2 :: Assertion  
unit_platformExpression_complex2 =
  evaluatePlatformExpression "linux | darwin" "freebsd" @?= False

-- Test edge cases
unit_platformExpression_unknown :: Assertion
unit_platformExpression_unknown =
  evaluatePlatformExpression "unknown_platform" "linux" @?= False

unit_platformExpression_empty :: Assertion
unit_platformExpression_empty =
  evaluatePlatformExpression "" "linux" @?= True  -- Should default to True on parse failure

------------------------------------------------------------------------------------------------
-- Platform-Specific Test Examples
-- 
-- The `platform` function allows you to conditionally run tests based on the current platform.
-- It takes a platform expression string and a TestTree, and returns a TestTree that will only
-- run if the expression evaluates to true for the current platform.
--
-- The `platform` function works best with `tasty_` prefixed tests when using tasty-discover.
-- For custom tests that don't use the `tasty_` prefix, you can apply `platform` directly to TestTrees.
--
-- Platform expression syntax:
--   - Platform names: "linux", "darwin", "windows", "mingw32", "unix"
--   - Logical operators: "&" (AND), "|" (OR), "!" (NOT)
--   - Parentheses for grouping (future enhancement)
--   - Special platform "unix" matches both "linux" and "darwin"
--   - Platform name "windows" is mapped to "mingw32" (the actual System.Info.os value)
--
-- Examples:
--   platform "linux" test          -- Run only on Linux
--   platform "!windows" test       -- Run on all platforms except Windows  
--   platform "!windows & !darwin" test -- Run on platforms that are neither Windows nor Darwin
--   platform "linux | darwin" test     -- Run on Linux or Darwin (Unix-like systems)
--   platform "unix" test               -- Run on Unix-like systems (Linux or Darwin)

-- Simple platform-specific tests using tasty_ prefix
tasty_linuxOnly :: TestTree
tasty_linuxOnly = platform "linux" $ testCase "Linux-specific functionality" $ do
  -- This test only runs on Linux
  pure ()

tasty_notWindows :: TestTree  
tasty_notWindows = platform "!windows" $ testCase "Non-Windows functionality" $ do
  -- This test runs on all platforms except Windows
  -- Print OS info for debugging platform detection
  _ <- error $ "OS check: " ++ show os ++ ", Platform expression '!windows' evaluation: " ++ show (evaluatePlatformExpression "!windows" os)
  pure ()

tasty_unixLike :: TestTree
tasty_unixLike = platform "unix" $ testCase "Unix-like systems" $ do
  -- This test runs on Linux and Darwin (Unix-like systems)
  pure ()

-- Complex platform expressions
tasty_complexPlatform1 :: TestTree
tasty_complexPlatform1 = platform "!windows & !darwin" $ testCase "Neither Windows nor Darwin" $ do
  -- This test runs on platforms that are neither Windows nor Darwin (e.g., Linux)
  pure ()

tasty_complexPlatform2 :: TestTree
tasty_complexPlatform2 = platform "linux | darwin" $ testCase "Linux or Darwin only" $ do
  -- This test runs on either Linux or Darwin, but not Windows
  pure ()

-- Property tests with platform filtering
tasty_platformSpecific :: TestTree
tasty_platformSpecific = platform "!windows" $ testProperty "Property that doesn't work on Windows" $ 
  \(x :: Int) -> x + 0 == x

-- You can also combine platform filtering with other test transformations
-- using the Flavored type and function composition:

tasty_platformAndSkip :: TestTree  
tasty_platformAndSkip = platform "linux" $ skip $ testCase "Linux test that's also skipped" $ do
  -- This would only run on Linux, but it's also skipped, so it never actually runs
  pure ()

-- Test groups with platform filtering
tasty_platformGroup :: TestTree
tasty_platformGroup = platform "unix" $ testGroup "Unix-only tests" 
  [ testCase "Unix test 1" $ pure ()
  , testCase "Unix test 2" $ pure ()
  , testProperty "Unix property" $ \(x :: Int) -> x >= 0 || x < 0
  ]

-- For more advanced use cases, you can use Flavored with platform filtering
-- This allows you to work with custom test types that have Tasty instances

tasty_platformFlavored :: Flavored TestTree
tasty_platformFlavored = flavored (platform "!windows") $ testCase "Advanced platform test" $ do
  -- This uses the Flavored pattern to apply platform filtering
  pure ()

-- You can also create platform-specific custom Property tests
tasty_platformProperty :: Flavored Property  
tasty_platformProperty = flavored (platform "unix") $ property $ do
  -- This hedgehog property only runs on Unix-like systems
  x <- H.forAll $ G.int (R.linear 1 100)
  x H.=== x

------------------------------------------------------------------------------------------------
-- How to use the latest version of tasty-hedgehog

hprop_reverse :: H.Property
hprop_reverse = H.property $ do
  xs <- H.forAll $ G.list (R.linear 0 100) G.alpha
  reverse (reverse xs) H.=== xs

------------------------------------------------------------------------------------------------
-- How to add custom support for golden tests.

data GoldenTest = GoldenTest FilePath (IO ByteString)
  deriving stock (Generic)

instance TD.Tasty GoldenTest where
  tasty info (GoldenTest fp act) = pure $ goldenVsString (TD.descriptionOf info) fp act

tasty_goldenTest :: GoldenTest
tasty_goldenTest = GoldenTest "test/SubMod/example.golden" $ return "test"

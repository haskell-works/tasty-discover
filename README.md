[![CircleCI](https://circleci.com/gh/haskell-works/tasty-discover/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/tasty-discover/tree/master)
[![tasty-discover-nightly](http://stackage.org/package/tasty-discover/badge/nightly)](http://stackage.org/nightly/package/tasty-discover)
[![tasty-discover-lts](http://stackage.org/package/tasty-discover/badge/lts)](http://stackage.org/lts/package/tasty-discover)
[![Hackage Status](https://img.shields.io/hackage/v/tasty-discover.svg)](http://hackage.haskell.org/package/tasty-discover)
[![GitHub license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://raw.githubusercontent.com/haskell-works/tasty-discover/main/LICENSE)

# tasty-discover

- [Getting Started](#getting-started)
  * [Create Test Driver File](#create-test-driver-file)
  * [Configure Cabal or Hpack Test Suite](#configure-cabal-or-hpack-test-suite)
- [Write Tests](#write-tests)
  * [Test Transformations (Flavored, skip, platform)](#test-transformations-flavored-skip-platform)
    - [Flavored (test transformations)](#flavored-test-transformations)
    - [Skipping Tests](#skipping-tests)
    - [Platform-Specific Tests](#platform-specific-tests)
    - [Combining skip and platform](#combining-skip-and-platform)
    - [Using skip and platform (guidelines)](#using-skip-and-platform-guidelines)
  * [Comment Handling](#comment-handling)
- [Customise Discovery](#customise-discovery)
  * [No Arguments](#no-arguments)
  * [With Arguments](#with-arguments)
  * [Custom Main Function](#custom-main-function)
- [Example Project](#example-project)
- [Change Log](#change-log)
- [Deprecation Policy](#deprecation-policy)
- [Contributing](#contributing)
- [FAQ](#frequently-asked-questions)
- [Maintenance](#maintenance)
- [Releasing](#releasing)
- [Acknowledgements](#acknowledgements)
- [AI Guidelines](AI_GUIDELINES.md)
- [Coding Style](CODING_STYLE.md)

Haskell auto-magic test discovery and runner for the [tasty test framework].

[tasty test framework]: https://github.com/feuerbach/tasty

Prefix your test case names and `tasty-discover` will discover, collect and run
them. All popular Haskell test libraries are covered. Configure once then just
write your tests. Remember to add your test modules to your Cabal/Hpack
files. Tasty ingredients are included along with various configuration options
for different use cases.

**Recent improvements include:**
- New `--no-main` option for custom test runners
- **Platform-specific test filtering** with `platform` function and logical expressions
- **Skip test functionality** with `skip` function and yellow `[SKIPPED]` output
- **`Flavored` type** for general-purpose test transformations with extensible design
- Enhanced support for custom test types with `Tasty` instances
- Better handling of backup files and directories in test discovery
- Intelligent block comment handling to prevent false test discovery
- Comprehensive documentation with more test examples

See below for full documentation and examples.

# Getting Started

There are 4 simple steps:

  1. [Create a test driver file in the test directory](#create-test-driver-file)
  2. [Mark the driver file as the `main-is` in the test suite](#configure-cabal-or-hpack-test-suite)
  3. [Mark tests with the correct prefixes](#write-tests)
  4. [Customise test discovery as needed](#customise-discovery)

Check out the [example project](#example-project) to get moving quickly.

## Create Test Driver File

You can name this file anything you want but it must contain the correct
preprocessor definition for `tasty-discover` to run and to detect the
configuration. It should be at the top level of the test directory.

For example (in `test/Driver.hs`):

```
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```
following example, the test driver file is called `Driver.hs`:

```
test-suite test
  type: exitcode-stdio-1.0
  main-is: Driver.hs
  hs-source-dirs: test
  build-depends: base
```

If you use [hpack], that might look like:

[hpack]: https://github.com/sol/hpack

``` yaml
tests:
  test:
    main: "Driver.hs"
    source-dirs: "test"
    dependencies:
    - "base"
```

To ensure that `tasty-discover` is available even without installation, add this
to the test suite in your cabal file:

```
  build-tool-depends:
    tasty-discover:tasty-discover
```

See [`hpack` documentation](https://github.com/sol/hpack) for `stack` equivalent.

# Write Tests

Create test modules and prefix the test function name with an identifier that
corresponds to the testing library you wish to run the test with:

  - **prop_**: [QuickCheck](http://hackage.haskell.org/package/tasty-quickcheck) properties.
  - **scprop_**: [SmallCheck](http://hackage.haskell.org/package/tasty-smallcheck) properties.
  - **hprop_**: [Hedgehog](http://hackage.haskell.org/package/tasty-hedgehog) properties.
  - **unit_**: [HUnit](http://hackage.haskell.org/package/tasty-hunit) test cases.
  - **spec_**: [Hspec](http://hackage.haskell.org/package/tasty-hspec) specifications.
  - **test_**: [Tasty](http://hackage.haskell.org/package/tasty) TestTrees.
  - **tasty_**: Custom tests with [Tasty](http://hackage.haskell.org/package/tasty) instances.

Here is an example test module with a bunch of different tests:

``` haskell
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleTest where

import Data.List
import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Test.Tasty.SmallCheck
import qualified Test.Tasty.Hedgehog as TH
import qualified Hedgehog as H
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

-- HUnit test case
unit_listCompare :: IO ()
unit_listCompare = [1, 2, 3] `compare` [1,2] @?= GT

-- HUnit test case with additional info
unit_listInfo :: IO String
unit_listInfo = return "This test provides info"

-- HUnit test case with steps
unit_listSteps :: (String -> IO ()) -> IO ()
unit_listSteps step = do
  step "Setting up test data"
  step "Running the test"
  [1, 2, 3] `compare` [1,2] @?= GT

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

-- SmallCheck property
scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

-- Hedgehog property
hprop_reverseReverse :: H.Property
hprop_reverseReverse = H.property $ do
  xs <- H.forAll $ G.list (R.linear 0 100) G.alpha
  reverse (reverse xs) H.=== xs

-- Hspec specification
spec_prelude :: Spec
spec_prelude = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)

-- Simple Tasty TestTree
test_addition :: TestTree
test_addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a

-- List of Tasty TestTrees
test_multiplication :: [TestTree]
test_multiplication =
  [ testProperty "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
  , testProperty "One is identity" $ \(a :: Int) -> a * 1 == a
  ]

-- IO Tasty TestTree
test_generateTree :: IO TestTree
test_generateTree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

-- IO List of Tasty TestTrees
test_generateTrees :: IO [TestTree]
test_generateTrees = do
  inputs <- pure ["First input", "Second input"]
  pure $ map (\s -> testCase s $ pure ()) inputs

-- Custom test with Tasty instance
--
-- Write a test for anything with a Tasty instance
-- In order to use Flavored with tasty_ functions, add tasty-discover as a library
-- dependency to your test component in the cabal file.

data CustomTest = CustomTest String Assertion

instance Tasty CustomTest where
  tasty info (CustomTest prefix act) =
    pure $ testCase (prefix ++ descriptionOf info) act

tasty_myTest :: CustomTest
tasty_myTest = CustomTest "Custom: " $ pure ()

-- Custom Tasty TestTree (can be any TestTree)
tasty_customGroup :: TestTree
tasty_customGroup = testGroup "Custom Test Group"
  [ testCase "nested test 1" $ return ()
  , testCase "nested test 2" $ (1 + 1) @?= (2 :: Int)
  ]
```

## Test Transformations (Flavored, skip, platform)

This section covers how to transform tests using the Flavored pattern and how to apply skip and platform filters effectively.

### Flavored (test transformations)

The `Flavored` type provides a general-purpose mechanism for transforming `TestTree`s generated by `tasty_` functions before they are added to the test suite. This allows you to apply various options and modifications to your tests.

You can create flavored tests using the `flavored` constructor:

```haskell
flavored :: (TestTree -> TestTree) -> a -> Flavored a
```

#### Examples

```haskell
import Test.Tasty.Discover (Flavored, flavored, skip)

-- Skip a custom property test
tasty_skipProperty :: Flavored Property
tasty_skipProperty = flavored skip $ property $ do
  -- This test will be skipped and show as [SKIPPED] in yellow
  H.failure
```

When tests are skipped, they will show as `[SKIPPED]` in yellow in the test output and won't actually execute.

### Skipping Tests

You can skip tests using the skip functionality provided by tasty-discover. There are multiple ways to skip tests:

For a high-level overview of when to use flavored vs direct application, see
[Using skip and platform (guidelines)](#using-skip-and-platform-guidelines).

#### Using the `skip` function

You can use the `skip` function to skip any TestTree:

```haskell
import Test.Tasty.Discover (skip)

-- Skip a simple test
test_skipThis :: TestTree
test_skipThis = skip $ testCase "this will be skipped" $ pure ()

-- Skip a property test
prop_skipThis :: Property -> TestTree
prop_skipThis p = skip $ testProperty "skipped property" p
```

**Potential future uses:**
The `Flavored` mechanism is designed to be extensible and could be used for other `TestTree` transformations such as:
- Setting test timeouts
- Adding test metadata or descriptions
- Grouping tests under custom names
- Setting resource dependencies
- Applying multiple transformations in sequence

#### Important: How to skip a tasty_ test

Guideline (TL;DR): To skip a `tasty_` test so it shows `[SKIPPED]` and doesn’t run, wrap it with `flavored skip`:

```haskell
import Test.Tasty.Discover (Flavored, flavored, skip)

-- Skips at the TestTree level (preferred for tasty_ tests)
tasty_mySkipped :: Flavored TestTree
tasty_mySkipped = flavored skip $ testCase "will be skipped" $ pure ()
```

Details (why this matters): Applying `skip` directly to an already-constructed
`TestTree` marks only that subtree as skipped. The test body can detect it via
`askOption`, but the outer `Tasty` instance (used by `tasty_` functions) may not
render a top-level `[SKIPPED]` placeholder because the option is applied after
the instance decides how to wrap the node. Using `flavored skip` applies the
transformation early so the instance can short-circuit and substitute a skipped
placeholder.

For completeness, a direct `skip` example that observes the option inside the test:

```haskell
-- Direct skip on a TestTree: the test can read SkipTest via askOption, but the
-- outer instance may not show a top-level [SKIPPED] marker for this node.
test_directSkip :: TestTree
test_directSkip = skip $ askOption $ \(TD.SkipTest shouldSkip) ->
  testCase "observes SkipTest inside" $ assertBool "expected SkipTest" shouldSkip
```

Using `Flavored` ensures the `skip` is visible to the `Tasty` instance early enough to short-circuit with a skipped placeholder.

```

### Platform-Specific Tests

You can conditionally run tests based on the current operating platform using the `platform` function provided by tasty-discover. This is useful for tests that only work on specific operating systems or need to be excluded from certain platforms.

For general guidance and composition patterns with `skip`, see
[Using skip and platform (guidelines)](#using-skip-and-platform-guidelines).

The `platform` function takes a platform expression string and a `TestTree`, returning a `TestTree` that will only run if the expression evaluates to true for the current platform:

```haskell
import Test.Tasty.Discover (platform)

-- Run only on Linux
tasty_linuxOnly :: TestTree
tasty_linuxOnly = platform "linux" $ testCase "Linux-specific functionality" $ do
  -- This test only runs on Linux
  pure ()

-- Run on all platforms except Windows  
tasty_notWindows :: TestTree  
tasty_notWindows = platform "!windows" $ testCase "Non-Windows functionality" $ do
  -- This test runs on all platforms except Windows
  pure ()

-- Run on Unix-like systems (Linux or Darwin)
tasty_unixLike :: TestTree
tasty_unixLike = platform "unix" $ testCase "Unix-like systems" $ do
  -- This test runs on Linux and Darwin (Unix-like systems)
  pure ()
```

#### Platform Expression Syntax

Platform expressions support the following syntax:

**Platform Names:**
- `"linux"` - Linux systems
- `"darwin"` - macOS systems 
- `"windows"` - Windows systems (mapped to "mingw32" internally)
- `"mingw32"` - Windows systems (actual System.Info.os value)
- `"unix"` - Unix-like systems (matches both "linux" and "darwin")

**Logical Operators:**
- `"!"` (NOT) - Negation, e.g., `"!windows"` means "not Windows"
- `"&"` (AND) - Conjunction, e.g., `"!windows & !darwin"` means "neither Windows nor Darwin"
- `"|"` (OR) - Disjunction, e.g., `"linux | darwin"` means "Linux or Darwin"

**Complex Examples:**
```haskell
-- Run on platforms that are neither Windows nor Darwin (e.g., Linux)
tasty_complexPlatform1 :: TestTree
tasty_complexPlatform1 = platform "!windows & !darwin" $ testCase "Neither Windows nor Darwin" $ do
  pure ()

-- Run on either Linux or Darwin, but not Windows
tasty_complexPlatform2 :: TestTree
tasty_complexPlatform2 = platform "linux | darwin" $ testCase "Linux or Darwin only" $ do
  pure ()
```

#### Using `Flavored` with Platform Filtering

You can combine platform filtering with other test transformations using the `Flavored` type:

```haskell
import Test.Tasty.Discover (Flavored, flavored, platform)

-- Apply platform filtering to custom test types
tasty_platformFlavored :: Flavored TestTree
tasty_platformFlavored = flavored (platform "!windows") $ testCase "Advanced platform test" $ do
  pure ()

-- Platform-specific property test
tasty_platformProperty :: Flavored Property  
tasty_platformProperty = flavored (platform "unix") $ property $ do
  -- This hedgehog property only runs on Unix-like systems
  x <- H.forAll $ G.int (R.linear 1 100)
  x H.=== x
```

#### Combining skip and platform

Platform filtering can be combined with other tasty-discover features:

```haskell
-- Platform filtering with test skipping
tasty_platformAndSkip :: TestTree
tasty_platformAndSkip = platform "linux" $ skip $ testCase "Linux test that's also skipped" $ do
  -- This would only run on Linux, but it's also skipped
  pure ()

-- Platform filtering with test groups
tasty_platformGroup :: TestTree
tasty_platformGroup = platform "unix" $ testGroup "Unix-only tests"
  [ testCase "Unix test 1" $ pure ()
  , testCase "Unix test 2" $ pure ()
  , testProperty "Unix property" $ \(x :: Int) -> x >= 0 || x < 0
  ]
```

Platform filtering works by checking the current platform against the expression at runtime. If the expression evaluates to `False`, the test is automatically skipped using the same mechanism as the `skip` function.

#### Skipping entire test trees with `applySkips`

When you want to skip an entire test tree (such as a group of tests) and have each individual test show as `[SKIPPED]` in the output, use the `applySkips` function:

```haskell
import Test.Tasty.Discover (Flavored, flavored, platform, applySkips)

-- Skip an entire test group on Darwin
tasty_testTree_no_darwin :: Flavored (IO TestTree)
tasty_testTree_no_darwin =
  flavored (platform "!darwin") $ pure $
    applySkips $ testGroup "Non-Darwin group"
      [ testProperty "Test 1" $ \(x :: Int) -> x == x
      , testCase "Test 2" $ pure ()
      , testProperty "Test 3" $ \(x :: Int) -> x >= 0 || x < 0
      ]
```

On Darwin, this will display all tests as skipped in yellow:

```
Non-Darwin group
  Test 1 [SKIPPED]: OK
  Test 2 [SKIPPED]: OK
  Test 3 [SKIPPED]: OK
```

The `applySkips` function:
- Checks the `SkipTest` option (set by functions like `skip` or `platform`)
- Traverses the entire test tree and replaces each individual test with a skipped placeholder
- Preserves the test group structure
- Shows `[SKIPPED]` in yellow for each test

This is particularly useful for platform-specific test suites where you want to see which tests would run on other platforms, rather than hiding the entire group.

### Using skip and platform (guidelines)

TL;DR:
- For tests exposed via `tasty_` functions, prefer the `Flavored` pattern to apply
  transformations like `skip` and `platform` so they take effect at the TestTree level and can short-circuit execution.
- Applying `skip` directly to an already-constructed `TestTree` marks the subtree as skipped. The test can observe this via `askOption`, but the outer `Tasty` instance may not show a top-level `[SKIPPED]` placeholder.

Examples:

```haskell
import Test.Tasty.Discover (Flavored, flavored, skip, platform)
import Test.Tasty (TestTree, testCase)

-- Skip (preferred for tasty_ tests):
tasty_mySkipped :: Flavored TestTree
tasty_mySkipped = flavored skip $ testCase "will be skipped" $ pure ()

-- Platform filter with Flavored:
tasty_linuxOnly :: Flavored TestTree
tasty_linuxOnly = flavored (platform "linux") $ testCase "Linux only" $ pure ()

-- Compose platform and skip:
tasty_linuxButSkipped :: Flavored TestTree
tasty_linuxButSkipped = flavored (platform "linux") $ flavored skip $ testCase "won't run" $ pure ()

-- Direct skip on a TestTree (observed inside the test):
test_directSkip :: TestTree
test_directSkip = skip $ askOption $ \(TD.SkipTest shouldSkip) ->
  testCase "observes SkipTest inside" $ assertBool "expected SkipTest" shouldSkip
```

Platform expressions:
- Names: "linux", "darwin", "windows" (mapped to "mingw32"), "mingw32", and "unix" (matches both linux and darwin)
- Operators: `!` (NOT), `&` (AND), `|` (OR)
- Examples:
  - `platform "!windows & !darwin"` — neither Windows nor Darwin
  - `platform "linux | darwin"` — Linux or Darwin
  - `platform "unix"` — Linux or Darwin

## Test Type Variations

### HUnit Tests (`unit_` prefix)

The `unit_` prefix supports three different function signatures:
  ### Tasty TestTrees (`test_` prefix)
- `unit_testName :: IO ()` - Basic test case
- `unit_testName :: IO String` - Test case that provides additional info
- `unit_testName :: (String -> IO ()) -> IO ()` - Test case with steps

### Tasty TestTrees (`test_` prefix)

The `test_` prefix supports four different function signatures:

- `test_testName :: TestTree` - A single test tree
- `test_testName :: [TestTree]` - A list of test trees (automatically grouped)
- `test_testName :: IO TestTree` - A test tree generated in IO
- `test_testName :: IO [TestTree]` - A list of test trees generated in IO

### Custom Tests (`tasty_` prefix)

The `tasty_` prefix works with any type that has a `Tasty` instance:

- Built-in instances for `TestTree`, `[TestTree]`, `IO TestTree`, `IO [TestTree]`
- Custom instances for your own data types
- Provides access to test metadata through `TastyInfo`

### Organizing Tests with `testGroup`

The `testGroup` function is Tasty's way of organizing tests into hierarchical groups. You can use it in several ways:

**In `test_` functions:**
```haskell
test_arithmeticTests :: TestTree
test_arithmeticTests = testGroup "Arithmetic Operations"
  [ testCase "addition" $ 2 + 2 @?= 4
  , testCase "multiplication" $ 3 * 4 @?= 12
  , testProperty "commutativity" $ \a b -> a + b == b + (a :: Int)
  ]
```

**In `tasty_` functions:**
```haskell
tasty_myTestSuite :: TestTree
tasty_myTestSuite = testGroup "My Test Suite"
  [ testGroup "Unit Tests"
      [ testCase "test 1" $ pure ()
      , testCase "test 2" $ pure ()
      ]
  , testGroup "Properties"
      [ testProperty "prop 1" $ \x -> x == (x :: Int)
      ]
  ]
```

**In list form with `test_` functions:**
```haskell
test_groupedTests :: [TestTree]
test_groupedTests =
  [ testGroup "Group 1" [testCase "test A" $ pure ()]
  , testGroup "Group 2" [testCase "test B" $ pure ()]
  ]
```

This creates nested test hierarchies that make test output more organized and easier to navigate.

## Comment Handling

`tasty-discover` intelligently handles Haskell comments during test discovery to prevent false positives:

### Block Comments

Tests inside multiline block comments are automatically ignored:

```haskell
module MyTest where

-- This test will be discovered
unit_validTest :: IO ()
unit_validTest = pure ()

{- This test will be ignored
unit_commentedOut :: IO ()
unit_commentedOut = pure ()
-}

{- Nested comments are also handled correctly
{- Even deeply nested ones
unit_deeplyNested :: IO ()
unit_deeplyNested = pure ()
-}
unit_alsoIgnored :: IO ()
unit_alsoIgnored = pure ()
-}
```

### Line Comments

Line comments (starting with `--`) are handled by the Haskell lexer and don't interfere with test discovery:

```haskell
-- unit_thisIsIgnored :: IO ()
unit_thisIsFound :: IO ()  -- This test will be discovered
unit_thisIsFound = pure ()
```

This feature prevents compilation errors that would occur if `tasty-discover` tried to reference tests that are commented out, making it easier to temporarily disable tests during development.

# Customise Discovery

You configure `tasty-discover` by passing options to the test driver file.

## No Arguments

Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --debug #-}`

  - **--debug**: Output the contents of the generated module while testing.
  - **--tree-display**: Display the test output results hierarchically.
  - **--no-main**: Generate a module without a main function, exporting `tests` and `ingredients` instead.

## With Arguments

Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --modules="*CustomTest.hs" #-}`

  - **--modules**: Which test modules to discover (with glob pattern).
  - **--search-dir**: Where to look for tests. This is a directory relative
    to the location of the source file. By default, this is the directory
    of the source file."
  - **--ignores**: Which test modules to ignore (with glob pattern).
  - **--generated-module**: The name of the generated test module.
  - **--ingredient**: Tasty ingredients to add to your test runner.
  - **--inplace**: Has the generated code written to the source file.

It is also possible to override [tasty test options] with `-optF`:

[tasty test options]: https://github.com/feuerbach/tasty#options

``` bash
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --hide-successes #-}
```

## Custom Main Function

The `--no-main` option allows you to write your own custom main function while still using tasty-discover for test discovery. This is useful when you need to:

- Apply custom test transformations or wrappers
- Add custom logging or output formatting
- Integrate with custom test runners or CI systems
- Control exactly how tests are executed

### Example Usage

Create your test discovery file (e.g., `test/Tests.hs`):

```haskell
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --no-main -optF --generated-module -optF Tests #-}
```

Then create your custom main file (e.g., `test/Main.hs`):

```haskell
module Main where

import qualified Tests
import qualified Test.Tasty as T

main :: IO ()
main = do
  putStrLn "=== Custom Test Runner ==="

  -- Get discovered tests and ingredients
  discoveredTests <- Tests.tests

  -- Apply custom transformations
  let wrappedTests = T.testGroup "My Custom Tests" [discoveredTests]

  -- Run with custom configuration
  T.defaultMainWithIngredients Tests.ingredients wrappedTests
```

Configure your cabal test suite to use the custom main:

```
test-suite my-tests
  type: exitcode-stdio-1.0
  main-is: Main.hs
  other-modules: Tests
  hs-source-dirs: test
  build-depends: base, tasty
  build-tool-depends: tasty-discover:tasty-discover
```

# Example Project

See the [testing for this package] for a fully configured example.

[testing for this package]: https://github.com/haskell-works/tasty-discover/tree/main/test

# Change Log

Please see the [CHANGELOG.md] for the latest changes.

We try to keep [tagged releases] in our release process, if you care about that.

[CHANGELOG.md]: https://github.com/haskell-works/tasty-discover/blob/main/CHANGELOG.md
[tagged releases]: https://github.com/haskell-works/tasty-discover/releases

# Releasing

This project's release flow is automated via GitHub Actions and triggered by pushing version tags.

Release checklist:

1) Prepare notes
- Update `CHANGELOG.md`: move items from "Unreleased" to a new version section with the current date.

2) Bump version
- Edit `tasty-discover.cabal` and set `version:` to the new version (e.g., `5.x.y`).

3) Commit changes
- Commit the changes: `git commit -m "Release X.Y.Z"`
- Push the commit: `git push origin main`

4) Create and push tag
- Create a git tag: `git tag -a vX.Y.Z -m "Release version X.Y.Z"`
- Push the tag: `git push origin vX.Y.Z`

5) CI does the rest (automated)
- When the tag is pushed, GitHub Actions automatically:
  - Runs the full test suite
  - Validates the cabal project with `cabal check`
  - Builds source distributions (`cabal v2-sdist`)
  - Uploads to Hackage (requires repo secrets `HACKAGE_USER`/`HACKAGE_PASS`)
  - Creates a draft GitHub Release for the tag

6) Publish release
- Go to https://github.com/haskell-works/tasty-discover/releases
- Edit the draft GitHub Release notes if needed and publish

Notes:
- The workflow is defined in `.github/workflows/haskell.yml`.
- The release workflow only triggers on tags matching `v[0-9]+.[0-9]+.[0-9]+` (e.g., v5.2.0).
- Keep `tested-with` in the cabal file up to date with CI's GHC matrix.

# Deprecation Policy

If a breaking change is implemented, you'll see a major version increase, an
entry in the [change log] and a compile-time error with a deprecation warning
and clear instructions on how to upgrade. Please do complain if we're doing
this too much.

[change log]: https://github.com/haskell-works/tasty-discover/blob/main/CHANGELOG.md

# Contributing

All contributions welcome! The continuous integration suite is pretty
comprehensive, so just get hacking and add a test case - there are *plenty* of
examples, so this should be simple - and I'll get to review your change ASAP.

Please follow the guidelines in [CODING_STYLE.md](CODING_STYLE.md) for consistent code formatting and patterns.

For AI assistants and detailed development guidelines, see [AI_GUIDELINES.md](AI_GUIDELINES.md).

# Frequently Asked Questions

## Deleting Tests Breaks The Test Run

This is a known limitation and has been reported. No fix is planned unless you have time.

Please see [#145](https://github.com/haskell-works/tasty-discover/issues/145) for more information.

# Maintenance

If you're interested in helping maintain this package, please let [@newhoggy] know!

It doesn't take much time (max ~3 hours a month) and all we need to do is:

  * Triage issues that are raised.
  * Review pull requests from contributors.
  * Fix bugs when present.
  * Make releases.
  * Manage bounds issues on Stackage.

You can [create an issue] or drop him a line at **lukewm AT riseup DOT NET**.

[@newhoggy]: https://twitter.com/newhoggy
[create an issue]: https://github.com/haskell-works/tasty-discover/issues/new

# Acknowledgements

Thanks to [hspec-discover] and [tasty-auto] for making this possible.

A huge thanks to the growing list of contributors.

[hspec-discover]: https://hspec.github.io/hspec-discover.html
[tasty-auto]: https://github.com/minad/tasty-auto

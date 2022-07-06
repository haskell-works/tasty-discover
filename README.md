[![CircleCI](https://circleci.com/gh/haskell-works/tasty-discover/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/tasty-discover/tree/master)
[![tasty-discover-nightly](http://stackage.org/package/tasty-discover/badge/nightly)](http://stackage.org/nightly/package/tasty-discover)
[![tasty-discover-lts](http://stackage.org/package/tasty-discover/badge/lts)](http://stackage.org/lts/package/tasty-discover)
[![Hackage Status](https://img.shields.io/hackage/v/tasty-discover.svg)](http://hackage.haskell.org/package/tasty-discover)
[![GitHub license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://raw.githubusercontent.com/lwm/tasty-discover/master/LICENSE)

# tasty-discover

- [Getting Started](#getting-started)
  * [Create Test Driver File](#create-test-driver-file)
  * [Configure Cabal or Hpack Test Suite](#configure-cabal-or-hpack-test-suite)
- [Write Tests](#write-tests)
- [Customise Discovery](#customise-discovery)
  * [No Arguments](#no-arguments)
  * [With Arguments](#with-arguments)
- [Example Project](#example-project)
- [Change Log](#change-log)
- [Deprecation Policy](#deprecation-policy)
- [Contributing](#contributing)
- [FAQ](#frequently-asked-questions)
- [Maintenance](#maintenance)
- [Acknowledgements](#acknowledgements)

Haskell auto-magic test discovery and runner for the [tasty test framework].

[tasty test framework]: https://github.com/feuerbach/tasty

Prefix your test case names and `tasty-discover` will discover, collect and run
them. All popular Haskell test libraries are covered. Configure once then just
write your tests. Remember to add your test modules to your Cabal/Hpack
files. Tasty ingredients are included along with various configuration options
for different use cases.

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

## Configure Cabal or Hpack Test Suite

In order for Cabal/Stack to know where the tests are, you'll need to configure
the `main-is` option of your test-suite to point to the driver file. In the
following example, the test driver file is called `Driver.hs`:

```
test-suite test
  main-is: Driver.hs
  hs-source-dirs: test
  build-depends: base
```

If you use [hpack], that might look like:

[hpack]: https://git.coop/sol/hpack

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
  - **tasty_**: Custom tests

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

-- HUnit test case
unit_listCompare :: IO ()
unit_listCompare = [1, 2, 3] `compare` [1,2] @?= GT

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

-- SmallCheck property
scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

-- Hspec specification
spec_prelude :: Spec
spec_prelude = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)

-- Custom test
--
-- Write a test for anything with a Tasty instance
-- 
-- In order to use this feature, you must add tasty-discover as a library dependency
-- to your test component in the cabal file.
--
-- The instance defined should not be an orphaned instance.  A future version of
-- tasty-discover may choose to define orphaned instances for popular test libraries.
import Test.Tasty (testCase)
import Test.Tasty.Discover (TestCase(..), descriptionOf)

data CustomTest = CustomTest String Assertion

instance Tasty CustomTest where
  tasty info (CustomTest prefix act) =
    pure $ testCase (prefix ++ descriptionOf info) act

tasty_myTest :: CustomTest
tasty_myTest = CustomTest "Custom: " $ pure ()

-- Tasty TestTree
test_multiplication :: [TestTree]
test_multiplication = [testProperty "One is identity" $ \(a :: Int) -> a * 1 == a]

-- Tasty IO TestTree
test_generateTree :: IO TestTree
test_generateTree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

-- Tasty IO [TestTree]
test_generateTrees :: IO [TestTree]
test_generateTrees = do
  inputs <- pure ["First input", "Second input"]
  pure $ map (\s -> testCase s $ pure ()) inputs
```

# Customise Discovery

You configure `tasty-discover` by passing options to the test driver file.

## No Arguments

Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --debug #-}`

  - **--debug**: Output the contents of the generated module while testing.
  - **--tree-display**: Display the test output results hierarchically.

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

[tasty test options]: https://git.coop/feuerbach/tasty#options

``` bash
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --hide-successes #-}
```

# Example Project

See the [testing for this package] for a fully configured example.

[testing for this package]: https://git.coop/lwm/tasty-discover/tree/master/test

# Change Log

Please see the [CHANGELOG.md] for the latest changes.

We try to keep [tagged releases] in our release process, if you care about that.

[CHANGELOG.md]: https://git.coop/lwm/tasty-discover/blob/master/CHANGELOG.md
[tagged releases]: https://git.coop/lwm/tasty-discover/releases

# Deprecation Policy

If a breaking change is implemented, you'll see a major version increase, an
entry in the [change log] and a compile-time error with a deprecation warning
and clear instructions on how to upgrade. Please do complain if we're doing
this too much.

[change log]: https://git.coop/lwm/tasty-discover/blob/master/CHANGELOG.md

# Contributing

All contributions welcome! The continuous integration suite is pretty
comprehensive, so just get hacking and add a test case - there are *plenty* of
examples, so this should be simple - and I'll get to review your change ASAP.

# Frequently Asked Questions

## Deleting Tests Breaks The Test Run

This is a known limitation and has been reported. No fix is planned unless you have time.

Please see [#145](https://git.coop/lwm/tasty-discover/issues/145) for more information.

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

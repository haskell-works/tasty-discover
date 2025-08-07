# Coding Style Guide for tasty-discover

This document outlines the coding style and conventions used in the tasty-discover project.

## General Principles

- **Consistency**: Follow existing patterns in the codebase
- **Clarity**: Write code that is easy to read and understand
- **Simplicity**: Prefer simple, straightforward solutions
- **Maintainability**: Consider future developers who will work with the code

## Haskell Style Guidelines

### Module Structure

```haskell
{-# LANGUAGE LanguageExtension #-}
{-# OPTIONS_GHC -Wno-specific-warning #-}

-- | Module documentation
module Module.Name
  ( -- * Types
    TypeName (..)
  , AnotherType

    -- * Functions
  , functionName
  , anotherFunction
  ) where

import Prelude hiding (something)
import qualified Data.Map as Map
import qualified External.Module as Ext

-- Module content here
```

### Language Extensions

- Use `{-# LANGUAGE #-}` pragmas at the top of files
- Prefer explicit extensions over `GHC2021` in library code for compatibility
- Common extensions used in this project:
  - `FlexibleInstances` - For typeclass instances
  - `DeriveGeneric` - For automatic Generic instances
  - `DerivingStrategies` - For explicit deriving strategies

### Imports

```haskell
-- Standard library imports first
import Data.List (sort, nub)
import Data.Maybe (fromJust)

-- Qualified imports with meaningful aliases
import qualified Data.Map as Map
import qualified System.Environment as Env
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

-- Local project imports last
import Test.Tasty.Discover.Internal.Config
```

**Guidelines:**
- Group imports: standard library, external packages, local modules
- Use qualified imports for modules with common names
- Choose short but meaningful aliases (`T` for Test.Tasty, `HU` for HUnit)
- Explicit import lists for functions to avoid namespace pollution

### Type Signatures

```haskell
-- Always include top-level type signatures
generateTestDriver :: Config -> String -> [String] -> FilePath -> [Test] -> String

-- Use meaningful parameter names
processTests :: [Test] -> IO [TestTree]

-- Document complex types
type TestGenerator = Test -> String  -- ^ Converts test to code string
```

### Function Definitions

```haskell
-- Simple functions
isTestFile :: FilePath -> Bool
isTestFile path = takeExtension path == ".hs"

-- Complex functions with guards
generateImports :: Config -> [Generator] -> [String]
generateImports config generators
  | noMain config = baseImports
  | otherwise     = baseImports ++ mainImports
  where
    baseImports = ["import Prelude", "import qualified Test.Tasty as T"]
    mainImports = ["import qualified System.Environment as E"]

-- Pattern matching
processGenerator :: Generator -> String
processGenerator Generator{generatorPrefix = prefix, generatorSetup = setup} =
  "-- Generator for " ++ prefix ++ "\n" ++ setup
```

### Data Types

```haskell
-- Record syntax with meaningful field names
data Config = Config
  { configDebug       :: Bool
  , configModules     :: String
  , configNoMain      :: Bool
  , configIngredients :: [String]
  } deriving stock (Eq, Show, Generic)

-- Simple ADTs
data TestType
  = UnitTest
  | PropertyTest
  | CustomTest
  deriving stock (Eq, Show, Ord)
```

### Comments and Documentation

```haskell
-- | Generate the main test driver module.
--
-- This function creates a complete Haskell module that either:
-- * Exports and runs tests via main (default)
-- * Exports tests and ingredients for custom runners (--no-main)
generateTestDriver
  :: Config       -- ^ Configuration options
  -> String       -- ^ Module name
  -> [String]     -- ^ Ingredient imports
  -> FilePath     -- ^ Source file path
  -> [Test]       -- ^ Discovered tests
  -> String       -- ^ Generated module code

-- Single-line comments for implementation details
sortTests :: [Test] -> [Test]
sortTests tests =
  sortOn testModule tests  -- Sort by module name first
```

### String Handling

```haskell
-- Prefer explicit concatenation for simple cases
errorMsg = "Error in module " ++ moduleName ++ ": " ++ details

-- Use unlines for multi-line generation
generateModule :: [String] -> String
generateModule sections = unlines
  [ "{-# LANGUAGE FlexibleInstances #-}"
  , ""
  , "module " ++ moduleName ++ " where"
  , ""
  , unlines imports
  ]

-- Use here-docs or concat for complex templates
templateCode = concat
  [ "main :: IO ()\n"
  , "main = do\n"
  , "  tests >>= T.defaultMain\n"
  ]
```

### Error Handling

```haskell
-- Use Maybe for optional values
findGenerator :: String -> [Generator] -> Maybe Generator
findGenerator prefix = find ((prefix `isPrefixOf`) . generatorPrefix)

-- Use Either for operations that can fail
parseConfig :: [String] -> Either String Config
parseConfig args = case getOpt Permute options args of
  (opts, [], []) -> Right $ foldr ($) defaultConfig opts
  (_, _, errs)   -> Left $ concat errs

-- Use error/undefined sparingly, with descriptive messages
generateTest :: Test -> String
generateTest test = case getGenerator test of
  Just gen -> generatorSetup gen test
  Nothing  -> error $ "No generator found for test: " ++ show test
```

## Project-Specific Conventions

### Code Generation

```haskell
-- Use consistent indentation for generated code
generateTestCase :: Test -> String
generateTestCase test = unlines
  [ "  t" ++ show index ++ " <- " ++ setup
  , ""
  ]
  where
    setup = generatorSetup (getGenerator test) test

-- Generate imports in sorted order
generateImports :: [String] -> String
generateImports = unlines . nub . sort
```

### CLI Option Handling

```haskell
-- Use consistent option parsing patterns
data Config = Config
  { optDebug   :: Bool
  , optNoMain  :: Bool
  , optModules :: String
  } deriving stock (Eq, Show)

-- Option descriptors with clear help text
options :: [OptDescr (Config -> Config)]
options =
  [ Option [] ["debug"]
      (NoArg $ \c -> c {optDebug = True})
      "Output generated module contents"
  , Option [] ["no-main"]
      (NoArg $ \c -> c {optNoMain = True})
      "Generate module without main function"
  ]
```

### Test Code Organization

```haskell
-- Group related tests in modules
module ConfigTest where

-- Use descriptive test names
unit_parseValidConfig :: IO ()
unit_parseValidConfig = do
  let result = parseConfig ["--debug", "--modules=Test*.hs"]
  result @?= Right expectedConfig

prop_configRoundTrip :: Config -> Bool
prop_configRoundTrip config =
  parseConfig (configToArgs config) == Right config

-- Use meaningful test data
spec_configParsing :: Spec
spec_configParsing = describe "Config parsing" $ do
  it "handles empty arguments" $ do
    parseConfig [] `shouldBe` Right defaultConfig

  it "rejects invalid options" $ do
    parseConfig ["--invalid"] `shouldSatisfy` isLeft
```

## File Organization

### Directory Structure
```
src/
  Test/Tasty/Discover/
    Internal/
      Config.hs       -- CLI configuration
      Driver.hs       -- Code generation
      Generator.hs    -- Test generators
app/
  Main.hs            -- CLI entry point
test/
  ConfigTest.hs      -- Config tests
  DiscoverTest.hs    -- Discovery tests
test-no-main/        -- Feature demonstration
```

### Module Naming
- Use qualified module names: `Test.Tasty.Discover.Internal.Config`
- Keep modules focused on single responsibilities
- Use `Internal` for implementation details not part of public API

## Tools and Automation

### HLint Configuration
- Follow suggestions from `.hlint.yaml`
- Common rules we follow:
  - Use `fmap` instead of `map` in functor contexts
  - Prefer `when` over `if ... then ... else ()`
  - Use explicit imports over hiding

### Stylish Haskell
- Configuration in `.stylish-haskell.yaml`
- Automatic import sorting and alignment
- Consistent record formatting
- Remove trailing whitespace from lines

### Build Warnings
- Treat warnings seriously, aim for warning-free builds
- Use `{-# OPTIONS_GHC -Wno-... #-}` sparingly and with justification
- Common warnings to avoid:
  - Unused imports
  - Incomplete patterns
  - Type defaults

### Formatting Rules
- Lines should not end with unnecessary whitespace
- Use consistent indentation (spaces, not tabs)
- Limit line length to reasonable bounds (typically 80-100 characters)
- End files with a single newline character

## Code Review Guidelines

### Before Submitting
1. Run `cabal build` - ensure clean build
2. Run `cabal test` - all tests pass
3. Check `hlint src/ app/ test/` - address suggestions
4. Verify formatting with stylish-haskell
5. Update documentation if needed

### Review Checklist
- [ ] Follows existing code patterns
- [ ] Includes appropriate type signatures
- [ ] Has meaningful variable/function names
- [ ] Includes tests for new functionality
- [ ] Updates documentation if needed
- [ ] Handles errors appropriately
- [ ] No unused imports or functions

---

*This style guide should evolve with the project. Propose changes via pull requests.*

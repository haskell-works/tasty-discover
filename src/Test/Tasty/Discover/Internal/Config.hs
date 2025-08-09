-- | The test driver configuration options module.
--
-- Anything that can be passed as an argument to the test driver
-- definition exists as a field in the 'Config' type.

module Test.Tasty.Discover.Internal.Config
  ( -- * Configuration Options
    Config (..)
  , GlobPattern
  , SkipTest (..)
  , OnPlatform (..)
  , checkPlatform
  , onLinux
  , onDarwin
  , onWindows
  , onUnix

    -- * Configuration Parser
  , parseConfig

    -- * Configuration Defaults
  , defaultConfig
  ) where

import Data.Maybe            (isJust)
import GHC.Generics          (Generic)
import System.Console.GetOpt (ArgDescr (NoArg, ReqArg), ArgOrder (Permute), OptDescr (Option), getOpt')
import System.FilePath ((</>))
import System.Info (os)
import Test.Tasty.Options (IsOption (..), safeRead)

-- | A tasty ingredient.
type Ingredient = String

-- | A glob pattern.
type GlobPattern = String

-- | Newtype wrapper for skip test option.
--
-- This option type integrates with Tasty's option system to control whether
-- tests should be skipped. When set to @SkipTest True@, tests will show as
-- @[SKIPPED]@ in yellow in the test output and won't actually execute.
--
-- Used internally by the 'skip' function and 'Flavored' type to implement
-- test skipping functionality.
newtype SkipTest = SkipTest Bool
  deriving stock (Show, Eq, Generic)

instance IsOption SkipTest where
  defaultValue = SkipTest False
  parseValue = fmap SkipTest . safeRead
  optionName = return "skip-test"
  optionHelp = return "Skip test execution (useful for debugging test discovery)"

-- | Newtype wrapper for platform-specific test filtering.
--
-- This option type allows tests to be conditionally executed based on platform
-- criteria. The wrapped function takes a platform string and returns whether
-- the test should run on that platform.
--
-- Platform values correspond to System.Info.os:
-- - "linux" for Linux systems
-- - "darwin" for macOS
-- - "mingw32" for Windows (GHC compiled)
-- - "unix" matches both "linux" and "darwin"
--
-- Example usage:
-- @
-- -- Only run on Linux
-- onLinux :: OnPlatform
-- onLinux = OnPlatform (== "linux")
--
-- -- Run on Unix-like systems
-- onUnix :: OnPlatform
-- onUnix = OnPlatform (\p -> p `elem` ["linux", "darwin"])
-- @
newtype OnPlatform = OnPlatform (String -> Bool)

instance IsOption OnPlatform where
  defaultValue = OnPlatform (const True)  -- Run on all platforms by default
  parseValue s = case s of
    "linux"   -> Just $ OnPlatform (== "linux")
    "darwin"  -> Just $ OnPlatform (== "darwin")
    "mingw32" -> Just $ OnPlatform (== "mingw32")  -- Windows with GHC
    "windows" -> Just $ OnPlatform (== "mingw32")  -- Alias for mingw32
    "unix"    -> Just $ OnPlatform (\p -> p `elem` ["linux", "darwin"])
    _         -> Nothing
  optionName = return "on-platform"
  optionHelp = return "Run test only on specified platform (linux|darwin|mingw32|windows|unix)"

-- | Check if the current platform matches the OnPlatform criteria
checkPlatform :: OnPlatform -> Bool
checkPlatform (OnPlatform f) = f os

-- | Helper function: only run on Linux
onLinux :: OnPlatform
onLinux = OnPlatform (== "linux")

-- | Helper function: only run on macOS
onDarwin :: OnPlatform
onDarwin = OnPlatform (== "darwin")

-- | Helper function: only run on Windows (mingw32)
onWindows :: OnPlatform
onWindows = OnPlatform (== "mingw32")

-- | Helper function: only run on Unix-like systems (Linux or macOS)
onUnix :: OnPlatform
onUnix = OnPlatform (\p -> p `elem` ["linux", "darwin"])

-- | The discovery and runner configuration.
data Config = Config
  { modules             :: Maybe GlobPattern -- ^ Glob pattern for matching modules during test discovery.
  , moduleSuffix        :: Maybe String      -- ^ <<<DEPRECATED>>>: Module suffix.
  , searchDir           :: FilePath          -- ^ Directory where to look for tests.
  , generatedModuleName :: Maybe String      -- ^ Name of the generated main module.
  , ignores             :: Maybe GlobPattern -- ^ Glob pattern for ignoring modules during test discovery.
  , ignoredModules      :: [FilePath]        -- ^ <<<DEPRECATED>>>: Ignored modules by full name.
  , tastyIngredients    :: [Ingredient]      -- ^ Tasty ingredients to use.
  , tastyOptions        :: [String]          -- ^ Options passed to tasty
  , inPlace             :: Bool              -- ^ Whether the source file should be modified in-place.
  , noModuleSuffix      :: Bool              -- ^ <<<DEPRECATED>>>: suffix and look in all modules.
  , debug               :: Bool              -- ^ Debug the generated module.
  , treeDisplay         :: Bool              -- ^ Tree display for the test results table.
  , noMain              :: Bool              -- ^ Whether to generate main function.
  } deriving stock (Show, Generic)

-- | The default configuration
defaultConfig :: FilePath -> Config
defaultConfig theSearchDir = Config Nothing Nothing theSearchDir Nothing Nothing [] [] [] False False False False False

-- | Deprecation message for old `--[no-]module-suffix` option.
moduleSuffixDeprecationMessage :: String
moduleSuffixDeprecationMessage = error $ concat
  [ "\n\n"
  , "----------------------------------------------------------\n"
  , "DEPRECATION NOTICE: `--[no-]module-suffix` is deprecated.\n"
  , "The default behaviour now discovers all test module suffixes.\n"
  , "Please use the `--modules='<glob-pattern>'` option to specify.\n"
  , "----------------------------------------------------------\n"
  ]

-- | Deprecation message for old `--ignore-module` option.
ignoreModuleDeprecationMessage :: String
ignoreModuleDeprecationMessage = error $ concat
  [ "\n\n"
  , "----------------------------------------------------------\n"
  , "DEPRECATION NOTICE: `--ignore-module` is deprecated.\n"
  , "Please use the `--ignores='<glob-pattern>'` option instead.\n"
  , "----------------------------------------------------------\n"
  ]

-- | Configuration options parser.
parseConfig :: FilePath -> String -> [String] -> Either String Config
parseConfig srcDir prog args = case getOpt' Permute (options srcDir) args of
  (opts, rest, rest', []) ->
    let config = foldl (flip id) (defaultConfig srcDir) { tastyOptions = rest ++ rest' } opts in
      if noModuleSuffix config || isJust (moduleSuffix config)
        then error moduleSuffixDeprecationMessage
        else if not $ null (ignoredModules config)
          then error ignoreModuleDeprecationMessage
          else Right config
  (_, _, _, err:_)  -> formatError err
  where formatError err = Left (prog ++ ": " ++ err)

-- | All configuration options.
options :: FilePath -> [OptDescr (Config -> Config)]
options srcDir =
  [ Option [] ["modules"]
      (ReqArg (\s c -> c {modules = Just s}) "GLOB-PATTERN")
      "Specify desired modules with a glob pattern (white-list)"
  , Option [] ["module-suffix"]
      (ReqArg (\s c -> c {moduleSuffix = Just s}) "SUFFIX")
      "<<<DEPRECATED>>>: Specify desired test module suffix"
  , Option [] ["search-dir"]
      (ReqArg (\s c -> c {searchDir = srcDir </> s}) "DIR")
      "Directory where to look for tests relative to the directory of src. By default, this is the directory of src."
  , Option [] ["generated-module"]
      (ReqArg (\s c -> c {generatedModuleName = Just s}) "MODULE")
      "Qualified generated module name"
  , Option [] ["ignores"]
      (ReqArg (\s c -> c {ignores = Just s}) "GLOB-PATTERN")
      "Specify desired modules to ignore with a glob pattern (black-list)"
  , Option [] ["ignore-module"]
      (ReqArg (\s c -> c {ignoredModules = s : ignoredModules c}) "FILE")
      "<<<DEPRECATED>>>: Ignore a test module"
  , Option [] ["ingredient"]
      (ReqArg (\s c -> c {tastyIngredients = s : tastyIngredients c}) "INGREDIENT")
      "Qualified tasty ingredient name"
  , Option [] ["in-place"]
      (NoArg $ \c -> c {inPlace = True})
      "Whether the source file should be modified in-place"
  , Option [] ["no-module-suffix"]
      (NoArg $ \c -> c {noModuleSuffix = True})
      "<<<DEPRECATED>>>: Ignore test module suffix and import them all"
  , Option [] ["debug"]
      (NoArg $ \c -> c {debug = True})
      "Debug output of generated test module"
  , Option [] ["tree-display"]
      (NoArg $ \c -> c {treeDisplay = True})
      "Display test output hierarchically"
  , Option [] ["no-main"]
      (NoArg $ \c -> c {noMain = True})
      "Do not generate a main function"
  ]

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigTest where

import Data.List                              (isInfixOf, isSuffixOf, sort)
import Test.Tasty.Discover.Internal.Config
import Test.Tasty.Discover.Internal.Driver    (ModuleTree (..), findTests, generateTestDriver, mkModuleTree, showTests, extractTests)
import Test.Tasty.Discover.Internal.Generator (Test (..), mkTest)

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Hspec.Core.Spec (Spec, describe, it)

import Test.Hspec (shouldBe, shouldSatisfy)

import qualified Data.Map.Strict as M

spec_modules :: Spec
spec_modules = describe "Test discovery" $ do
  it "Discovers tests" $ do
    let expectedTests = [ mkTest "PropTest.hs" "prop_additionAssociative"
                        , mkTest "SubSubMod/PropTest.hs" "prop_additionCommutative"
                        ]
        config        = (defaultConfig "test/SubMod") { modules = Just "*Test.hs" }
    discoveredTests <- findTests config
    sort discoveredTests `shouldBe` sort expectedTests

spec_ignores :: Spec
spec_ignores = describe "Module ignore configuration" $ do
  it "Ignores tests in modules with the specified suffix" $ do
    let ignoreModuleConfig = (defaultConfig "test/SubMod") { ignores = Just "*.hs" }
    discoveredTests <- findTests ignoreModuleConfig
    discoveredTests `shouldBe` []

spec_badModuleGlob :: Spec
spec_badModuleGlob = describe "Module suffix configuration" $ do
  it "Filters discovered tests by specified suffix" $ do
    let badGlobConfig = (defaultConfig "test/SubMod") { modules = Just "DoesntExist*.hs" }
    discoveredTests <- findTests badGlobConfig
    discoveredTests `shouldBe` []

spec_backupFilesIgnored :: Spec
spec_backupFilesIgnored = describe "Backup file filtering" $ do
  it "Only matches .hs files, not backup files containing .hs" $ do
    let config = defaultConfig "test/BackupFiles"
    discoveredTests <- findTests config
    -- Should only find ValidTest.hs, not ValidTest.hs.orig or ValidTest.hs.bak
    let moduleNames = map testModule discoveredTests
    length discoveredTests `shouldBe` 1
    moduleNames `shouldBe` ["ValidTest"]
    -- Verify no backup file patterns were found
    let hasBackupPattern name = ".hs." `isInfixOf` name || ".hs" `isInfixOf` name && not (".hs" `isSuffixOf` name)
    moduleNames `shouldSatisfy` (not . any hasBackupPattern)

spec_modulesGlobIgnoresDirectories :: Spec
spec_modulesGlobIgnoresDirectories = describe "Modules glob directory handling" $ do
  it "Ignores directories that match the glob pattern" $ do
    let config = (defaultConfig "test/ModulesGlob") { modules = Just "*" }
    discoveredTests <- findTests config
    -- Should find both test files, not fail on the Sub directory
    let moduleNames = sort $ map testModule discoveredTests
    length discoveredTests `shouldBe` 2
    moduleNames `shouldBe` ["Sub.OneTest", "TwoTest"]

spec_customModuleName :: Spec
spec_customModuleName = describe "Module name configuration" $ do
  it "Creates a generated main function with the specified name" $ do
    let generatedModule = generateTestDriver (defaultConfig "test/") "FunkyModuleName" [] "test/" []
    "FunkyModuleName" `shouldSatisfy` (`isInfixOf` generatedModule)

unit_noTreeDisplayDefault :: IO ()
unit_noTreeDisplayDefault = do
  let config = defaultConfig "test/SubMod"
  tests <- findTests config
  let testNumVars = map (('t' :) . show) [(0::Int)..]
      trees = showTests config tests testNumVars
  length trees @?= 4

unit_treeDisplay :: IO ()
unit_treeDisplay = do
  let config = (defaultConfig "test/SubMod") { treeDisplay = True }
  tests <- findTests config
  let testNumVars = map (('t' :) . show) [(0::Int)..]
      trees = showTests config tests testNumVars
  length trees @?= 3

prop_mkModuleTree :: ModuleTree -> Property
prop_mkModuleTree mtree =
  let (tests, testVars) = unzip $ flattenTree mtree
  in mkModuleTree tests testVars === mtree
  where flattenTree (ModuleTree mp) = M.assocs mp >>= flattenModule
        flattenModule (mdl, (subTree, testVars)) = concat
          [ map (\(Test subMdl _, tVar) -> (Test (mdl ++ '.':subMdl) "-", tVar)) (flattenTree subTree)
          , map (Test mdl "-", ) testVars ]

instance Arbitrary ModuleTree where
  arbitrary = sized $ \size ->
    resize (min size 12) (ModuleTree . M.fromList <$> listOf1 mdlGen)
    where mdlGen = sized $ \size -> do
            mdl <- listOf1 (elements ['a'..'z'])
            subTree <- if size == 0
              then pure $ ModuleTree M.empty
              else resize (size `div` 2) arbitrary
            tVars <- listOf1 (listOf1 arbitrary)
            pure (mdl, (subTree, tVars))

spec_commentHandling :: Spec
spec_commentHandling = describe "Comment handling" $ do
  it "ignores tests in block comments" $ do
    let content = unlines
          [ "module Test where"
          , "{- block comment"
          , "test_ignored :: TestTree"
          , "test_ignored = testCase \"ignored\" $ pure ()"
          , "-}"
          , "test_valid :: TestTree"
          , "test_valid = testCase \"valid\" $ pure ()"
          ]
    let tests = extractTests "Test.hs" content
        testNames = map testFunction tests
    testNames `shouldBe` ["test_valid"]

  it "ignores tests in nested block comments" $ do
    let content = unlines
          [ "module Test where"
          , "{- outer comment"
          , "test_outerIgnored :: TestTree"
          , "  {- inner comment"
          , "test_innerIgnored :: TestTree"
          , "  -}"
          , "test_stillOuterIgnored :: TestTree"
          , "-}"
          , "test_valid :: TestTree"
          ]
    let tests = extractTests "Test.hs" content
        testNames = map testFunction tests
    testNames `shouldBe` ["test_valid"]

  it "correctly handles line comments (existing behavior)" $ do
    let content = unlines
          [ "module Test where"
          , "-- test_lineIgnored :: TestTree"
          , "-- test_lineIgnored = testCase \"ignored\" $ pure ()"
          , "test_valid :: TestTree"
          , "test_valid = testCase \"valid\" $ pure ()"
          ]
    let tests = extractTests "Test.hs" content
        testNames = map testFunction tests
    testNames `shouldBe` ["test_valid"]

  it "finds multiple valid tests correctly" $ do
    let content = unlines
          [ "module Test where"
          , "test_first :: TestTree"
          , "{-"
          , "test_ignored :: TestTree"
          , "-}"
          , "test_second :: TestTree"
          , "test_third :: TestTree"
          ]
    let tests = extractTests "Test.hs" content
        testNames = sort $ map testFunction tests
    testNames `shouldBe` sort ["test_first", "test_second", "test_third"]

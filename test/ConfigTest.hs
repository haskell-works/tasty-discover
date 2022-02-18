{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigTest where

import Data.List             (isInfixOf, sort)
import Test.Tasty.Config
import Test.Tasty.Discover   (ModuleTree (..), findTests, generateTestDriver, mkModuleTree, showTests)
import Test.Tasty.Generator  (Test (..), mkTest)
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

#if MIN_VERSION_tasty_hspec(1,1,7)
import Test.Hspec
#endif

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

module BackupFiles.ValidTest where

-- This module is part of the regression test for spec_backupFilesIgnored
-- in ConfigTest.hs. The accompanying .hs.orig and .hs.bak files test
-- that tasty-discover only processes .hs files and ignores backup files.

prop_validTest :: Bool -> Bool
prop_validTest _ = True

module Test.PursInstaller.Spec where

import Prelude

import Data.Identity (Identity)
import Effect.Aff (Aff)
import Test.PursInstaller.Setup (TestDirs, check, isSuccess)
import Test.Spec (SpecT, describe, it)

spec :: SpecT Aff TestDirs Identity Unit
spec = do
  describe "spec" do
    it "binary is executable" \ { pursInstaller } -> do
      pursInstaller [ "--version" ] >>= check { stdout: mempty, stderr: mempty, result: isSuccess }
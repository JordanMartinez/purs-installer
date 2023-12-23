module Test.PursInstaller.Spec where

import Prelude

import Data.Foldable (for_)
import Data.Identity (Identity)
import Effect.Aff (Aff)
import Test.PursInstaller.Setup (TestDirs, check, isSuccess)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions.String (shouldContain)

spec :: SpecT Aff TestDirs Identity Unit
spec = do
  it "binary is executable" \{ pursInstaller } -> do
    pursInstaller [ "--version" ] >>= check { stdout: mempty, stderr: mempty, result: isSuccess }
  describe "can install PureScript" do
    let
      versions = 
        [ "0.15.13"
        -- , "0.15.0"
        -- , "0.14.9"
        -- , "0.14.0"
        -- , "0.13.8"
        -- , "0.13.0"
        ]
    for_ versions \version -> do
      it (version <> ": Initial install") \{ pursInstaller, purs, checkFixture } -> do
        let
          checkStdout = checkFixture (version <> "-initial-stdout")
          checkStderr = checkFixture (version <> "-initial-stderr")
        pursInstaller [ "--purs-version", version ] >>= check { stdout: checkStdout, stderr: checkStderr, result: isSuccess }
        purs [ "--version" ] >>= check { stdout: shouldContain version, stderr: mempty, result: isSuccess }

      it (version <> ": Cached install") \{ pursInstaller, purs, checkFixture } -> do
        let
          checkStdout = checkFixture (version <> "-cached-stdout")
          checkStderr = checkFixture (version <> "-cached-stderr")
        pursInstaller [ "--purs-version", version ] >>= check { stdout: checkStdout, stderr: checkStderr, result: isSuccess }
        purs [ "--version" ] >>= check { stdout: shouldContain version, stderr: mempty, result: isSuccess }
  
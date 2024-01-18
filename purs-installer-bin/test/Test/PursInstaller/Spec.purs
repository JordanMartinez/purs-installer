module Test.PursInstaller.Spec where

import Prelude

import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Version (Version, isPreRelease, parseVersion, showVersion)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Platform (Platform(..))
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import PursInstaller.Lib (accountForMacPreReleaseBug)
import Test.PursInstaller.Setup (TestDirs, check, isSuccess)
import Test.Spec (SpecT, it)
import Test.Spec.Assertions.String (shouldContain)

spec :: SpecT Aff TestDirs Effect Unit
spec = do
  it "binary is executable" \{ pursInstaller } -> do
    pursInstaller [ "--version" ] >>= check { stdout: mempty, stderr: mempty, result: isSuccess }

  let 
    isLinux = Just Linux == Process.platform
    fixtureSuffix hasLibTInfo
      | isLinux && hasLibTInfo = "-libtinfo"
      | otherwise = "-normal"


  for_ versions \info@{ version, hasLibTInfo } -> do
    it ("Initial install: " <> toTestTitle info) \{ pursInstaller, purs, checkFixture } -> do
      let 
        installVersion = showVersion version
        expectedVersion = showVersion $ fromMaybe version $ accountForMacPreReleaseBug version
      pursInstaller [ "install", "--purs-version", installVersion ] >>= check 
        { stdout: checkFixture (installVersion <> "-initial-stdout" <> fixtureSuffix hasLibTInfo)
        , stderr: mempty
        , result: isSuccess 
        }
      unless (hasLibTInfo && isLinux) do
        purs [ "--version" ] >>= check { stdout: shouldContain expectedVersion, stderr: mempty, result: isSuccess }

  for_ versions \info@{ version, hasLibTInfo } -> do
    it ("Cached install: " <> toTestTitle info) \{ pursInstaller, purs, checkFixture } -> do
      let 
        installVersion = showVersion version
        expectedVersion = showVersion $ fromMaybe version $ accountForMacPreReleaseBug version
      pursInstaller [ "install", "--purs-version", installVersion ] >>= check 
        { stdout: checkFixture (installVersion <> "-cache-stdout" <> fixtureSuffix hasLibTInfo)
        , stderr: mempty
        , result: isSuccess 
        }
      unless (hasLibTInfo && isLinux) do
        purs [ "--version" ] >>= check { stdout: shouldContain expectedVersion, stderr: mempty, result: isSuccess }
  where
    toTestTitle r = showVersion r.version <> case r.hasLibTInfo, isPreRelease r.version of
      true, true -> " (libtinfo; prerelease)"
      false, true -> " (prerelease)"
      true, false -> " (libtinfo)"
      false, false -> ""

    versions :: Array { version :: Version, hasLibTInfo :: Boolean }
    versions =
      [ { version: v "0.15.14", hasLibTInfo: false }
      , { version: v "0.15.2", hasLibTInfo: false }
      , { version: v "0.15.1-0", hasLibTInfo: false }
      , { version: v "0.15.0-alpha-07", hasLibTInfo: false }
      , { version: v "0.14.9", hasLibTInfo: false }
      , { version: v "0.14.0", hasLibTInfo: true }
      ]
      where
      v = either (\e -> unsafeCrashWith $ "Invalid version: " <> show e) identity <<< parseVersion
  
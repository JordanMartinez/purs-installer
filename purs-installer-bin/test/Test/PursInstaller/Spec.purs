module Test.PursInstaller.Spec where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Node.ChildProcess.Types (Exit(..), pipe)
import Node.Library.Execa (execa)
import Node.Process (platformStr)
import Node.Process as Process
import Record as Record
import Test.PursInstaller.Setup (TestDirs, check, isFailure, isSuccess)
import Test.Spec (SpecT, beforeAll_, it)
import Test.Spec.Assertions.String (shouldContain)
import Type.Proxy (Proxy(..))

spec :: SpecT Aff TestDirs Effect Unit
spec = do
  it "binary is executable" \{ pursInstaller } -> do
    pursInstaller [ "--version" ] >>= check { stdout: mempty, stderr: mempty, result: isSuccess }
  let
    { platformPrefix, isWindows } = 
      if platformStr == "win32" then { platformPrefix: "-win", isWindows: true }
      else { platformPrefix: "-nix", isWindows: false }

  for_ versions \info@{ version, hasLibTInfo } -> do
    it ("Initial install: " <> toTestTitle info) \{ pursInstaller, purs, checkFixture } -> do
      if (hasLibTInfo && not isWindows) then do
        pursInstaller [ "install", "--purs-version", version ] >>= check 
          { stdout: checkFixture (version <> platformPrefix <> "-initial-libtinfo-stdout")
          , stderr: mempty
          , result: isFailure 
          }
      else do
        pursInstaller [ "install", "--purs-version", version ] >>= check 
          { stdout: checkFixture (version <> platformPrefix <> "-initial-stdout")
          , stderr: mempty
          , result: isSuccess 
          }
        purs [ "--version" ] >>= check { stdout: shouldContain version, stderr: mempty, result: isSuccess }

  for_ versions \info@{ version, hasLibTInfo } -> do
    unless (hasLibTInfo && not isWindows) do 
      it ("Cached install: " <> toTestTitle info) \{ pursInstaller, purs, checkFixture } -> do
        pursInstaller [ "install", "--purs-version", version ] >>= check 
          { stdout: checkFixture (version <> platformPrefix <> "-cached-stdout")
          , stderr: mempty
          , result: isSuccess 
          }
        purs [ "--version" ] >>= check { stdout: shouldContain version, stderr: mempty, result: isSuccess }
  allowLibtinfoInstall <- liftEffect $ map isJust $ Process.lookupEnv "ALLOW_LIBTINFO_INSTALL"
  log $ "Not running tests involving versions that depend on libtinfo. To run, set the `ALLOW_LIBTINFO_INSTALL` env variable to any content"
  when (not isWindows && allowLibtinfoInstall) do
    beforeAll_ installLibtinfo do
      let libtinfoVersions = Array.filter _.hasLibTInfo versions
      for_ libtinfoVersions \info@{ version } -> do
        it ("Initial install (post-libtinfo-install): " <> toTestTitle info) \{ pursInstaller, purs, checkFixture } -> do
            pursInstaller [ "install", "--purs-version", version ] >>= check 
              { stdout: checkFixture (version <> platformPrefix <> "-post-libtinfo-install-initial-stdout")
              , stderr: mempty
              , result: isSuccess 
              }
            purs [ "--version" ] >>= check { stdout: shouldContain version, stderr: mempty, result: isSuccess }

      for_ libtinfoVersions \info@{ version } -> do
        it ("Cached install (post-libtinfo-install): " <> toTestTitle info) \{ pursInstaller, purs, checkFixture } -> do
          pursInstaller [ "install", "--purs-version", version ] >>= check 
            { stdout: checkFixture (version <> platformPrefix <> "-post-libtinfo-install-cached-stdout")
            , stderr: mempty
            , result: isSuccess 
            }
          purs [ "--version" ] >>= check { stdout: shouldContain version, stderr: mempty, result: isSuccess }
  where
    toTestTitle r = r.version <> case r.hasLibTInfo, r.isPrerelease of
      true, true -> " (libtinfo; prerelease)"
      false, true -> " (prerelease)"
      true, false -> " (libtinfo)"
      false, false -> ""

    versions :: Array { version :: String, hasLibTInfo :: Boolean, isPrerelease :: Boolean }
    versions = map (\r -> Record.insert (Proxy :: Proxy "isPrerelease") (isPrerelease r) r)
      [ { version: "0.15.14", hasLibTInfo: false }
      , { version: "0.15.0", hasLibTInfo: false }
      , { version: "0.15.1-0", hasLibTInfo: false }
      , { version: "0.14.9", hasLibTInfo: false }
      , { version: "0.14.0", hasLibTInfo: true }
      , { version: "0.13.8", hasLibTInfo: true }
      , { version: "0.13.0", hasLibTInfo: true }
      ]
      where
      isPrerelease = eq 2 <<< Array.length <<< String.split (Pattern "-") <<< _.version

    installLibtinfo = do
      libtInfo <- _.getResult =<< execa "sudo" [ "apt-get", "install", "-y", "libtinfo-dev"] (_ 
            { stdout = Just pipe
            , stderr = Just pipe
            })
      case libtInfo.exit of
        Normally 0 -> log "Successfully installed libtinfo"
        _ -> liftEffect $ throw $ "Failed to install libtinfo:\n" <> libtInfo.message
  

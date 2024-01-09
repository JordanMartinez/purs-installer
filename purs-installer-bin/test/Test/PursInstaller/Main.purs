module Test.PursInstaller.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.PursInstaller.Setup (withTempDir)
import Test.PursInstaller.Spec as Spec
import Test.Spec (around)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (Config, defaultConfig, runSpecT)

testConfig :: Config
testConfig = defaultConfig
  { slow = Milliseconds 10_000.0
  , timeout = Just (Milliseconds 30_000.0)
  , exit = true
  }

main :: Effect Unit
main = do
  runTests <- runSpecT testConfig [ consoleReporter ] do
    around withTempDir do
      Spec.spec
  launchAff_ $ void runTests
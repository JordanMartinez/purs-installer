module Test.PursInstaller.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
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
main = launchAff_ $ void $ un Identity $ runSpecT testConfig [ consoleReporter ] do
  around withTempDir do
    Spec.spec
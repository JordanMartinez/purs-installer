module Test.PursInstaller.Setup where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess.Types (Exit(..), pipe)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Perms (permsAll)
import Node.FS.Sync as FS
import Node.Library.Execa (ExecaResult, execa)
import Node.OS as OS
import Node.Path (FilePath, dirname, sep)
import Node.Path as Path
import Node.Process (platformStr)
import Node.Process as Process
import Test.Spec.Assertions (fail)

type TestDirs =
  { pursInstaller :: Array String -> Aff ExecaResult
  , purs :: Array String -> Aff ExecaResult
  , testCwd :: FilePath
  , checkFixture :: FilePath -> String -> Aff Unit
  }

withTempDir :: (TestDirs -> Aff Unit) -> Aff Unit
withTempDir = Aff.bracket createTempDir cleanupTempDir
  where
  createTempDir = do
    oldCwd <- liftEffect $ Process.cwd
    temp <- mkTempPath "purs-installer-test-"
    FSA.mkdir' temp { mode: permsAll, recursive: true }
    isDebug <- liftEffect $ map isJust $ Process.lookupEnv "SPEC_TEST_DEBUG"
    pursBin <- liftEffect $ Path.resolve [ temp ] case platformStr of
      "win32" -> "purs.exe"
      _ -> "purs"
    when isDebug do
      log $ "Running test in " <> temp
    let
      fixture path = Path.concat [ oldCwd, "purs-installer-bin", "test", "fixtures", path ]

      pursInstaller :: Array String -> Aff ExecaResult
      pursInstaller args =
        _.getResult =<< execa "node"
          ([ Path.concat [ oldCwd, "bin", "index.js" ] ] <> args) (_ 
            { stdout = Just pipe
            , stderr = Just pipe
            , cwd = Just temp
            })

      purs :: Array String -> Aff ExecaResult
      purs args = 
        _.getResult =<< execa pursBin args (_ 
            { stdout = Just pipe
            , stderr = Just pipe
            , cwd = Just temp
            })

    pure
      { pursInstaller
      , purs
      , testCwd: temp
      , checkFixture: \path -> checkFixture' temp (fixture path)
      }

  cleanupTempDir { testCwd } = do
    FSA.rm' testCwd { force: false, recursive: true, maxRetries: 0, retryDelay: 100 }

shouldEqualStr
  :: forall m
   . MonadThrow Error m
  => String
  -> String
  -> m Unit
shouldEqualStr v1 v2 =
  when (v1 /= v2) do
    fail $ Array.intercalate "\n"
      [ ""
      , "===== (Actual)"
      , v1
      , "====="
      , "  ≠"
      , "===== (Expected)"
      , v2
      , "====="
      , ""
      ]

isSuccess :: ExecaResult -> Aff Unit
isSuccess r = case r.exit of
  Normally 0 -> pure unit
  _ -> fail $ prettyPrint r.message

isFailure :: ExecaResult -> Aff Unit
isFailure r = case r.exit of
  Normally 0 -> fail $ prettyPrint r.message
  _ -> pure unit

prettyPrint :: String -> String
prettyPrint =
    String.replaceAll (Pattern "\\n") (Replacement "\n")
      <<< String.replaceAll (Pattern "\\\"") (Replacement "\"")

check
  :: { stdout :: String -> Aff Unit
     , stderr :: String -> Aff Unit
     , result :: ExecaResult -> Aff Unit
     }
  -> ExecaResult
  -> Aff Unit
check checkers execResult = do
  let
    stdout = String.trim $ execResult.stdout
    stderr = String.trim $ execResult.stderr

  printStdoutStderr <- liftEffect $ map isJust $ Process.lookupEnv "SPEC_TEST_DEBUG"

  when printStdoutStderr do
    log $ "STDOUT:\n" <> prettyPrint stdout
    log $ "STDERR:\n" <> prettyPrint stderr
  checkers.result execResult
  checkers.stdout stdout
  checkers.stderr stderr

checkFixture' :: FilePath -> FilePath -> String -> Aff Unit
checkFixture' testCwd fixtureFileExpected actual = do
  overwriteSpecFile <- liftEffect $ map isJust $ Process.lookupEnv "SPEC_TEST_ACCEPT"
  let 
    normalizeOutput = 
      String.replaceAll (Pattern $ testCwd <> sep) (Replacement "")
      >>> String.replaceAll (Pattern $ ".exe") (Replacement "")
      >>> String.replaceAll (Pattern $ ".cmd") (Replacement "")
      >>> String.replaceAll (Pattern $ "/private") (Replacement "")
      >>> String.replaceAll (Pattern $ "\r\n") (Replacement "\n")
      >>> String.replaceAll (Pattern $ "\r") (Replacement "\n")
  if overwriteSpecFile then do
    Console.log $ "Overwriting fixture at path: " <> fixtureFileExpected
    let parentDir = dirname fixtureFileExpected
    unlessM (liftEffect $ FS.exists parentDir) $ FSA.mkdir' parentDir { mode: permsAll, recursive: true }
    FSA.writeTextFile UTF8 fixtureFileExpected (normalizeOutput actual <> "\n")
  else do
    expected <- String.trim <$> FSA.readTextFile UTF8 fixtureFileExpected
    (normalizeOutput actual) `shouldEqualStr` (normalizeOutput expected)

checkOutputsStr
  :: { stdoutStr :: Maybe String
     , stderrStr :: Maybe String
     , result :: ExecaResult -> Aff Unit
     }
  -> ExecaResult
  -> Aff Unit
checkOutputsStr checkers =
  check
    { stdout: maybe mempty (\exp act -> act `shouldEqualStr` exp) checkers.stdoutStr
    , stderr: maybe mempty (\exp act -> act `shouldEqualStr` exp) checkers.stderrStr
    , result: checkers.result
    }

mkTempPath :: forall m. MonadEffect m => String -> m FilePath
mkTempPath suffix = liftEffect do
  randomStr <- do
    now <- Now.now
    hashStringToHex $ show now <> suffix
  osTemp <- OS.tmpdir
  -- Return the dir, but don't make it - that's the responsibility of the client
  let tempDirPath = Path.concat [ osTemp, randomStr ]
  pure tempDirPath

hashStringToHex :: forall m. MonadEffect m => String -> m String
hashStringToHex input = liftEffect do
  inputBuf <- Buffer.fromString input UTF8
  newHash <- createHash "sha256"
  hash <- updateHash inputBuf newHash
  digest <- digestHash hash
  hashStr <- Buffer.toString Base64 digest
  outputBuf <- Buffer.fromString ("sha256-" <> hashStr) UTF8
  Buffer.toString Hex outputBuf

foreign import data HashObject :: Type

foreign import createHashImpl :: EffectFn1 String HashObject
foreign import updateHashImpl :: EffectFn2 Buffer HashObject HashObject
foreign import digestHashImpl :: EffectFn1 HashObject Buffer

createHash :: String -> Effect HashObject
createHash = runEffectFn1 createHashImpl

updateHash :: Buffer -> HashObject -> Effect HashObject
updateHash = runEffectFn2 updateHashImpl

digestHash :: HashObject -> Effect Buffer
digestHash = runEffectFn1 digestHashImpl
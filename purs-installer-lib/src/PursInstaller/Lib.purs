module PursInstaller.Lib where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Codec.JSON (decode)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CAR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Version (Version, isPreRelease, showVersion)
import Data.Version as Version
import Effect.Aff (Aff, Error, error, makeAff, message, nonCanceler, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throwException)
import JS.Fetch (fetchWithOptions)
import JS.Fetch.Request as Request
import JS.Fetch.Response as Response
import JSON (parse)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess.Types (Exit(..), inherit, pipe)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Perms (permsAll)
import Node.FS.Stats as Stat
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream')
import Node.FS.Sync as FS
import Node.Library.Execa (ExecaResult, execa)
import Node.OS as OS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Platform (Platform(..))
import Node.Process (platform, platformStr)
import Node.Process as Process
import Node.Stream (Readable, Writable)
import Node.Stream as Stream
import Promise.Aff (toAffE)
import PursInstaller.Constants (ReleaseFile, ReleaseFileType(..), defaultCacheDir)
import PursInstaller.Constants as Constants
import PursInstaller.Foreign.Cacache (CacheFilePath, CacheKey(..))
import PursInstaller.Foreign.Cacache as Cacache
import PursInstaller.Foreign.Tar (extractPursToDir, extractToDir)
import PursInstaller.Hash (HashAlgorithm(..), hash, hashToSsri)
import PursInstaller.Monad (AppM, die, logDebug, logInfo)
import Unsafe.Coerce (unsafeCoerce)

installFromCacheReleaseOrSource :: AppM Unit
installFromCacheReleaseOrSource = do
  env@{ version } <- ask
  logInfo [ "Detecting architecture..." ]
  { machine, platform, releaseFile } <- getBuildProfile
  logDebug
    [ "Machine: " <> machine
    , "Platform: " <> platform
    , "ReleaseFile: " <> releaseFile.fileName
    ]
  absBinFile <- resolveName releaseFile.osFileName env.name
  deleteBinFileIfExist absBinFile
  cacheDir <- liftEffect defaultCacheDir
  let cacheKey = toCacheKey { version, machine, platform }

  cacheResult <- installFromCache { cacheDir, cacheKey, version, absBinFile }
  case cacheResult of
    CacheFailure -> do
      installFromRelease { cacheDir, absBinFile, version, cacheKey, releaseFile }
    InstalledFromCache -> do
      logInfo [ "Successfully installed PureScript from cache" ]

getBuildProfile :: AppM { machine :: String, platform :: String, releaseFile :: ReleaseFile }
getBuildProfile = do
  machine <- liftEffect OS.machine
  let
    x86_64 = machine == "x86_64"
    aarch64 = machine == "aarch64"
    arm64 = machine == "arm64"
  pure $ { machine, platform: platformStr, releaseFile: _ } case Process.platform of
    Just x -> case x of
      Darwin
        | x86_64 -> Constants.macos
        | arm64 -> Constants.macosArm64
      Linux
        | x86_64 -> Constants.linux64
        | aarch64 -> Constants.linuxArm64
      Win32
        | x86_64 -> Constants.win64
      _ -> Constants.sourceTarGz
    Nothing -> Constants.sourceTarGz

resolveName 
  :: String
  -> Maybe String 
  -> AppM String
resolveName pursFile = case _ of
    Nothing -> do
      pkgJson <- liftEffect $ FS.exists "package.json"
      if pkgJson then do
        eitherJ <- liftAff $ parse <$> FSA.readTextFile UTF8 "package.json"
        case eitherJ of
          Left _ -> do
            logDebug [ "Found a 'package.json' file but could not decode it. Not renaming purs binary file to `purs.bin`" ]
            liftEffect $ Path.resolve [] pursFile
          Right json -> do
            case decode (CAR.object { bin: CAR.object { purs: CJ.string } }) json of
              Right { bin: { purs } } -> do
                liftEffect $ Path.resolve [] purs
              _ -> do
                logDebug [ "Found a package.json' file, but '.bin.purs` is not a String" ]
                liftEffect $ Path.resolve [] pursFile
      else do
        logDebug [ "No package.json file found." ]
        liftEffect $ Path.resolve [] pursFile
    Just newName -> do
      logDebug [ "Using user-provided binary name: " <> newName ]
      newName' <- liftEffect $ Path.resolve [] newName
      logInfo [ "Using user-provided binary name (post resolution): " <> newName' ]
      pure newName'

deleteBinFileIfExist :: FilePath -> AppM Unit
deleteBinFileIfExist absBinFile = do
  binFileExists <- liftEffect $ FS.exists absBinFile
  when binFileExists do
    stats <- liftAff $ FSA.stat absBinFile
    when (Stats.isDirectory stats) do
      die [ "Tried to create a PureScript binary at '" <> absBinFile <> "', but a directory already exists there." ]
    logDebug [ "Deleting an existing file where where the PureScript binary will be installed: "  <> absBinFile ]
    liftAff $ FSA.unlink absBinFile

data CacheResult
  = CacheFailure
  | InstalledFromCache

derive instance Eq CacheResult
derive instance Ord CacheResult
derive instance Generic CacheResult _
instance Show CacheResult where 
  show x = genericShow x

toCacheKey :: { version :: Version, machine :: String, platform :: String } -> CacheKey
toCacheKey r = CacheKey $ Array.intercalate "-" [ showVersion r.version, r.platform, r.machine ]

installFromCache 
  :: { cacheDir :: CacheFilePath
     , cacheKey :: CacheKey
     , version :: Version
     , absBinFile :: FilePath
     }
  -> AppM CacheResult
installFromCache { cacheDir, cacheKey, version, absBinFile } = do
  nullableInfo <- liftAff $ toAffE $ Cacache.info @( mode :: Int ) cacheDir cacheKey
  case toMaybe nullableInfo of
    Just info -> do
      let 
        originalSsri = info.integrity
        cachePath = info.path
        binMode = info.metadata.mode
      logDebug [ "Found cache info: " <> show { cachePath, binMode } ]
      binBuffer <- liftAff $ FSA.readFile cachePath
      errOrCachedSsri <- liftEffect $ hashToSsri Sha512 binBuffer
      case errOrCachedSsri of
        Left err -> do
          logDebug 
            [ "Encountered error when hashing cached file path:" 
            , message err
            ]
          revokeCacheEntry cacheDir cacheKey
          pure CacheFailure
        Right cachedSsri -> do
          if cachedSsri /= originalSsri then do
            logInfo 
              [ "Cached file's contents were manipulated: ssri strings don't match." 
              , "Expected: " <> show originalSsri
              , "Actual:   " <> show cachedSsri
              ]
            revokeCacheEntry cacheDir cacheKey
            pure CacheFailure
          else do
            logInfo [ "Cache's recorded ssri string matches file's ssri string." ]
            logDebug [ "Copying cached file to install location" ]
            result <- liftAff $ streamCopy { from: cachePath, to: absBinFile, toMode: binMode }
            case result of
              Left err -> do
                logDebug 
                  [ "Encountered error when restoring from cache:" 
                  , message err
                  ]
                revokeCacheEntry cacheDir cacheKey
                pure CacheFailure
              Right _ -> do
                logDebug [ "Restored file from cache." ]

                binCheck <- checkBinary version absBinFile
                case binCheck of
                  Just err -> do
                    logInfo
                      [ "Cached file failed binary check. Revoking form cache." ]
                    logDebug 
                      [ "Error was: "
                      , message err
                      ]
                    revokeCacheEntry cacheDir cacheKey
                    liftAff $ FSA.unlink absBinFile
                    pure CacheFailure
                  Nothing -> do
                    pure InstalledFromCache
    Nothing -> do
      logDebug [ "No cache found." ]
      pure CacheFailure

streamCopy :: { from :: FilePath, to :: FilePath, toMode :: Int } -> Aff (Either Error Unit)
streamCopy { from, to, toMode } = do
  fromStream <- liftEffect $ createReadStream from
  toStream <- liftEffect $ createWriteStream' to { mode: toMode }
  streamPipeline fromStream toStream

streamPipeline :: forall w r. Readable w -> Writable r -> Aff (Either Error Unit)
streamPipeline from to = try $ makeAff \done -> do
  Stream.pipeline from [] to case _ of
    Just err -> done $ Left err
    Nothing -> done $ Right unit
  pure nonCanceler

installFromRelease
  :: { cacheDir :: CacheFilePath
     , absBinFile :: FilePath
     , version :: Version
     , cacheKey :: CacheKey
     , releaseFile :: ReleaseFile
     }
  -> AppM Unit
installFromRelease args = case args.releaseFile.fileType of
  BinaryFile -> do
    installFromReleasedBinary args
  SourceFile -> do
    logInfo [ "There is no released binary for your build profile. Building from source..." ]
    installFromSource args

installFromReleasedBinary
  :: { cacheDir :: CacheFilePath
     , absBinFile :: FilePath
     , version :: Version
     , cacheKey :: CacheKey
     , releaseFile :: ReleaseFile
     }
  -> AppM Unit
installFromReleasedBinary { cacheDir, version, cacheKey, releaseFile, absBinFile } = do
  result <- downloadFromSource version releaseFile
  case result of
    Left err -> do
      die 
        [ "Failure when downloading and verifying release file. Error was:"
        , message err
        ]
    Right pursFile -> do
      renamePursFile { original: pursFile, final: absBinFile }

      binCheck <- checkBinary version absBinFile
      for_ binCheck (liftEffect <<< throwException)

      cacheBinaryFile { cacheDir, absBinFile, cacheKey, version }

renamePursFile :: { original :: FilePath, final :: FilePath } -> AppM Unit
renamePursFile { original, final } = do
  when (original /= final) do
    logDebug 
      [ "Renaming purs file"
      , "  From: " <> original
      , "  To:   " <> final
      ]
    liftAff $ FSA.rename original final
  logDebug [ "Purs file located at: " <> final ]

installFromSource
  :: { cacheDir :: CacheFilePath
     , absBinFile :: FilePath
     , version :: Version
     , cacheKey :: CacheKey
     , releaseFile :: ReleaseFile
     }
  -> AppM Unit
installFromSource args = do
  checkStack

  buildDir <- mkTempDir "node-purescript"
  
  let
    baseDir = Path.dirname args.absBinFile
    compressedPath = Path.concat [ baseDir, args.releaseFile.fileName ]
    pursBin = args.releaseFile.osFileName
    pursFile = Path.concat [ baseDir, pursBin ]
  errOrMainBuf <- download $ args.releaseFile.downloadUrl args.version
  case errOrMainBuf of
    Left errMain -> liftEffect $ throwException errMain
    Right mainBuf -> do
      logDebug [ "Writing downloaded content to path: " <> compressedPath ]
      liftAff $ FSA.writeFile compressedPath mainBuf
      
      logDebug [ "Extracting " <> pursBin <> " from .tar.gz file..." ]
      liftAff $ extractToDir compressedPath buildDir
      env <- ask
      let 
        spawnOptions = (_ { cwd = Just buildDir, stdout = Just inherit, stderr = Just inherit })
        
        withDiffUser :: Array String -> Array String
        withDiffUser spawnArgs
          | not $ Array.elem "--allow-different-user" spawnArgs
          , not $ Array.elem "--no-allow-different-user" spawnArgs
          , Just Win32 <- platform = spawnArgs <> [ "--allow-different-user" ]
          | otherwise = spawnArgs

        setupArgs = withDiffUser [ "setup" ]

        buildArgs = Set.fromFoldable
          [ "--dry-run"
          , "--pedantic"
          , "--fast"
          , "--only-snapshot"
          , "--only-dependencies"
          , "--only-configure"
          , "--trace"
          , "--profile"
          , "--no-strip"
          , "--coverage"
          ]
        installArgs = 
          withDiffUser [ "install", "--local-bin-path=" <> baseDir, "--flag=purescript:RELEASE", "--no-run-tests", "--no-run-benchmark" ] 
            <> Array.filter (flip Set.member buildArgs) env.stackArgs

      logDebug [ "Running 'stack setup'" ]
      setupResult <- liftAff $ _.getResult =<< execa stackBin setupArgs spawnOptions
      cleanExitOrDieWith (\msg -> [ "Failure when running 'stack setup'", msg ]) setupResult
      logDebug [ "Running 'stack install'" ]
      installResult <- liftAff $ _.getResult =<< execa stackBin installArgs spawnOptions
      cleanExitOrDieWith (\msg -> [ "Failure when running 'stack install'", msg ]) installResult

      logInfo [ "Successfully compiled PureScript from source" ]

      renamePursFile { original: pursFile, final: args.absBinFile }
      
      mbErr <- checkBinary args.version args.absBinFile
      for_ mbErr (liftEffect <<< throwException)

      cacheBinaryFile 
        { cacheDir: args.cacheDir
        , absBinFile: args.absBinFile
        , version: args.version
        , cacheKey: args.cacheKey
        }

stackBin :: String
stackBin
  | platformStr == "win32" = "stack.exe"
  | otherwise = "stack"

checkStack :: AppM Unit
checkStack = do
  result <- liftAff $ _.getResult =<< execa stackBin [ "--numeric-version" ] identity
  cleanExitOrDieWith (const [ "Stack must be installed" ]) result

cleanExitOrDieWith :: (String -> Array String) -> ExecaResult -> AppM Unit
cleanExitOrDieWith toMsg result = do
  logDebug [ result.escapedCommand ]
  case result.exit of
    Normally 0 -> 
      pure unit
    _ -> do
      logDebug [ result.message ]
      die $ toMsg result.message

mkTempDir :: String -> AppM FilePath
mkTempDir dirPrefix = do
  osTmpDir <- liftEffect OS.tmpdir
  tmpDir <- liftAff $ FSA.mkdtemp $ Path.concat [ osTmpDir, dirPrefix <> "-" ]
  logDebug [ "Temp directory located at: " <> tmpDir ]
  unlessM (liftEffect $ FS.exists tmpDir) do
    logDebug [ "Making directory because it doesn't exist: " <> tmpDir ]
    liftAff $ FSA.mkdir' tmpDir { mode: permsAll, recursive: true }
  pure tmpDir

downloadFromSource
  :: Version 
  -> ReleaseFile 
  -> AppM (Either Error FilePath)
downloadFromSource version releaseFile = do
  logInfo $ [ "Getting " <> releaseFile.osFileName <> " (" <> showVersion version <> ")" ]
  pwd <- liftEffect $ Process.cwd
  try $ downloadAndUntargz
    { baseDir: pwd
    , file: releaseFile
    , version
    }

download :: String -> AppM (Either Error Buffer)
download url = try do
  logDebug [ "Downloading via url: " <> url ]
  req <- liftEffect $ Request.unsafeNew url $ unsafeCoerce
    { redirect: "follow" }
  resp <- liftAff $ toAffE $ fetchWithOptions req { keepalive: true }
  abuff <- liftAff $ toAffE $ Response.arrayBuffer resp
  liftEffect $ Buffer.fromArrayBuffer abuff

checkSha
  :: { expectedSha :: String
     , fileName :: String
     , fileContent :: Buffer
     }
  -> AppM Unit
checkSha { expectedSha, fileName, fileContent } = do
  result <- liftEffect $ hash Sha1 fileContent
  case result of
    Left err -> die
      [ "Error encountered while attempting to hash the downloaded file, " <> fileName <> ":"
      , message err
      ]
    Right actualHash
      | expectedSha /= actualHash ->
          die
            [ "Hashes fail to match for file, " <> fileName
            , "Expected: " <> expectedSha
            , "Actual  : " <> actualHash
            ]
      | otherwise -> do
          logDebug [ "Hashes match for file, " <> fileName ]

downloadAndUntargz
  :: { version :: Version
     , file :: ReleaseFile
     , baseDir :: FilePath
     }
  -> AppM FilePath
downloadAndUntargz { version, file, baseDir } = do
  let
    tarGzFilePath = Path.concat [ baseDir, file.fileName ]
    pursBin = file.osFileName
  pursFile <- liftEffect $ Path.resolve [ baseDir ] pursBin
  unlessM (liftEffect $ FS.exists baseDir) do
    logDebug [ "Making parent dir because it doesn't exist: " <> baseDir ]
    liftAff $ FSA.mkdir' baseDir { mode: permsAll, recursive: true }
  whenM (liftEffect $ FS.exists tarGzFilePath) do
    liftAff $ FSA.unlink tarGzFilePath
  errOrMainBuf <- download $ file.downloadUrl version
  case errOrMainBuf of
    Left errMain -> liftEffect $ throwException errMain
    Right mainBuf -> do
      logDebug [ "Writing downloaded content to path: " <> tarGzFilePath ]
      liftAff $ FSA.writeFile tarGzFilePath mainBuf

      for_ file.shaFile \shaFile -> do
        logDebug [ "Downloading corresponding sha file: " <> shaFile.fileName ]
        errOrShaBuf <- download $ shaFile.downloadUrl version
        case errOrShaBuf of
          Left errSha -> liftEffect $ throwException errSha
          Right shaBuf -> do
            let
              extractSha = case platformStr of
                "win32" -> 
                  -- win64.sha's file content: 
                  -- SHA1(bundle/win64.tar.gz)= f2add5a746e4162ccfbfa6fd15004deafbc1e9ba
                  String.trim <<< SCU.dropWhile (\c -> c /= ' ')
                _ -> 
                  -- all other .sha's file content:
                  -- 393a8955b40e945095f469a976421f8dfa595040  bundle/linux64.tar.gz
                  String.trim <<< SCU.takeWhile (\c -> c /= ' ')
            expectedSha <- liftEffect $ map extractSha $ Buffer.toString UTF8 shaBuf
            checkSha
              { expectedSha
              , fileName: file.fileName
              , fileContent: mainBuf
              }
      logDebug [ "Extracting " <> pursBin <> " from .tar.gz file..." ]
      liftAff $ extractPursToDir tarGzFilePath baseDir pursBin

      logDebug [ "Deleting .tar.gz file: " <> tarGzFilePath ]
      liftAff $ FSA.unlink tarGzFilePath
      pure pursFile

-- See https://github.com/purescript/purescript/issues/4460
accountForMacPreReleaseBug :: Version -> Maybe Version
accountForMacPreReleaseBug v
  | Just Darwin <- Process.platform
  , isPreRelease v
  , between (Version.version 0 15 0 Nil Nil) (Version.version 0 15 10 Nil Nil) v =
      Just $ Version.version 
        (Version.major v) 
        (Version.minor v) 
        (Version.patch v - 1) 
        (Version.preRelease v) 
        (Version.buildMetadata v)

  | otherwise = Nothing

checkBinary 
  :: Version
  -> FilePath
  -> AppM (Maybe Error)
checkBinary version pursFile = do
  let pursVersionCommand = "'" <> pursFile <> " --version'"
  logInfo [ "Verifying that binary exists and is executable via " <> pursVersionCommand ]
  sp <- liftAff $ _.getResult =<< execa pursFile [ "--version" ] (_ { stdout = Just pipe, stderr = Just pipe })
  let outputtedVersion = String.trim sp.stdout
  case sp.exit of
    Normally 0 
      | showVersion version == outputtedVersion -> do
          logInfo [ "Binary check succeeded." ]
          logDebug 
            [ "Expected version:  " <> showVersion version
            , "Outputted version: " <> outputtedVersion 
            ]
          pure Nothing
      | Just actualVersion <- accountForMacPreReleaseBug version -> do
          logInfo [ "Binary check succeeded." ]
          logDebug 
            [ "Downloaded version: " <> showVersion version <> " (what's being installed)"
            , "Expected version:   " <> showVersion actualVersion <> " (adjusted for Mac prerelease bug; see https://github.com/purescript/purescript/issues/4460)"
            , "Outputted version:  " <> outputtedVersion 
            ]
          pure Nothing
      | otherwise -> do
          pure $ Just $ error $ Array.intercalate "\n"
              [ "Binary check failed. Calling " <> pursVersionCommand <> " did not produce the expected version." 
              , "Expected version:   " <> showVersion version
              , "Outputted version:  " <> outputtedVersion 
              ]
    _ 
      | String.contains (Pattern "libtinfo.so.5") sp.message -> do
          logInfo 
            [ "==="
            , "`purs --version` check failed because `libtinfo.so.5` could not be found."
            , ""
            , "This version of PureScript requires `libtinfo5` before the compiler can be used. Please install `libtinfo5`." 
            , "==="
            ]
          pure Nothing
      | otherwise ->
          pure $ Just $ error $ Array.intercalate "\n"
            [ "Calling " <> pursVersionCommand <> " failed with error:"
            , sp.message
            ]

cacheBinaryFile ::
  { cacheDir :: CacheFilePath
  , version :: Version
  , cacheKey :: CacheKey
  , absBinFile :: FilePath
  }
   -> AppM Unit
cacheBinaryFile { cacheDir, absBinFile, version, cacheKey } = do
  stats <- liftAff $ FSA.stat absBinFile
  binStream <- liftEffect $ createReadStream absBinFile
  cacheStream <- liftEffect $ Cacache.putStream @(mode :: Number, version :: String) cacheDir cacheKey
    { size: Stat.size stats
    , metadata:
        { mode: Stat.mode stats
        , version: showVersion version
        }
    }
  copyResult <- liftAff $ streamPipeline binStream cacheStream
  case copyResult of
    Left copyErr -> do
      logInfo [ "Failed to cache binary. Clearing entry from cache." ]
      logDebug
        [ "Error was:"
        , message copyErr
        ]
      clearCacheEntry cacheDir cacheKey
      verifyCache cacheDir
    Right _ -> do
      verifyCache cacheDir
      logInfo [ "Successfully cached binary" ]


revokeCacheEntry :: CacheFilePath -> CacheKey -> AppM Unit
revokeCacheEntry cachePath cacheKey@(CacheKey key) = do
  logDebug [ "Clearing cache entry for key: " <> key ]
  clearCacheEntry cachePath cacheKey
  verifyCache cachePath

clearCacheEntry :: CacheFilePath -> CacheKey -> AppM Unit
clearCacheEntry cachePath cacheKey = do
  liftAff $ void $ try $ toAffE $ Cacache.rmEntry cachePath cacheKey

verifyCache :: CacheFilePath -> AppM Unit
verifyCache cachePath = do
  liftAff $ void $ try $ toAffE $ Cacache.verify cachePath

listCache :: Aff Unit
listCache = do
  cacheDir <- liftEffect defaultCacheDir
  output <- liftAff $ toAffE $ Cacache.ls cacheDir
  Console.log output

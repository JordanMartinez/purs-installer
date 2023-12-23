module PursInstaller.Constants 
  ( ShaFile
  , ReleaseFile
  , ReleaseFileType(..)
  , linuxArm64
  , linux64
  , macosArm64
  , macos
  , win64
  , sourceTarGz
  , defaultCacheDir
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Version (Version, showVersion)
import Effect (Effect)
import Node.Library.EnvPaths (envPathsNow)
import Node.Process (platformStr)
import PursInstaller.Foreign.Cacache (CacheFilePath(..))

type ShaFile =
  { fileName :: String
  , downloadUrl :: Version -> String
  }

type ReleaseFile =
  { fileName :: String
  , fileType :: ReleaseFileType
  , osFileName :: String
  , downloadUrl :: Version -> String
  , shaFile :: Maybe ShaFile
  }

data ReleaseFileType
  = BinaryFile
  | SourceFile

derive instance Eq ReleaseFileType
derive instance Ord ReleaseFileType
derive instance Generic ReleaseFileType _
instance Show ReleaseFileType where show x = genericShow x

linuxArm64 :: ReleaseFile
linuxArm64 = mkReleaseFileWithSha "linux-arm64" Nothing BinaryFile

linux64 :: ReleaseFile
linux64 = mkReleaseFileWithSha "linux64" Nothing BinaryFile

macosArm64 :: ReleaseFile
macosArm64 = mkReleaseFileWithSha "macos-arm64" Nothing BinaryFile

macos :: ReleaseFile
macos = mkReleaseFileWithSha "macos" Nothing BinaryFile

win64 :: ReleaseFile
win64 = mkReleaseFileWithSha "win64" (Just ".exe") BinaryFile

mkReleaseFileWithSha :: String -> Maybe String -> ReleaseFileType -> ReleaseFile
mkReleaseFileWithSha fileBase addExe fileType = do
  let tarGzFile = fileBase <> ".tar.gz"
  { downloadUrl: toReleaseUrl tarGzFile
  , osFileName: "purs" <> fold addExe
  , fileName: tarGzFile
  , fileType
  , shaFile: Just do
      let shaFile = fileBase <> ".sha"
      { downloadUrl: toReleaseUrl shaFile
      , fileName: shaFile
      }
  }

sourceTarGz :: ReleaseFile
sourceTarGz = do
  { downloadUrl: archiveUrl
  , osFileName: "purs" <> (if platformStr == "win32" then ".exe" else "")
  , shaFile: Nothing
  , fileName: "source-code.tar.gz"
  , fileType: SourceFile
  }

toReleaseUrl :: String -> Version -> String
toReleaseUrl file v = "https://github.com/purescript/purescript/releases/download/v" <> showVersion v <> "/" <> file

archiveUrl :: Version -> String
archiveUrl v = "https://github.com/purescript/purescript/archive/refs/tags/v" <> showVersion v <> ".tar.gz"

defaultCacheDir :: Effect CacheFilePath
defaultCacheDir = (CacheFilePath <<< _.cache) <$> envPathsNow { name: "purs-installer", suffix: Nothing }

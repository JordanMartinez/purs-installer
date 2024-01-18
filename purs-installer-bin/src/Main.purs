module PursInstaller.Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.String (toLower)
import Data.Version as Version
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, message, runAff_)
import Effect.Class.Console (log)
import Effect.Console as Console
import Node.Process as Process
import Options.Applicative (Parser, ParserPrefs(..), command, customExecParser, defaultPrefs, eitherReader, fullDesc, header, help, helper, info, long, many, metavar, option, progDesc, short, showDefaultWith, strArgument, strOption, subparser, switch, value, (<**>))
import Options.Applicative.Types (optional)
import Parsing (parseErrorMessage)
import PursInstaller.Lib (installFromCacheReleaseOrSource, listCache)
import PursInstaller.Monad (LogLevel(..), runAppM)
import Spago.Generated.BuildInfo as BI

main :: Effect Unit
main = startApp =<< parseArgs
  where
  parseArgs = customExecParser
    ( defaultPrefs # \(ParserPrefs p) -> ParserPrefs
        ( p
            { prefShowHelpOnError = true
            , prefShowHelpOnEmpty = true
            }
        )
    )
    ( info ((versionString <|> rest) <**> helper)
        ( fullDesc
            <> progDesc "Installs PureScript"
            <> header "purs-installer - install PureScript from a release or source"
        )
    )
  versionString = map (const Version) $ switch $ fold
    [ long "version"
    , short 'v'
    , help "Print the version"
    ]

  rest = subparser
    ( command "install" (info (Install <$> installParser) ( progDesc "Install PureScript" ))
    <> command "list" (info (pure ListCache) ( progDesc "Print the list of cached binaries." ))
  )

type CliArgs =
  { pursVersion :: String
  , name :: Maybe String
  , logLevel :: LogLevel
  , stackArgs :: Array String
  }

installParser :: Parser CliArgs
installParser = ado
  pursVersion <- strOption $ fold
    [ long "purs-version"
    , metavar "VERSION"
    , help "PureScript version (e.g. 0.15.13, 0.15.4-0)."
    ]
  name <- optional $ strOption $ fold
    [ long "name"
    , metavar "NAME"
    , help "Change the binary name to the provided one. Defaults to 'purs.bin' if a `package.json` file exists whose 'bin' field is 'purs'. Otherwise, defaults to 'purs.exe' on Windows and 'purs' on others."
    ]
  quiet <- switch $ fold
    [ long "quiet"
    , help "Hide all logging aside from errors."
    ]
  logLevel <- option (eitherReader parseLogLevel) $ fold
    [ long "log-level"
    , help "Indicate the level of logging: 'quiet', 'info', or 'debug'. The '--quiet' flag takes precedence."
    , value Info
    , showDefaultWith (toLower <<< show)
    ]
  stackArgs <- many $ strArgument $ fold
    [ help "Arguments to pass onto 'stack' if one must install from source code"
    , metavar "STACK_ARGS"
    ]
  in 
    { pursVersion
    , name
    , logLevel: if quiet then Quiet else logLevel
    , stackArgs: Array.fromFoldable stackArgs
    }
  where
  parseLogLevel = case _ of
    "quiet" -> Right Quiet
    "info" -> Right Info
    "debug" -> Right Debug
    err -> Left err

data Command
  = Version
  | Install CliArgs
  | ListCache

startApp :: Command -> Effect Unit
startApp = case _ of
  Version ->
    log BI.packages."purs-installer-bin"
  Install args ->
    case Version.parseVersion args.pursVersion of
      Left err -> do
        Console.error $ "Failed to convert --purs-version arg into a version: " <> parseErrorMessage err
        Process.setExitCode 1
      Right version -> do
        runOrReportError $ runAppM installFromCacheReleaseOrSource
          { version
          , logLevel: args.logLevel
          , name: args.name
          , stackArgs: args.stackArgs
          }
  ListCache ->
    launchAff_ listCache
  where
  runOrReportError :: forall a. Aff a -> Effect Unit
  runOrReportError = runAff_ case _ of
    Left err -> do
      Console.error $ message err
      Process.setExitCode 1
    Right _ -> do
      pure unit
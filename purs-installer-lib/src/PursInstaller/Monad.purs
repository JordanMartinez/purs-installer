module PursInstaller.Monad 
  ( LogLevel(..)
  , logInfo
  , logDebug
  , die
  , Env
  , AppM(..)
  , runAppM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Version (Version)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)


data LogLevel
  = Quiet
  | Info
  | Debug

derive instance Eq LogLevel
derive instance Ord LogLevel
instance Show LogLevel where
  show = case _ of
    Quiet -> "Quiet"
    Info -> "Info"
    Debug -> "Debug"

logInfo :: Array String -> AppM Unit
logInfo arr = do
  { logLevel } <- ask
  when (logLevel >= Info) do
    Console.log $ Array.intercalate "\n" arr

die :: Array String -> AppM Unit
die = liftEffect <<< throw <<< Array.intercalate "\n"

logDebug :: Array String -> AppM Unit
logDebug arr = do
  { logLevel } <- ask
  when (logLevel == Debug) do
    Console.log $ Array.intercalate "\n" arr

type Env =
  { version :: Version
  , name :: Maybe String
  , logLevel :: LogLevel
  , stackArgs :: Array String
  }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: forall a. AppM a -> Env -> Aff a
runAppM (AppM m) = runReaderT m

derive instance Newtype (AppM a)  _
derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadAsk Env AppM
derive newtype instance MonadReader Env AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
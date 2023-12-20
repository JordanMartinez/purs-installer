module PursInstaller.Foreign.Tar 
  ( extractPursToDir
  , extractToDir
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn3, runEffectFn4)
import Node.Path (FilePath)

foreign import extractPursToDirImpl :: EffectFn4 FilePath FilePath String (EffectFn1 (Nullable Error) Unit) Unit

extractPursToDir :: FilePath -> FilePath -> String -> Aff Unit
extractPursToDir file dir pursFile = makeAff \done -> do
  runEffectFn4 extractPursToDirImpl file dir pursFile
    ( mkEffectFn1 \err -> case toMaybe err of
        Nothing -> done $ Right unit
        Just err' -> done $ Left err'
    )
  pure nonCanceler

foreign import extractToDirImpl :: EffectFn3 FilePath FilePath (EffectFn1 (Nullable Error) Unit) Unit

extractToDir :: FilePath -> FilePath -> Aff Unit
extractToDir file dir = makeAff \done -> do
  runEffectFn3 extractToDirImpl file dir
    ( mkEffectFn1 \err -> case toMaybe err of
        Nothing -> done $ Right unit
        Just err' -> done $ Left err'
    )
  pure nonCanceler
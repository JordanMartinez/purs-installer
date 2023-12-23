module PursInstaller.Hash 
  ( hash
  , ssriHash
  , HashAlgorithm(..)
  , SsriString -- constructor intentionally not exported
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.Buffer (Buffer)

foreign import hashImpl :: EffectFn2 String Buffer String

hash :: HashAlgorithm -> Buffer -> Effect (Either Error String)
hash algo buf = try $ runEffectFn2 hashImpl (printAlgorithm algo) buf

data HashAlgorithm
  = Sha1
  | Sha512

printAlgorithm :: HashAlgorithm -> String
printAlgorithm = case _ of
  Sha1 -> "sha1"
  Sha512 -> "sha512"

ssriHash :: HashAlgorithm -> Buffer -> Effect (Either Error SsriString)
ssriHash algo buf = map (map (SsriString <<< append ssri_)) $ hash algo buf
  where
  ssri_ = printAlgorithm algo <> "-"

newtype SsriString = SsriString String

derive instance Eq SsriString
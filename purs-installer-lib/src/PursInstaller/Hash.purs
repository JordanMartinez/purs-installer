module PursInstaller.Hash 
  ( hash
  , hashToSsri
  , HashAlgorithm(..)
  , SsriString -- constructor intentionally not exported
  ) where

import Prelude

import Data.Either (Either)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..), encodingToNode)

foreign import hashImpl :: EffectFn3 String Buffer String String

hash :: HashAlgorithm -> Buffer -> Effect (Either Error String)
hash algo buf = try $ runEffectFn3 hashImpl (printAlgorithm algo) buf (encodingToNode Hex)

data HashAlgorithm
  = Sha1
  | Sha512

printAlgorithm :: HashAlgorithm -> String
printAlgorithm = case _ of
  Sha1 -> "sha1"
  Sha512 -> "sha512"

hashToSsri :: HashAlgorithm -> Buffer -> Effect (Either Error SsriString)
hashToSsri algo buf = do
  errOrHash <- try $ runEffectFn3 hashImpl algoStr buf (encodingToNode Base64)
  for errOrHash \h -> do
    pure $ SsriString $ algoStr <> "-" <> h
  where
  algoStr = printAlgorithm algo

newtype SsriString = SsriString String

derive instance Eq SsriString
derive newtype instance Show SsriString
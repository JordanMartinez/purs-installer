module PursInstaller.Hash where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Buffer (Buffer)

foreign import sha1HashImpl :: EffectFn1 Buffer String

sha1Hash :: Buffer -> Effect (Either Error String)
sha1Hash buf = try $ runEffectFn1 sha1HashImpl buf

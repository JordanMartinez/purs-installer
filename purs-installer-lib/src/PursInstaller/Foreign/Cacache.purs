module PursInstaller.Foreign.Cacache 
  ( CacheKey(..)
  , CacheFilePath(..)
  , get
  , putStream
  , info
  , rmEntry
  , verify
  , ls
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.Buffer (Buffer)
import Node.Path (FilePath)
import Node.Stream (Writable)
import Prim.Row as Row
import Promise (Promise)
import PursInstaller.Hash (SsriString)

newtype CacheKey = CacheKey String

derive newtype instance Eq CacheKey
derive newtype instance Ord CacheKey
derive instance Generic CacheKey _
derive newtype instance Show CacheKey
derive instance Newtype CacheKey _

newtype CacheFilePath = CacheFilePath FilePath

derive newtype instance Eq CacheFilePath
derive newtype instance Ord CacheFilePath
derive instance Generic CacheFilePath _
derive newtype instance Show CacheFilePath
derive instance Newtype CacheFilePath _

foreign import getImpl :: EffectFn2 FilePath String (Promise { data :: Buffer, integrity :: SsriString })

get :: CacheFilePath -> CacheKey -> Effect (Promise { data :: Buffer, integrity :: SsriString })
get (CacheFilePath cachePath) (CacheKey key) = runEffectFn2 getImpl cachePath key

foreign import putStreamImpl :: forall r. EffectFn3 FilePath String { | r } (Writable ())

type PutStreamOptions metadata =
  ( metadata :: { | metadata }
  , size :: Number
  )

putStream 
  :: forall @metadataRows r trash
   . Row.Union r trash (PutStreamOptions metadataRows)
  => CacheFilePath 
  -> CacheKey 
  -> { | r } 
  -> Effect (Writable ())
putStream (CacheFilePath cachePath) (CacheKey key) options = runEffectFn3 putStreamImpl cachePath key options

type CacheInfo r =
  { integrity :: SsriString
  , path :: FilePath
  , time :: Milliseconds
  , metadata :: { | r }
  }

foreign import infoImpl :: forall r. EffectFn2 FilePath String (Promise (Nullable (CacheInfo r )))

info :: forall @r. CacheFilePath -> CacheKey -> Effect (Promise (Nullable (CacheInfo r )))
info (CacheFilePath cachePath) (CacheKey key) =  runEffectFn2 infoImpl cachePath key

foreign import rmEntryImpl :: EffectFn2 FilePath String (Promise Unit)

rmEntry :: CacheFilePath -> CacheKey -> Effect (Promise Unit)
rmEntry (CacheFilePath cachePath) (CacheKey key) = runEffectFn2 rmEntryImpl cachePath key

foreign import verifyImpl :: EffectFn1 FilePath (Promise Unit)

verify :: CacheFilePath -> Effect (Promise Unit)
verify (CacheFilePath cachePath) = runEffectFn1 verifyImpl cachePath

foreign import lsImpl :: EffectFn1 FilePath (Promise String)

ls :: CacheFilePath -> Effect (Promise String)
ls (CacheFilePath cachePath) = runEffectFn1 lsImpl cachePath

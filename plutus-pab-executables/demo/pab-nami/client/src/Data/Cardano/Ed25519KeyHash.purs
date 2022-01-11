module Data.Cardano.Ed25519KeyHash
  ( Ed25519KeyHash
  , toBytes
  ) where

import Data.ArrayBuffer.Types (Uint8Array)

foreign import data Ed25519KeyHash :: Type

foreign import toBytes :: Ed25519KeyHash -> Uint8Array

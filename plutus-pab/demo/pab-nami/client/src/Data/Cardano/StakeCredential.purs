module Data.Cardano.StakeCredential
  ( StakeCredential
  , toKeyHash
  ) where

import Data.Cardano.Ed25519KeyHash (Ed25519KeyHash)

foreign import data StakeCredential :: Type

foreign import toKeyHash :: StakeCredential -> Ed25519KeyHash

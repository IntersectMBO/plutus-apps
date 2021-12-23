module Data.Cardano.TransactionBody
  ( TransactionBody
  , outputs
  ) where

import Data.Cardano.TransactionOutputs (TransactionOutputs)

foreign import data TransactionBody :: Type

foreign import outputs :: TransactionBody -> TransactionOutputs

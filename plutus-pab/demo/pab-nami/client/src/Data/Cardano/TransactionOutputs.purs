module Data.Cardano.TransactionOutputs
  ( TransactionOutputs
  ) where

foreign import data TransactionOutputs :: Type

foreign import len :: TransactionOutputs -> Int

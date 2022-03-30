{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Value
  ( module Export
  , noAdaValue
  , adaOnlyValue
  , isAdaOnlyValue
  , currencyValueOf
  ) where

import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value as Export
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (Bool, Eq (..), Maybe (..), (-), mempty)

{-# INLINABLE noAdaValue #-}
-- | Value without any Ada.
noAdaValue :: Value -> Value
noAdaValue v = v - adaOnlyValue v

{-# INLINABLE adaOnlyValue #-}
-- | Value without any non-Ada.
adaOnlyValue :: Value -> Value
adaOnlyValue v = Ada.toValue (Ada.fromValue v)

{-# INLINABLE isAdaOnlyValue #-}
isAdaOnlyValue :: Value -> Bool
isAdaOnlyValue v = adaOnlyValue v == v

{-# INLINABLE currencyValueOf #-}
-- | Get the quantities of just the given 'CurrencySymbol' in the 'Value'. This
-- is useful when implementing minting policies as they are responsible for
-- checking all minted/burnt tokens of their own 'CurrencySymbol'.
currencyValueOf :: Value -> CurrencySymbol -> Value
currencyValueOf (Value m) c = case Map.lookup c m of
    Nothing -> mempty
    Just t  -> Value (Map.singleton c t)

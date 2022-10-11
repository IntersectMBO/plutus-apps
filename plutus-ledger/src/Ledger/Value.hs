{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Value
  ( module Export
  , noAdaValue
  , adaOnlyValue
  , isAdaOnlyValue
  ) where

import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value as Export
import PlutusTx.Prelude (Bool, Eq (..), (-))

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

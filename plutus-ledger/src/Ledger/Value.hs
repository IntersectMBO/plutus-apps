{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Value
  ( module Plutus.V1.Ledger.Value
  , noAdaValue
  , adaOnlyValue
  , isAdaOnlyValue
  ) where

import Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value
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

{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Value
  ( module Export
  , noAdaValue
  ) where

import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value as Export
import PlutusTx.Prelude ((-))

{-# INLINABLE noAdaValue #-}
-- | Value without any Ada.
noAdaValue :: Value -> Value
noAdaValue v = v - Ada.toValue (Ada.fromValue v)


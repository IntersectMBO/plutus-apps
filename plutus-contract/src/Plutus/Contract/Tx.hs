{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
module Plutus.Contract.Tx where

import Control.Lens
import Data.Maybe (fromMaybe)

import Data.Map (Map)
import Ledger.Address qualified as Address
import Ledger.Constraints.TxConstraints (UntypedConstraints)
import Ledger.Tx (ChainIndexTxOut)
import Plutus.Contract.Typed.Tx qualified as Typed
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), TxOutRef, Validator)
import PlutusTx qualified

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
collectFromScript
    :: Map Address.Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScript = collectFromScriptFilter (\_ -> const True)

-- | See
collectFromScriptFilter
    :: (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map Address.Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptFilter flt am vls (Redeemer red) =
    let mp'  = fromMaybe mempty $ am ^. at (Address.scriptAddress vls)
    in Typed.collectFromScriptFilter @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red

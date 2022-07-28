{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
module Ledger.Constraints.OffChain.V1 where

import Control.Lens (at, (^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Ledger.Address (Address)
import Ledger.Constraints.TxConstraints (TxConstraints, UntypedConstraints, mustSpendOutputFromTheScript)
import Ledger.Tx (ChainIndexTxOut)
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), TxOutRef, Validator)
import PlutusTx qualified

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
collectFromScript
    :: Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScript = collectFromScriptFilter (\_ -> const True)

collectFromScriptFilter
    :: (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptFilter flt am vls (Redeemer red) =
    let mp'  = fromMaybe mempty $ am ^. at (PV1.mkValidatorAddress vls)
    in collectFromTheScriptFilter @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red

-- | Given the pay to script address of the 'Validator', collect from it
--   all the outputs that match a predicate, using the 'RedeemerValue'.
collectFromTheScriptFilter ::
    forall i o
    .  (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map.Map TxOutRef ChainIndexTxOut
    -> i
    -> TxConstraints i o
collectFromTheScriptFilter flt utxo red =
    let ourUtxo :: Map.Map TxOutRef ChainIndexTxOut
        ourUtxo = Map.filterWithKey flt utxo
    in collectFromTheScript ourUtxo red

-- | A version of 'collectFromScript' that selects all outputs at the address
-- of the provided typed validator in the lookups.
collectFromTheScript ::
    forall i o
    .  Map.Map TxOutRef ChainIndexTxOut
    -> i
    -> TxConstraints i o
collectFromTheScript utxo redeemer =
    foldMap (flip mustSpendOutputFromTheScript redeemer) $ Map.keys utxo

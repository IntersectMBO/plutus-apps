module Ledger.Constraints.OffChain.V2 where

import Control.Lens (at, (^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Ledger.Address (Address)
import Ledger.Constraints.TxConstraints (UntypedConstraints, mustSpendScriptOutput)
import Ledger.Tx (ChainIndexTxOut)
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.V2.Ledger.Api (Redeemer, TxOutRef, Validator)

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
collectFromScript
    :: Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScript = collectFromScriptFilter (\_ -> const True)

-- | See
collectFromScriptFilter
    :: (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptFilter flt am vls red = -- (Redeemer red) =
    -- let mp'  = fromMaybe mempty $ am ^. at (PV2.mkValidatorAddress vls)
    -- in collectFromTheScriptFilter @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red
    let mp'  = fromMaybe mempty $ am ^. at (PV2.mkValidatorAddress vls)
        ourUtxo = Map.filterWithKey flt mp'
    in foldMap (flip mustSpendScriptOutput red) $ Map.keys ourUtxo

-- TODO Uncomment and modify once PlutusV2 TypedValidator are available
-- -- | Given the pay to script address of the 'Validator', collect from it
-- --   all the outputs that match a predicate, using the 'RedeemerValue'.
-- collectFromTheScriptFilter ::
--     forall i o
--     .  (TxOutRef -> ChainIndexTxOut -> Bool)
--     -> Map.Map TxOutRef ChainIndexTxOut
--     -> i
--     -> TxConstraints i o
-- collectFromTheScriptFilter flt utxo red =
--     let ourUtxo :: Map.Map TxOutRef ChainIndexTxOut
--         ourUtxo = Map.filterWithKey flt utxo
--     in collectFromTheScript ourUtxo red

-- -- | A version of 'collectFromScript' that selects all outputs
-- --   at the address
-- collectFromTheScript ::
--     forall i o
--     .  Map.Map TxOutRef ChainIndexTxOut
--     -> i
--     -> TxConstraints i o
-- collectFromTheScript utxo redeemer =
--     foldMap (flip mustSpendOutputFromTheScript redeemer) $ Map.keys utxo

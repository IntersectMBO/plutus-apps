{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Functions for working with the contract interface using typed transactions.
module Plutus.Contract.Typed.Tx where

import Ledger (TxOutRef)
import Ledger.Constraints (TxConstraints, mustSpendOutputFromTheScript)
import Ledger.Tx (OffChainTxOut)

import Data.Map qualified as Map

-- | Given the pay to script address of the 'Validator', collect from it
--   all the outputs that match a predicate, using the 'RedeemerValue'.
collectFromScriptFilter ::
    forall i o
    .  (TxOutRef -> OffChainTxOut -> Bool)
    -> Map.Map TxOutRef OffChainTxOut
    -> i
    -> TxConstraints i o
collectFromScriptFilter flt utxo red =
    let ourUtxo :: Map.Map TxOutRef OffChainTxOut
        ourUtxo = Map.filterWithKey flt utxo
    in collectFromScript ourUtxo red

-- | A version of 'collectFromScript' that selects all outputs
--   at the address
collectFromScript ::
    forall i o
    .  Map.Map TxOutRef OffChainTxOut
    -> i
    -> TxConstraints i o
collectFromScript utxo redeemer =
    foldMap (flip mustSpendOutputFromTheScript redeemer) $ Map.keys utxo

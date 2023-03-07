{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , ChainIndexTxOutputs(..)
    , ChainIndexTxOut(..)
    , ReferenceScript(..)
    , Address(..)
    , OutputDatum(..)
    , Value(..)
    , fromOnChainTx
    , txOuts
    , txOutRefs
    , txOutsWithRef
    , txOutRefMap
    , txOutRefMapForAddr
    , txRedeemersWithHash
    , validityFromChainIndex
    -- ** Lenses
    , citxTxId
    , citxInputs
    , citxOutputs
    , citxValidRange
    , citxData
    , citxRedeemers
    , citxScripts
    , citxCardanoTx
    , _InvalidTx
    , _ValidTx
    ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Ledger (CardanoTx (CardanoTx), OnChainTx (..), TxOutRef (..))
import Ledger.Address (CardanoAddress)
import Ledger.Scripts (Redeemer, RedeemerHash)
import Plutus.ChainIndex.Types
import Plutus.Contract.CardanoAPI (fromCardanoTx, setValidity)
import Plutus.Script.Utils.Scripts (redeemerHash)
import Plutus.V2.Ledger.Api (Address (..), OutputDatum (..), Value (..))

-- | Get tx outputs from tx.
txOuts :: ChainIndexTx -> [ChainIndexTxOut]
txOuts ChainIndexTx { _citxOutputs = ValidTx outputs }         = outputs
txOuts ChainIndexTx { _citxOutputs = InvalidTx (Just output) } = [ output ]
txOuts ChainIndexTx { _citxOutputs = InvalidTx Nothing }       = []

-- | Get tx output references from tx.
txOutRefs :: ChainIndexTx -> [TxOutRef]
txOutRefs tx = [ TxOutRef (_citxTxId tx) (fromIntegral idx) | idx <- [0 .. length (txOuts tx) - 1] ]

-- | Get tx output references and tx outputs from tx.
txOutsWithRef :: ChainIndexTx -> [(ChainIndexTxOut, TxOutRef)]
txOutsWithRef tx = zip (txOuts tx) (txOutRefs tx)

-- | Get 'Map' of tx outputs references to tx.
txOutRefMap :: ChainIndexTx -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
txOutRefMap tx =
    fmap (, tx) $ Map.fromList $ fmap swap $ txOutsWithRef tx

-- | Get 'Map' of tx outputs from tx for a specific address.
txOutRefMapForAddr :: CardanoAddress -> ChainIndexTx -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
txOutRefMapForAddr addr tx =
    Map.filter ((==) addr . citoAddress . fst) $ txOutRefMap tx

validityFromChainIndex :: ChainIndexTx -> TxValidity
validityFromChainIndex tx =
  case _citxOutputs tx of
    InvalidTx _ -> TxInvalid
    ValidTx _   -> TxValid

-- | Convert a 'OnChainTx' to a 'ChainIndexTx'. An invalid 'OnChainTx' will not
-- produce any 'ChainIndexTx' outputs and the collateral inputs of the
-- 'OnChainTx' will be the inputs of the 'ChainIndexTx'.
--
-- Cardano api transactions store validity internally. Our emulated blockchain stores validity outside of the transactions,
-- so we need to make sure these match up.
fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid (CardanoTx tx era)   -> fromCardanoTx era $ setValidity True tx
    Invalid (CardanoTx tx era) -> fromCardanoTx era $ setValidity False tx

txRedeemersWithHash :: ChainIndexTx -> Map RedeemerHash Redeemer
txRedeemersWithHash ChainIndexTx{_citxRedeemers} = Map.fromList
    $ fmap (\r -> (redeemerHash r, r))
    $ Map.elems _citxRedeemers

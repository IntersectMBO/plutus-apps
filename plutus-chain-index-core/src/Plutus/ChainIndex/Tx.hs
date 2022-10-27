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

import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Ledger (OnChainTx (..), ScriptTag (Cert, Mint, Reward), SomeCardanoApiTx (SomeTx), Tx (..),
               TxInput (txInputType), TxOut (getTxOut), TxOutRef (..), onCardanoTx, txCertifyingRedeemers, txId,
               txMintingRedeemers, txRewardingRedeemers)
import Ledger.Address (Address)
import Ledger.Scripts (Redeemer, RedeemerHash)
import Ledger.Tx (TxInputType (TxScriptAddress), fillTxInputWitnesses)
import Plutus.ChainIndex.Types
import Plutus.Contract.CardanoAPI (fromCardanoTx, fromCardanoTxOut, setValidity)
import Plutus.Script.Utils.Scripts (redeemerHash)
import Plutus.V1.Ledger.Tx (RedeemerPtr (RedeemerPtr), Redeemers, ScriptTag (Spend))
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
txOutRefMapForAddr :: Address -> ChainIndexTx -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
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
fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid ctx ->
        onCardanoTx
            (\case tx@Tx{txInputs, txOutputs, txValidRange, txData, txScripts} ->
                    ChainIndexTx
                        { _citxTxId = txId tx
                        , _citxInputs = map (fillTxInputWitnesses tx) txInputs
                        , _citxOutputs = ValidTx $ map (fromCardanoTxOut . getTxOut) txOutputs
                        , _citxValidRange = txValidRange
                        , _citxData = txData
                        , _citxRedeemers = calculateRedeemerPointers tx
                        , _citxScripts = txScripts
                        , _citxCardanoTx = Nothing
                        }
            )
            (fromOnChainCardanoTx True)
            ctx
    Invalid ctx ->
        onCardanoTx
            (\case tx@Tx{txCollateralInputs, txReturnCollateral, txValidRange, txData, txScripts} ->
                    ChainIndexTx
                        { _citxTxId = txId tx
                        , _citxInputs = map (fillTxInputWitnesses tx) txCollateralInputs
                        , _citxOutputs = InvalidTx $ fmap (fromCardanoTxOut . getTxOut) txReturnCollateral
                        , _citxValidRange = txValidRange
                        , _citxData = txData
                        , _citxRedeemers = calculateRedeemerPointers tx
                        , _citxScripts = txScripts
                        , _citxCardanoTx = Nothing
                        }
            )
            (fromOnChainCardanoTx False)
            ctx

txRedeemersWithHash :: ChainIndexTx -> Map RedeemerHash Redeemer
txRedeemersWithHash ChainIndexTx{_citxRedeemers} = Map.fromList
    $ fmap (\r -> (redeemerHash r, r))
    $ Map.elems _citxRedeemers

-- Cardano api transactions store validity internally. Our emulated blockchain stores validity outside of the transactions,
-- so we need to make sure these match up. Once we only have cardano api txs this can be removed.
fromOnChainCardanoTx :: Bool -> SomeCardanoApiTx -> ChainIndexTx
fromOnChainCardanoTx validity (SomeTx tx era) = fromCardanoTx era $ setValidity validity tx

-- TODO: the index of the txin is probably incorrect as we take it from the set.
-- To determine the proper index we have to convert the plutus's `TxIn` to cardano-api `TxIn` and
-- sort them by using the standard `Ord` instance.
calculateRedeemerPointers :: Tx -> Redeemers
calculateRedeemerPointers tx = spends <> rewards <> mints <> certs
    -- we sort the inputs to make sure that the indices match with redeemer pointers

    where
        rewards = Map.fromList $ zipWith (\n (_, rd) -> (RedeemerPtr Reward n, rd)) [0..]  $ sort $ Map.assocs $ txRewardingRedeemers tx
        mints   = Map.fromList $ zipWith (\n (_, rd) -> (RedeemerPtr Mint n, rd)) [0..]  $ sort $ Map.assocs $ txMintingRedeemers tx
        certs   = Map.fromList $ zipWith (\n (_, rd) -> (RedeemerPtr Cert n, rd)) [0..]  $ sort $ Map.assocs $ txCertifyingRedeemers tx
        spends = Map.fromList $ mapMaybe (uncurry getRd) $ zip [0..] $ fmap txInputType $ sort $ txInputs tx

        getRd n = \case
            TxScriptAddress rd _ _ -> Just (RedeemerPtr Spend n, rd)
            _                      -> Nothing

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
    , txOutRefs
    , txOutsWithRef
    , txOutRefMap
    , txOutRefMapForAddr
    , txRedeemersWithHash
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

import Cardano.Api (NetworkId)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Ledger (OnChainTx (..), SomeCardanoApiTx (SomeTx), Tx (..), TxIn (..), TxInType (..), TxOutRef (..), onCardanoTx,
               txId)
import Ledger.Tx.CardanoAPI (toCardanoTxOut, toCardanoTxOutDatumHash)
import Plutus.ChainIndex.Types
import Plutus.Contract.CardanoAPI (fromCardanoTx, fromCardanoTxOut, setValidity)
import Plutus.Script.Utils.Scripts (datumHash, redeemerHash)
import Plutus.Script.Utils.V1.Scripts (validatorHash)
import Plutus.V1.Ledger.Api (Datum, DatumHash, MintingPolicy (getMintingPolicy), MintingPolicyHash (MintingPolicyHash),
                             Redeemer, RedeemerHash, Script, Validator (getValidator), ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Scripts (ScriptHash (ScriptHash))
import Plutus.V1.Ledger.Tx (RedeemerPtr (RedeemerPtr), Redeemers, ScriptTag (Spend))
import Plutus.V2.Ledger.Api (Address (..), OutputDatum (..), Value (..))

-- | Get tx output references from tx.
txOutRefs :: ChainIndexTx -> [TxOutRef]
txOutRefs ChainIndexTx { _citxTxId, _citxOutputs = ValidTx outputs } =
    map (\idx -> TxOutRef _citxTxId idx) [0 .. fromIntegral $ length outputs - 1]
txOutRefs ChainIndexTx{_citxOutputs = InvalidTx} = []

-- | Get tx output references and tx outputs from tx.
txOutsWithRef :: ChainIndexTx -> [(ChainIndexTxOut, TxOutRef)]
txOutsWithRef tx@ChainIndexTx { _citxOutputs = ValidTx outputs } = zip outputs $ txOutRefs tx
txOutsWithRef ChainIndexTx { _citxOutputs = InvalidTx }          = []

-- | Get 'Map' of tx outputs references to tx.
txOutRefMap :: ChainIndexTx -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
txOutRefMap tx =
    fmap (, tx) $ Map.fromList $ fmap swap $ txOutsWithRef tx

-- | Get 'Map' of tx outputs from tx for a specific address.
txOutRefMapForAddr :: Address -> ChainIndexTx -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
txOutRefMapForAddr addr tx =
    Map.filter ((==) addr . citoAddress . fst) $ txOutRefMap tx

-- | Convert a 'OnChainTx' to a 'ChainIndexTx'. An invalid 'OnChainTx' will not
-- produce any 'ChainIndexTx' outputs and the collateral inputs of the
-- 'OnChainTx' will be the inputs of the 'ChainIndexTx'.
fromOnChainTx :: NetworkId -> OnChainTx -> ChainIndexTx
fromOnChainTx networkId = \case
    Valid ctx ->
        onCardanoTx
            (\tx@Tx{txInputs, txOutputs, txValidRange, txData, txMintScripts} ->
                let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
                ChainIndexTx
                    { _citxTxId = txId tx
                    , _citxInputs = txInputs
                    , _citxOutputs = case traverse (toCardanoTxOut networkId toCardanoTxOutDatumHash) txOutputs of
                        Right txs -> either (const InvalidTx) ValidTx $ traverse fromCardanoTxOut txs
                        Left _    -> InvalidTx
                    , _citxValidRange = txValidRange
                    , _citxData = txData <> otherDataHashes
                    , _citxRedeemers = redeemers
                    , _citxScripts = mintingPolicies txMintScripts <> validatorHashes
                    , _citxCardanoTx = Nothing
                    })
            (fromOnChainCardanoTx True)
            ctx
    Invalid ctx ->
        onCardanoTx
            (\tx@Tx{txCollateral, txValidRange, txData, txInputs, txMintScripts} ->
                let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
                ChainIndexTx
                    { _citxTxId = txId tx
                    , _citxInputs = txCollateral
                    , _citxOutputs = InvalidTx
                    , _citxValidRange = txValidRange
                    , _citxData = txData <> otherDataHashes
                    , _citxRedeemers = redeemers
                    , _citxScripts = mintingPolicies txMintScripts <> validatorHashes
                    , _citxCardanoTx = Nothing
                    })
            (fromOnChainCardanoTx False)
            ctx

-- Cardano api transactions store validity internally. Our emulated blockchain stores validity outside of the transactions,
-- so we need to make sure these match up. Once we only have cardano api txs this can be removed.
fromOnChainCardanoTx :: Bool -> SomeCardanoApiTx -> ChainIndexTx
fromOnChainCardanoTx validity (SomeTx tx era) =
    either (error . ("Plutus.ChainIndex.Tx.fromOnChainCardanoTx: " ++) . show) id $ fromCardanoTx era $ setValidity validity tx

mintingPolicies :: Map MintingPolicyHash MintingPolicy -> Map ScriptHash Script
mintingPolicies = Map.fromList . fmap toScript . Map.toList
  where
    toScript (MintingPolicyHash mph, mp) = (ScriptHash mph, getMintingPolicy mp)

validators :: [TxIn] -> (Map ScriptHash Script, Map DatumHash Datum, Redeemers)
validators = foldMap (\(ix, txIn) -> maybe mempty (withHash ix) $ txInType txIn) . zip [0..] . sort
  -- we sort the inputs to make sure that the indices match with redeemer pointers
  where
    -- TODO: the index of the txin is probably incorrect as we take it from the set.
    -- To determine the proper index we have to convert the plutus's `TxIn` to cardano-api `TxIn` and
    -- sort them by using the standard `Ord` instance.
    withHash ix (ConsumeScriptAddress _lang val red dat) =
      let (ValidatorHash vh) = validatorHash val
       in ( Map.singleton (ScriptHash vh) (getValidator val)
          , Map.singleton (datumHash dat) dat
          , Map.singleton (RedeemerPtr Spend ix) red
          )
    withHash _ _ = mempty

txRedeemersWithHash :: ChainIndexTx -> Map RedeemerHash Redeemer
txRedeemersWithHash ChainIndexTx{_citxRedeemers} = Map.fromList
    $ fmap (\r -> (redeemerHash r, r))
    $ Map.elems _citxRedeemers

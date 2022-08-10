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

import Cardano.Api (NetworkId, txOutValueToValue)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (Serialise (decode, encode))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Control.Arrow ((&&&))
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Ledger (OnChainTx (..), SomeCardanoApiTx (SomeTx), Tx (..), TxIn (..), TxInType (..), TxOutRef (..), onCardanoTx,
               txId)
import Ledger.Address (Address)
import Ledger.Blockchain (OnChainTx (..))
import Ledger.Scripts (Datum, DatumHash, Redeemer, RedeemerHash, Script, ScriptHash, redeemerHash)
import Ledger.Slot (SlotRange)
import Ledger.Tx (Certificate (certificateRedeemer), SomeCardanoApiTx, TxId, TxIn, TxInput (txInputType),
                  TxInputType (TxConsumeScriptAddress), TxOutRef (TxOutRef), Withdrawal (withdrawalRedeemer),
                  fillTxInputWitnesses, txId)
import Ledger.Tx.CardanoAPI (FromCardanoError, fromCardanoAddress, fromCardanoTxOutDatum, fromCardanoValue,
                             toCardanoTxOut, toCardanoTxOutBabbage, toCardanoTxOutDatumHash,
                             toCardanoTxOutDatumHashBabbage)
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
            (\case tx@Tx{txInputs, txOutputs, txValidRange, txData, txScripts, txWithdrawals, txCertificates, txMintingScripts} ->
                    let redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txInputs
                    in
                    ChainIndexTx
                        { _citxTxId = txId tx
                        , _citxInputs = Set.fromList $ map (fillTxInputWitnesses tx) txInputs
                        , _citxOutputs = case traverse (toCardanoTxOutBabbage networkId toCardanoTxOutDatumHashBabbage) txOutputs of
                            Right txs -> either (const InvalidTx) ValidTx $ traverse fromTxOut txs
                            Left _    -> InvalidTx
                        , _citxValidRange = txValidRange
                        , _citxData = txData
                        , _citxRedeemers = redeemersToMap redeemers
                        , _citxScripts = txScripts
                        , _citxCardanoTx = Nothing
                        }
            )
            (fromOnChainCardanoTx True)
            ctx
    Invalid ctx ->
        onCardanoTx
            (\case tx@Tx{txCollateral, txValidRange, txData, txScripts, txMintingScripts, txWithdrawals, txCertificates} ->
                    let  redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txCollateral in
                    ChainIndexTx
                        { _citxTxId = txId tx
                        , _citxInputs = Set.fromList $ map (fillTxInputWitnesses tx) txCollateral
                        , _citxOutputs = InvalidTx
                        , _citxValidRange = txValidRange
                        , _citxData = txData
                        , _citxRedeemers = redeemersToMap redeemers
                        , _citxScripts = txScripts
                        , _citxCardanoTx = Nothing
                        }
            )
            (fromOnChainCardanoTx False)
            ctx
    where
        redeemersToMap :: [Redeemer] -> Map RedeemerHash Redeemer
        redeemersToMap = Map.fromList . map (redeemerHash &&& id)

        allRedeemers txWithdrawals txCertificates txMintingScripts txInputs =
            mapMaybe withdrawalRedeemer txWithdrawals
            <> mapMaybe certificateRedeemer txCertificates
            <> Map.elems txMintingScripts
            <> mapMaybe (
                (\case
                    TxConsumeScriptAddress rd _ _ -> Just rd
                    _                             -> Nothing) . txInputType) txInputs

txRedeemersWithHash :: ChainIndexTx -> Map RedeemerHash Redeemer
txRedeemersWithHash ChainIndexTx{_citxRedeemers} = Map.fromList
    $ fmap (\r -> (redeemerHash r, r))
    $ Map.elems _citxRedeemers

-- Cardano api transactions store validity internally. Our emulated blockchain stores validity outside of the transactions,
-- so we need to make sure these match up. Once we only have cardano api txs this can be removed.
fromOnChainCardanoTx :: Bool -> SomeCardanoApiTx -> ChainIndexTx
fromOnChainCardanoTx validity (SomeTx tx era) =
    either (error . ("Plutus.ChainIndex.Tx.fromOnChainCardanoTx: " ++) . show) id $ fromCardanoTx era $ setValidity validity tx


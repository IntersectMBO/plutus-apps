{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , ChainIndexTxOutputs(..)
    , fromOnChainTx
    , txOutRefs
    , txOutsWithRef
    , txOutRefMap
    , txOutRefMapForAddr
    , ChainIndexTxOut(..)
    , fromTxOut
    , ReferenceScript(..)
    , fromCardanoTxOutRefScript
    , Address(..)
    , OutputDatum(..)
    , Value(..)
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Ledger (Tx (..))
import Ledger.Address (Address)
import Ledger.Blockchain (OnChainTx (..))
import Ledger.Scripts (Datum, DatumHash, Redeemer, RedeemerHash, Script, ScriptHash, redeemerHash)
import Ledger.Slot (SlotRange)
import Ledger.Tx (Certificate (certificateRedeemer), SomeCardanoApiTx, TxId, TxIn, TxInput (txInputType),
                  TxInputType (TxConsumeScriptAddress), TxOutRef (TxOutRef), Withdrawal (withdrawalRedeemer),
                  fillTxInputWitnesses, txId)
import Ledger.Tx.CardanoAPI (FromCardanoError, fromCardanoAddress, fromCardanoTxOutDatum, fromCardanoValue,
                             toCardanoTxOutBabbage, toCardanoTxOutDatumHashBabbage)
import Plutus.V2.Ledger.Api (Address (..), OutputDatum (..), Value (..))
import Prettyprinter

instance Serialise C.ScriptInAnyLang where
    encode (C.ScriptInAnyLang lang script) =
        let
            -- Since lang is a GADT we have to encode the script in all branches
            other = case lang of
                C.SimpleScriptLanguage C.SimpleScriptV1 -> encodeWord 0 <> encode (C.serialiseToCBOR script)
                C.SimpleScriptLanguage C.SimpleScriptV2 -> encodeWord 1 <> encode (C.serialiseToCBOR script)
                C.PlutusScriptLanguage C.PlutusScriptV1 -> encodeWord 2 <> encode (C.serialiseToCBOR script)
                C.PlutusScriptLanguage C.PlutusScriptV2 -> encodeWord 3 <> encode (C.serialiseToCBOR script)
        in encodeListLen 2 <> other
    decode = do
        len <- decodeListLen
        langWord <- decodeWord
        script <- decode
        case (len, langWord) of
            (2, 0) -> do
                let decoded = either (error "Failed to deserialise AsSimpleScriptV1 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsSimpleScriptV1) script)
                pure $ C.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV1) decoded
            (2, 1) -> do
                let decoded = either (error "Failed to deserialise AsSimpleScriptV2 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsSimpleScriptV2) script)
                pure $ C.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV2) decoded
            (2, 2) -> do
                let decoded = either (error "Failed to deserialise AsPlutusScriptV1 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsPlutusScriptV1) script)
                pure $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) decoded
            (2, 3) -> do
                let decoded = either (error "Failed to deserialise AsPlutusScriptV2 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsPlutusScriptV2) script)
                pure $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) decoded
            _ -> fail "Invalid ScriptInAnyLang encoding"

instance OpenApi.ToSchema C.ScriptInAnyLang where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "ScriptInAnyLang") mempty

data ReferenceScript = ReferenceScriptNone | ReferenceScriptInAnyLang C.ScriptInAnyLang
  deriving (Eq, Show, Generic, Serialise, OpenApi.ToSchema)

instance ToJSON ReferenceScript where
  toJSON (ReferenceScriptInAnyLang s) = object ["referenceScript" .= s]
  toJSON ReferenceScriptNone          = Aeson.Null

instance FromJSON ReferenceScript where
  parseJSON = Aeson.withObject "ReferenceScript" $ \o ->
    case Aeson.lookup "referenceScript" o of
      Nothing        -> pure ReferenceScriptNone
      Just refScript -> ReferenceScriptInAnyLang <$> parseJSON refScript

fromCardanoTxOutRefScript :: C.ReferenceScript era -> ReferenceScript
fromCardanoTxOutRefScript = \case
    C.ReferenceScriptNone      -> ReferenceScriptNone
    C.ReferenceScript _ script -> ReferenceScriptInAnyLang script

data ChainIndexTxOut = ChainIndexTxOut
  { citoAddress   :: Address -- ^ We can't use AddressInAnyEra here because of missing FromJson instance for Byron era
  , citoValue     :: Value
  , citoDatum     :: OutputDatum
  , citoRefScript :: ReferenceScript
  } deriving (Eq, Show, Generic, Serialise, OpenApi.ToSchema)

instance ToJSON ChainIndexTxOut where
    toJSON ChainIndexTxOut{..} = object
        [ "address" .= toJSON citoAddress
        , "value" .= toJSON citoValue
        , "datum" .= toJSON citoDatum
        , "refScript" .= toJSON citoRefScript
        ]

instance FromJSON ChainIndexTxOut where
    parseJSON =
        Aeson.withObject "ChainIndexTxOut" $ \obj ->
            ChainIndexTxOut
                <$> obj .: "address"
                <*> obj .: "value"
                <*> obj .: "datum"
                <*> obj .:? "refScript" .!= ReferenceScriptNone

-- | List of outputs of a transaction. There are no outputs if the transaction
-- is invalid.
data ChainIndexTxOutputs =
    InvalidTx -- ^ The transaction is invalid so there is no outputs
  | ValidTx [ChainIndexTxOut]
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialise, OpenApi.ToSchema)

makePrisms ''ChainIndexTxOutputs

data ChainIndexTx = ChainIndexTx {
    _citxTxId       :: TxId,
    -- ^ The id of this transaction.
    _citxInputs     :: Set TxIn,
    -- ^ The inputs to this transaction.
    _citxOutputs    :: ChainIndexTxOutputs,
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    _citxValidRange :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    _citxData       :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    _citxRedeemers  :: Map RedeemerHash Redeemer,
    -- ^ Redeemers of the minting scripts.
    _citxScripts    :: Map ScriptHash Script,
    -- ^ The scripts (validator, stake validator or minting) part of cardano tx.
    _citxCardanoTx  :: Maybe SomeCardanoApiTx
    -- ^ The full Cardano API tx which was used to populate the rest of the
    -- 'ChainIndexTx' fields. Useful because 'ChainIndexTx' doesn't have all the
    -- details of the tx, so we keep it as a safety net. Might be Nothing if we
    -- are in the emulator.
    } deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialise, OpenApi.ToSchema)

makeLenses ''ChainIndexTx

instance Pretty ChainIndexTx where
    pretty ChainIndexTx{_citxTxId, _citxInputs, _citxOutputs = ValidTx outputs, _citxValidRange, _citxData, _citxRedeemers, _citxScripts} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ("outputs:" : fmap viaShow outputs))
                , hang 2 (vsep ("scripts hashes:": fmap (pretty . fst) (Map.toList _citxScripts)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Valid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]
    pretty ChainIndexTx{_citxTxId, _citxInputs, _citxOutputs = InvalidTx, _citxValidRange, _citxData, _citxRedeemers, _citxScripts} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ["no outputs:"])
                , hang 2 (vsep ("scripts hashes:": fmap (pretty . fst) (Map.toList _citxScripts)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Invalid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]

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

fromTxOut :: C.TxOut C.CtxTx era -> Either FromCardanoError ChainIndexTxOut
fromTxOut (C.TxOut addr val datum refScript) =
    ChainIndexTxOut
        <$> fromCardanoAddress addr
        <*> (pure $ fromCardanoValue $ txOutValueToValue val)
        <*> (pure $ fromCardanoTxOutDatum datum)
        <*> (pure $ fromCardanoTxOutRefScript refScript)

-- | Convert a 'OnChainTx' to a 'ChainIndexTx'. An invalid 'OnChainTx' will not
-- produce any 'ChainIndexTx' outputs and the collateral inputs of the
-- 'OnChainTx' will be the inputs of the 'ChainIndexTx'.
fromOnChainTx :: NetworkId -> OnChainTx -> ChainIndexTx
fromOnChainTx networkId = \case
    Valid tx@Tx{txInputs, txOutputs, txValidRange, txData, txScripts, txWithdrawals, txCertificates, txMintingScripts} ->
        let  redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txInputs
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
    Invalid tx@Tx{txCollateral, txValidRange, txData, txScripts, txMintingScripts, txWithdrawals, txCertificates} ->
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

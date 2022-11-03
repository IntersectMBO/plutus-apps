{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ledger.Tx
    ( module Export
    -- * DecoratedTxOut
    , DecoratedTxOut(..)
    , toTxOut
    , toTxInfoTxOut
    -- ** Lenses and Prisms
    , decoratedTxOutPubKeyHash
    , decoratedTxOutAddress
    , decoratedTxOutDatum
    , decoratedTxOutValue
    , decoratedTxOutPubKeyDatum
    , decoratedTxOutScriptDatum
    , decoratedTxOutStakingCredential
    , decoratedTxOutReferenceScript
    , decoratedTxOutValidatorHash
    , decoratedTxOutValidator
    , _PublicKeyDecoratedTxOut
    , _ScriptDecoratedTxOut
    , _decoratedTxOutAddress
    -- ** smart Constructors
    , mkDecoratedTxOut
    , mkPubkeyDecoratedTxOut
    , mkScriptDecoratedTxOut
    -- * DatumFromQuery
    , DatumFromQuery(..)
    , datumInDatumFromQuery
    -- * Transactions
    , CardanoTx(..)
    , cardanoApiTx
    , emulatorTx
    , onCardanoTx
    , cardanoTxMap
    , getCardanoTxId
    , getCardanoTxInputs
    , getCardanoTxCollateralInputs
    , getCardanoTxOutRefs
    , getCardanoTxOutputs
    , getCardanoTxSpentOutputs
    , getCardanoTxProducedOutputs
    , getCardanoTxReturnCollateral
    , getCardanoTxProducedReturnCollateral
    , getCardanoTxTotalCollateral
    , getCardanoTxFee
    , getCardanoTxMint
    , getCardanoTxValidityRange
    , getCardanoTxData
    , SomeCardanoApiTx(.., CardanoApiEmulatorEraTx)
    , ToCardanoError(..)
    , addSignature
    , addSignature'
    , addCardanoTxSignature
    , pubKeyTxOut
    , updateUtxo
    , updateUtxoCollateral
    , txOutRefs
    , unspentOutputsTx
    -- * Hashing transactions
    , txId
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxWitness (txwitsVKey)
import Cardano.Ledger.Babbage.TxBody (TxBody)

import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise (encode))
import Control.DeepSeq (NFData)
import Control.Lens (At (at), Getter, Traversal', makeLenses, makePrisms, to, views, (&), (?~), (^.), (^?))
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Proxy (Proxy))
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.OpenApi qualified as OpenApi
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)

import Ledger.Address (Address, PaymentPubKey, StakePubKey, pubKeyAddress)
import Ledger.Crypto (Passphrase, signTx, signTx', toPublicKey)
import Ledger.Orphans ()
import Ledger.Params (EmulatorEra, Params (pNetworkId))
import Ledger.Slot (SlotRange)
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI

import Plutus.Script.Utils.Scripts (scriptHash)
import Plutus.V1.Ledger.Api qualified as V1
import Plutus.V1.Ledger.Tx qualified as V1.Tx hiding (TxIn (..), TxInType (..))
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Tx qualified as V2.Tx hiding (TxIn (..), TxInType (..))

import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))
-- for re-export
import Ledger.Tx.Internal as Export
import Plutus.V1.Ledger.Tx as Export hiding (TxIn (..), TxInType (..), TxOut (..), inRef, inScripts, inType, outAddress,
                                      outValue, pubKeyTxIn, pubKeyTxIns, scriptTxIn, scriptTxIns, txOutPubKey)

-- | A datum in a transaction output that comes from a chain index query.
data DatumFromQuery
    = DatumUnknown
    | DatumInline V2.Datum
    | DatumInBody V2.Datum
    deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, NFData, OpenApi.ToSchema)

makePrisms ''DatumFromQuery

datumInDatumFromQuery :: Traversal' DatumFromQuery V2.Datum
datumInDatumFromQuery _ DatumUnknown    = pure DatumUnknown
datumInDatumFromQuery f (DatumInline d) = DatumInline <$> f d
datumInDatumFromQuery f (DatumInBody d) = DatumInBody <$> f d

-- | Offchain view of a transaction output.
data DecoratedTxOut =
    PublicKeyDecoratedTxOut {
      -- | The pubKey hash that protects the transaction address
      _decoratedTxOutPubKeyHash        :: V1.PubKeyHash,
      -- | The staking credential of the transaction address, if any
      _decoratedTxOutStakingCredential :: Maybe V1.StakingCredential,
      -- | Value of the transaction output.
      _decoratedTxOutValue             :: V2.Value,
      -- | Optional datum (inline datum or datum in transaction body) attached to the transaction output.
      _decoratedTxOutPubKeyDatum       :: Maybe (V2.DatumHash, DatumFromQuery),
      -- | Value of the transaction output.
      _decoratedTxOutReferenceScript   :: Maybe (Versioned V1.Script)
    }
  | ScriptDecoratedTxOut {
      -- | The hash of the script that protects the transaction address
      _decoratedTxOutValidatorHash     :: V1.ValidatorHash,
      -- | The staking credential of the transaction address, if any
      _decoratedTxOutStakingCredential :: Maybe V1.StakingCredential,
      -- | Value of the transaction output.
      _decoratedTxOutValue             :: V2.Value,
      -- | Datum attached to the transaction output, either in full (inline datum or datum in transaction body) or as a
      -- hash reference. A transaction output protected by a Plutus script
      -- is guardateed to have an associated datum.
      _decoratedTxOutScriptDatum       :: (V2.DatumHash, DatumFromQuery),
      -- The reference script is, in genereal, unrelated to the validator
      -- script althought it could also be the same.
      _decoratedTxOutReferenceScript   :: Maybe (Versioned V1.Script),
      -- | Full version of the validator protecting the transaction output
      _decoratedTxOutValidator         :: Maybe (Versioned V1.Validator)
  }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, NFData, OpenApi.ToSchema)

makeLenses ''DecoratedTxOut
makePrisms ''DecoratedTxOut


mkDecoratedTxOut
    :: V1.Address -> V2.Value -> (V2.DatumHash, DatumFromQuery) -> Maybe (Versioned V1.Script)
    -> DecoratedTxOut
mkDecoratedTxOut a v dat rs = case a of
  V2.Address (V2.PubKeyCredential c) sc -> PublicKeyDecoratedTxOut c sc v (Just dat) rs
  V2.Address (V2.ScriptCredential c) sc -> ScriptDecoratedTxOut c sc v dat rs Nothing

mkPubkeyDecoratedTxOut
    :: V1.Address -> V2.Value -> Maybe (V2.DatumHash, DatumFromQuery) -> Maybe (Versioned V1.Script)
    -> Maybe DecoratedTxOut
mkPubkeyDecoratedTxOut a v dat rs = case a of
  V2.Address (V2.PubKeyCredential c) sc -> Just $ PublicKeyDecoratedTxOut c sc v dat rs
  _                                     -> Nothing

mkScriptDecoratedTxOut
    :: V1.Address
    -> V2.Value
    -> (V2.DatumHash, DatumFromQuery)
    -> Maybe (Versioned V1.Script)
    -> Maybe (Versioned V1.Validator)
    -> Maybe DecoratedTxOut
mkScriptDecoratedTxOut a v dat rs val = case a of
  V2.Address (V2.ScriptCredential c) sc -> Just $ ScriptDecoratedTxOut c sc v dat rs val
  _                                     -> Nothing

_decoratedTxOutAddress :: DecoratedTxOut -> Address
_decoratedTxOutAddress PublicKeyDecoratedTxOut{_decoratedTxOutPubKeyHash, _decoratedTxOutStakingCredential} =
    V1.Address (V1.PubKeyCredential _decoratedTxOutPubKeyHash) _decoratedTxOutStakingCredential
_decoratedTxOutAddress ScriptDecoratedTxOut{_decoratedTxOutValidatorHash, _decoratedTxOutStakingCredential} =
    V1.Address (V1.ScriptCredential _decoratedTxOutValidatorHash) _decoratedTxOutStakingCredential

decoratedTxOutAddress :: Getter DecoratedTxOut Address
decoratedTxOutAddress = to _decoratedTxOutAddress

decoratedTxOutDatum :: Traversal' DecoratedTxOut (V2.DatumHash, DatumFromQuery)
decoratedTxOutDatum f p@(PublicKeyDecoratedTxOut pkh sc v dat rs) =
  maybe (pure p) (fmap (\ dat' -> PublicKeyDecoratedTxOut pkh sc v (Just dat') rs) . f) dat
decoratedTxOutDatum f (ScriptDecoratedTxOut vh sc v dat rs val) =
  (\dat' -> ScriptDecoratedTxOut vh sc v dat' rs val) <$> f dat

toTxOut :: C.NetworkId -> DecoratedTxOut -> Either ToCardanoError TxOut
toTxOut networkId p =
  TxOut <$> (C.TxOut
    <$> CardanoAPI.toCardanoAddressInEra networkId (p ^. decoratedTxOutAddress)
    <*> CardanoAPI.toCardanoTxOutValue (p ^. decoratedTxOutValue)
    <*> (toTxOutDatum $ p ^? decoratedTxOutDatum)
    <*> CardanoAPI.toCardanoReferenceScript (p ^. decoratedTxOutReferenceScript))

toTxOutDatum :: Maybe (V2.DatumHash, DatumFromQuery) -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.BabbageEra)
toTxOutDatum = CardanoAPI.toCardanoTxOutDatum . toPlutusOutputDatum

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that 'DecoratedTxOut' supports features such inline datums and
-- reference scripts which are not supported by V1 TxOut. Converting from
-- 'DecoratedTxOut' to 'TxOut' and back is therefore lossy.
toTxInfoTxOut :: DecoratedTxOut -> V2.Tx.TxOut
toTxInfoTxOut p =
    V2.Tx.TxOut (p ^. decoratedTxOutAddress) (p ^. decoratedTxOutValue)
                (toPlutusOutputDatum $ p ^? decoratedTxOutDatum)
                (views decoratedTxOutReferenceScript (fmap scriptHash) p)

toPlutusOutputDatum :: Maybe (V2.DatumHash, DatumFromQuery) -> V2.Tx.OutputDatum
toPlutusOutputDatum Nothing                   = V2.Tx.NoOutputDatum
toPlutusOutputDatum (Just (_, DatumInline d)) = V2.Tx.OutputDatum d
toPlutusOutputDatum (Just (dh, _))            = V2.Tx.OutputDatumHash dh

instance Pretty DecoratedTxOut where
    pretty p =
      hang 2 $ vsep [ "-" <+> pretty (p ^. decoratedTxOutValue) <+> "addressed to"
                    , pretty (p ^. decoratedTxOutAddress)]

{- Note [Why we have the Both constructor in CardanoTx]

We want to do validation with both the emulator and with the cardano-ledger library, at least as long
as we don't have Phase2 validation errors via the cardano-ledger library.

To do that we need the required signers which are only available in UnbalancedTx during balancing.
So during balancing we can create the SomeCardanoApiTx, while proper validation can only happen in
Wallet.Emulator.Chain.validateBlock, since that's when we know the right Slot number. This means that
we need both transaction types in the path from balancing to validateBlock. -}
data CardanoTx
    = EmulatorTx { _emulatorTx :: Tx }
    | CardanoApiTx { _cardanoApiTx :: SomeCardanoApiTx }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema, Serialise)

makeLenses ''CardanoTx

getEmulatorEraTx :: SomeCardanoApiTx -> C.Tx C.BabbageEra
getEmulatorEraTx (SomeTx tx C.BabbageEraInCardanoMode) = tx
getEmulatorEraTx _                                     = error "getEmulatorEraTx: Expected a Babbage tx"

pattern CardanoApiEmulatorEraTx :: C.Tx C.BabbageEra -> SomeCardanoApiTx
pattern CardanoApiEmulatorEraTx tx <- (getEmulatorEraTx -> tx) where
    CardanoApiEmulatorEraTx tx = SomeTx tx C.BabbageEraInCardanoMode

{-# COMPLETE CardanoApiEmulatorEraTx #-}

instance Pretty CardanoTx where
    pretty tx =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (getCardanoTxInputs tx)))
                , hang 2 (vsep ("reference inputs:" : fmap pretty (getCardanoTxReferenceInputs tx)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (getCardanoTxCollateralInputs tx)))
                , hang 2 (vsep ("outputs:" : fmap pretty (getCardanoTxOutputs tx)))
                ]
                <> maybe [] (\out -> [hang 2 (vsep ["return collateral:", pretty out])]) (getCardanoTxReturnCollateral tx)
                <> maybe [] (\val -> ["total collateral:" <+> pretty val]) (getCardanoTxTotalCollateral tx)
                ++ [ "mint:" <+> pretty (getCardanoTxMint tx)
                , "fee:" <+> pretty (getCardanoTxFee tx)
                ] ++ onCardanoTx (\tx' ->
                    [ hang 2 (vsep ("mps:": fmap pretty (Map.toList (txMintingWitnesses tx'))))
                    , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList (txSignatures tx'))))
                    ]) (const []) tx ++
                [ "validity range:" <+> viaShow (getCardanoTxValidityRange tx)
                , hang 2 (vsep ("data:": fmap pretty (Map.toList (getCardanoTxData tx))))
                , hang 2 (vsep ("redeemers:": fmap pretty (Map.elems $ getCardanoTxRedeemers tx)))
                ]
        in nest 2 $ vsep ["Tx" <+> pretty (getCardanoTxId tx) <> colon, braces (vsep lines')]

instance Pretty SomeCardanoApiTx where
  pretty = pretty . CardanoApiTx

instance Pretty CardanoAPI.CardanoBuildTx where
  pretty txBodyContent = case C.makeSignedTransaction [] <$> CardanoAPI.makeTransactionBody mempty txBodyContent of
    Right tx -> pretty $ CardanoApiEmulatorEraTx tx
    _        -> viaShow txBodyContent


onCardanoTx :: (Tx -> r) -> (SomeCardanoApiTx -> r) -> CardanoTx -> r
onCardanoTx l _ (EmulatorTx tx)    = l tx
onCardanoTx _ r (CardanoApiTx ctx) = r ctx

cardanoTxMap :: (Tx -> Tx) -> (SomeCardanoApiTx -> SomeCardanoApiTx) -> CardanoTx -> CardanoTx
cardanoTxMap l _ (EmulatorTx tx)    = EmulatorTx (l tx)
cardanoTxMap _ r (CardanoApiTx ctx) = CardanoApiTx (r ctx)

getCardanoTxId :: CardanoTx -> V1.Tx.TxId
getCardanoTxId = onCardanoTx txId getCardanoApiTxId

getCardanoApiTxId :: SomeCardanoApiTx -> V1.Tx.TxId
getCardanoApiTxId (SomeTx (C.Tx body _) _) = CardanoAPI.fromCardanoTxId $ C.getTxId body

getCardanoTxInputs :: CardanoTx -> [TxIn]
getCardanoTxInputs = onCardanoTx (\tx -> map (fillTxInputWitnesses tx) $ txInputs tx)
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns)

getCardanoTxCollateralInputs :: CardanoTx -> [TxIn]
getCardanoTxCollateralInputs = onCardanoTx
    (\tx -> map (fillTxInputWitnesses tx) $ txCollateralInputs tx)
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        CardanoAPI.fromCardanoTxInsCollateral txInsCollateral)

getCardanoTxReferenceInputs :: CardanoTx -> [TxIn]
getCardanoTxReferenceInputs = onCardanoTx
    (\tx -> map (fillTxInputWitnesses tx) $ txReferenceInputs tx)
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        txInsReferenceToPlutusTxIns txInsReference)
 where
     txInsReferenceToPlutusTxIns C.TxInsReferenceNone = []
     txInsReferenceToPlutusTxIns (C.TxInsReference _ txIns) =
         fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn) txIns

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, V1.Tx.TxOutRef)]
getCardanoTxOutRefs = onCardanoTx txOutRefs cardanoApiTxOutRefs
  where
    cardanoApiTxOutRefs :: SomeCardanoApiTx -> [(TxOut, V1.Tx.TxOutRef)]
    cardanoApiTxOutRefs (CardanoApiEmulatorEraTx (C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _)) =
      mkOut <$> zip [0..] (map TxOut txOuts)
      where
        mkOut (i, o) = (o, V1.TxOutRef (CardanoAPI.fromCardanoTxId $ C.getTxId txBody) i)

getCardanoTxOutputs :: CardanoTx -> [TxOut]
getCardanoTxOutputs = fmap fst . getCardanoTxOutRefs

getCardanoTxProducedOutputs :: CardanoTx -> Map V1.Tx.TxOutRef TxOut
getCardanoTxProducedOutputs = Map.fromList . fmap swap . getCardanoTxOutRefs

getCardanoTxSpentOutputs :: CardanoTx -> Set V1.Tx.TxOutRef
getCardanoTxSpentOutputs = Set.fromList . map txInRef . getCardanoTxInputs

getCardanoTxReturnCollateral :: CardanoTx -> Maybe TxOut
getCardanoTxReturnCollateral = onCardanoTx txReturnCollateral
    (\(CardanoApiEmulatorEraTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) ->
        case txReturnCollateral of
            C.TxReturnCollateralNone     -> Nothing
            C.TxReturnCollateral _ txOut -> Just $ TxOut txOut
    )

getCardanoTxProducedReturnCollateral :: CardanoTx -> Map V1.Tx.TxOutRef TxOut
getCardanoTxProducedReturnCollateral tx = maybe Map.empty (Map.singleton (V1.TxOutRef (getCardanoTxId tx) 0)) $
    getCardanoTxReturnCollateral tx

getCardanoTxTotalCollateral :: CardanoTx -> Maybe V1.Value
getCardanoTxTotalCollateral = onCardanoTx txTotalCollateral
    (\(CardanoApiEmulatorEraTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) ->
        case txTotalCollateral of
            C.TxTotalCollateralNone  -> Nothing
            C.TxTotalCollateral _ lv -> Just $ CardanoAPI.fromCardanoLovelace lv
    )

getCardanoTxFee :: CardanoTx -> V1.Value
getCardanoTxFee = onCardanoTx txFee (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoFee txFee)

getCardanoTxMint :: CardanoTx -> V1.Value
getCardanoTxMint = onCardanoTx txMint (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoMintValue txMintValue)

getCardanoTxValidityRange :: CardanoTx -> SlotRange
getCardanoTxValidityRange = onCardanoTx txValidRange
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoValidityRange txValidityRange)

getCardanoTxData :: CardanoTx -> Map V1.DatumHash V1.Datum
getCardanoTxData = onCardanoTx txData
    (\(SomeTx (C.Tx txBody _) _) -> fst $ CardanoAPI.scriptDataFromCardanoTxBody txBody)
    -- TODO: add txMetaData


-- TODO: To implement
-- getCardanoTxRedeemers :: CardanoTx -> Redeemers
-- getCardanoTxRedeemers = onCardanoTx txRedeemers (const Map.empty)
    -- (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
    --     Map.fromList $ mapMaybe (\(C.TxOut _ _ d _) -> fromCardanoTxOutDatum d) txOuts)
    -- where
    --     fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (DatumHash, Datum)
    --     fromCardanoTxOutDatum C.TxOutDatumNone = Nothing
    --     fromCardanoTxOutDatum (C.TxOutDatumHash _ _) = Nothing
    --     fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
    --         let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
    --     fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
    --         let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
getCardanoTxRedeemers :: CardanoTx -> Map V1.ScriptPurpose V1.Redeemer
getCardanoTxRedeemers = onCardanoTx txRedeemers (const Map.empty)

-- Defined here as uses `txId`.
instance Pretty Tx where
    pretty tx@(Tx _txInputs _txReferenceInputs _txCollateralInputs _txOutputs
                 _txReturnCollateral _txTotalCollateral _txMint _txFee
                 _txValidRange _txMintingScripts _txWithdrawals _txCertificates
                 _txSignatures _txScripts _txData _txMetadata) =
        let showNonEmpty empty x = [x | not empty]
            lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty _txInputs))
                , hang 2 (vsep ("reference inputs:" : fmap pretty _txReferenceInputs))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty _txCollateralInputs))
                , hang 2 (vsep ("outputs:" : fmap pretty _txOutputs))
                ]
                <> maybe [] (\out -> [hang 2 (vsep ["return collateral:", pretty out])]) _txReturnCollateral
                <> maybe [] (\val -> ["total collateral:" <+> pretty val]) _txTotalCollateral
                <> [ "mint:" <+> pretty _txMint
                , "fee:" <+> pretty _txFee
                , hang 2 (vsep ("mps:": fmap pretty (Map.assocs _txMintingScripts)))
                , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList _txSignatures)))
                , "validity range:" <+> viaShow _txValidRange
                ]
                <> (showNonEmpty (Map.null _txData) $ hang 2 (vsep ("data:": fmap pretty (Map.toList _txData))))
                <> (showNonEmpty (Map.null _txScripts) $ hang 2 (vsep ("attached scripts:": fmap pretty (fmap version <$> Map.toList _txScripts))))
                <> (showNonEmpty (null _txWithdrawals) $ hang 2 (vsep ("withdrawals:": fmap pretty _txWithdrawals)))
                <> (showNonEmpty (null _txCertificates) $ hang 2 (vsep ("certificates:": fmap pretty _txCertificates)))
                <> (["metadata: present" | isJust _txMetadata])
            txid = txId tx
        in nest 2 $ vsep ["Tx" <+> pretty txid <> colon, braces (vsep lines')]

-- | Compute the id of a transaction.
txId :: Tx -> V1.Tx.TxId
txId tx = TxId $ V1.toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: CardanoTx -> Map V1.Tx.TxOutRef TxOut -> Map V1.Tx.TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx) `Map.union` getCardanoTxProducedOutputs tx

-- | Update a map of unspent transaction outputs and signatures based
--   on the collateral inputs of a transaction (for when it is invalid).
updateUtxoCollateral :: CardanoTx -> Map V1.Tx.TxOutRef TxOut -> Map V1.Tx.TxOutRef TxOut
updateUtxoCollateral tx unspent =
    (unspent `Map.withoutKeys` (Set.fromList . map txInRef $ getCardanoTxCollateralInputs tx))
    `Map.union` getCardanoTxProducedReturnCollateral tx

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, V1.Tx.TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, V1.Tx.TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map V1.Tx.TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (V1.Tx.TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: V1.Value -> PaymentPubKey -> Maybe StakePubKey -> Either ToCardanoError TxOut
pubKeyTxOut v pk sk = do
  aie <- CardanoAPI.toCardanoAddressInEra (pNetworkId def) $ pubKeyAddress pk sk
  txov <- CardanoAPI.toCardanoValue v
  pure $ TxOut $ C.TxOut aie (C.TxOutValue C.MultiAssetInBabbageEra txov) C.TxOutDatumNone C.Api.ReferenceScriptNone

type PrivateKey = Crypto.XPrv

addCardanoTxSignature :: PrivateKey -> CardanoTx -> CardanoTx
addCardanoTxSignature privKey = cardanoTxMap (addSignature' privKey) addSignatureCardano
    where
        addSignatureCardano :: SomeCardanoApiTx -> SomeCardanoApiTx
        addSignatureCardano (CardanoApiEmulatorEraTx ctx)
            = CardanoApiEmulatorEraTx (addSignatureCardano' ctx)

        addSignatureCardano' (C.Api.ShelleyTx shelleyBasedEra (ValidatedTx body wits isValid aux))
            = C.Api.ShelleyTx shelleyBasedEra (ValidatedTx body wits' isValid aux)
          where
            wits' = wits <> mempty { txwitsVKey = newWits }
            newWits = case fromPaymentPrivateKey privKey body of
              C.Api.ShelleyKeyWitness _ wit -> Set.singleton wit
              _                             -> Set.empty

        fromPaymentPrivateKey :: PrivateKey -> TxBody EmulatorEra -> C.Api.KeyWitness C.Api.BabbageEra
        fromPaymentPrivateKey xprv txBody
          = C.Api.makeShelleyKeyWitness
              (C.Api.ShelleyTxBody C.Api.ShelleyBasedEraBabbage txBody notUsed notUsed notUsed notUsed)
              (C.Api.WitnessPaymentExtendedKey (C.Api.PaymentExtendedSigningKey xprv))
          where
            notUsed = undefined -- hack so we can reuse code from cardano-api

-- | Sign the transaction with a 'PrivateKey' and passphrase (ByteString) and add the signature to the
--   transaction's list of signatures.
addSignature :: PrivateKey -> Passphrase -> Tx -> Tx
addSignature privK passPhrase tx = tx & signatures . at pubK ?~ sig where
    sig = signTx (txId tx) privK passPhrase
    pubK = toPublicKey privK

-- | Sign the transaction with a 'PrivateKey' that has no passphrase and add the signature to the
--   transaction's list of signatures
addSignature' :: PrivateKey -> Tx -> Tx
addSignature' privK tx = tx & signatures . at pubK ?~ sig where
    sig = signTx' (txId tx) privK
    pubK = toPublicKey privK

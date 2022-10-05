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

module Ledger.Tx
    ( module Export
    -- * ChainIndexTxOut
    , ChainIndexTxOut(..)
    , toTxOut
    , toTxInfoTxOut
    -- ** Lenses and Prisms
    , ciTxOutAddress
    , ciTxOutValue
    , ciTxOutPublicKeyDatum
    , ciTxOutScriptDatum
    , ciTxOutReferenceScript
    , ciTxOutValidator
    , _PublicKeyChainIndexTxOut
    , _ScriptChainIndexTxOut
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
    -- * Transactions
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
import Control.Lens (At (at), makeLenses, makePrisms, (&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Proxy (Proxy))
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
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
import Plutus.Script.Utils.Scripts (datumHash, scriptHash)
import Plutus.V1.Ledger.Api qualified as V1
import Plutus.V1.Ledger.Tx qualified as V1.Tx hiding (TxIn (..), TxInType (..))
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Tx qualified as V2.Tx hiding (TxIn (..), TxInType (..))
import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))
-- for re-export
import Ledger.Tx.Internal as Export
import Plutus.V1.Ledger.Tx as Export hiding (TxIn (..), TxInType (..), TxOut (..), inRef, inScripts, inType, outAddress,
                                      outValue, pubKeyTxIn, pubKeyTxIns, scriptTxIn, scriptTxIns, txOutPubKey)

-- | Transaction output that comes from a chain index query.
--
-- It is defined here instead of the plutus-chain-index because plutus-ledger
-- uses that datatype, and plutus-ledger can't depend on plutus-chain-index
-- because of a cyclic dependency.
--
-- This datatype was created in order to be used in
-- 'Ledger.Constraints.processConstraint', specifically with the constraints
-- 'MustSpendPubKeyOutput' and 'MustSpendScriptOutput'.
data ChainIndexTxOut =
    PublicKeyChainIndexTxOut {
      -- | Address of the transaction output. The address is protected by a
      -- public key hash.
      _ciTxOutAddress         :: Address,
      -- | Value of the transaction output.
      _ciTxOutValue           :: V2.Value,
      -- | Optional datum (inline datum or datum in transaction body) attached to the transaction output.
      _ciTxOutPublicKeyDatum  :: Maybe (V2.DatumHash, Maybe V2.Datum),
      -- | Optional reference script attached to the transaction output.
      _ciTxOutReferenceScript :: Maybe (Versioned V1.Script)
    }
  | ScriptChainIndexTxOut {
      -- | Address of the transaction output. The address is protected by a
      -- script.
      _ciTxOutAddress         :: Address,
      -- | Value of the transaction output.
      _ciTxOutValue           :: V1.Value,
      -- | Datum attached to the transaction output, either in full (inline datum or datum in transaction body) or as a
      -- hash reference. A transaction output protected by a Plutus script
      -- is guardateed to have an associated datum.
      _ciTxOutScriptDatum     :: (V2.DatumHash, Maybe V2.Datum),
      -- | Optional reference script attached to the transaction output.
      -- The reference script is, in genereal, unrelated to the validator
      -- script althought it could also be the same.
      _ciTxOutReferenceScript :: Maybe (Versioned V1.Script),
      -- | Validator protecting the transaction output, either in full or
      -- as a hash reference.
      _ciTxOutValidator       :: (V1.ValidatorHash, Maybe (Versioned V1.Validator))
    }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, NFData, OpenApi.ToSchema)

makeLenses ''ChainIndexTxOut
makePrisms ''ChainIndexTxOut

toTxOut :: C.NetworkId -> ChainIndexTxOut -> Either ToCardanoError TxOut
toTxOut networkId (PublicKeyChainIndexTxOut addr v datum referenceScript) =
  TxOut <$> (C.TxOut
    <$> CardanoAPI.toCardanoAddressInEra networkId addr
    <*> CardanoAPI.toCardanoTxOutValue v
    <*> maybe (pure CardanoAPI.toCardanoTxOutNoDatum) (CardanoAPI.toCardanoTxOutDatumHash . fst) datum
    <*> CardanoAPI.toCardanoReferenceScript referenceScript)
toTxOut networkId (ScriptChainIndexTxOut addr v (dh, _) referenceScript _validator) =
  TxOut <$> (C.TxOut
    <$> CardanoAPI.toCardanoAddressInEra networkId addr
    <*> CardanoAPI.toCardanoTxOutValue v
    <*> CardanoAPI.toCardanoTxOutDatumHash dh
    <*> CardanoAPI.toCardanoReferenceScript referenceScript)

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that 'ChainIndexTxOut' supports features such inline datums and
-- reference scripts which are not supported by V1 TxOut. Converting from
-- 'ChainIndexTxOut' to 'TxOut' and back is therefore lossy.
toTxInfoTxOut :: ChainIndexTxOut -> V2.Tx.TxOut
toTxInfoTxOut (PublicKeyChainIndexTxOut addr v datum referenceScript) =
    V2.Tx.TxOut addr v (toPlutusOutDatum datum) (scriptHash <$> referenceScript)
toTxInfoTxOut (ScriptChainIndexTxOut addr v datum referenceScript _validator) =
    V2.Tx.TxOut addr v (toPlutusOutDatum $ Just datum) (scriptHash <$> referenceScript)

toPlutusOutDatum :: Maybe (V2.DatumHash, Maybe V2.Datum) -> V2.Tx.OutputDatum
toPlutusOutDatum Nothing       = V2.Tx.NoOutputDatum
toPlutusOutDatum (Just (d, _)) = V2.Tx.OutputDatumHash d

instance Pretty ChainIndexTxOut where
    pretty PublicKeyChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]
    pretty ScriptChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]

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
                    [ hang 2 (vsep ("mps:": fmap pretty (Map.toList (txMintingScripts tx'))))
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
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        Map.fromList $ mapMaybe (\(C.TxOut _ _ d _) -> fromCardanoTxOutDatum d) txOuts)
    where
        fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (V1.DatumHash, V1.Datum)
        fromCardanoTxOutDatum C.TxOutDatumNone = Nothing
        fromCardanoTxOutDatum (C.TxOutDatumHash _ _) = Nothing
        fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
            let d' = V1.Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
        fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
            let d' = V1.Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
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
                <> (showNonEmpty (Map.null _txScripts) $ hang 2 (vsep ("attached scripts:": fmap pretty (Map.keys _txScripts))))
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

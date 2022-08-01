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
    ( module Ledger.Tx.Internal
    , module Plutus.V1.Ledger.Tx
    -- * ChainIndexTxOut
    , ChainIndexTxOut(..)
    , toTxOut
    , fromTxOut
    -- ** Lenses and Prisms
    , ciTxOutAddress
    , ciTxOutValue
    , ciTxOutPublicKeyDatum
    , ciTxOutScriptDatum
    , ciTxOutReferenceScript
    , ciTxOutValidator
    , _PublicKeyChainIndexTxOut
    , _ScriptChainIndexTxOut
    , TxOut(..)
    , CardanoTx(..)
    , cardanoApiTx
    , emulatorTx
    , onCardanoTx
    , mergeCardanoTxWith
    , cardanoTxMap
    , getCardanoTxId
    , getCardanoTxInputs
    , getCardanoTxCollateralInputs
    , getCardanoTxOutRefs
    , getCardanoTxOutputs
    , getCardanoTxSpentOutputs
    , getCardanoTxUnspentOutputsTx
    , getCardanoTxFee
    , getCardanoTxMint
    , getCardanoTxValidityRange
    , getCardanoTxData
    , addCardanoTxSignature
    , SomeCardanoApiTx(.., CardanoApiEmulatorEraTx)
    , EmulatorEraCardanoApiTx(..)
    , ToCardanoError(..)
    -- * Transactions
    , addSignature
    , addSignature'
    , pubKeyTxOut
    , updateUtxo
    , txOutRefs
    , unspentOutputsTx
    -- * Hashing transactions
    , txId
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise (decode, encode))
import Control.Lens (At (at), makeLenses, makePrisms, (&), (?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Ledger.Address (Address, PaymentPubKey, StakePubKey, pubKeyAddress)
import Ledger.Crypto (Passphrase, signTx, signTx', toPublicKey)
import Ledger.Orphans ()
import Ledger.Params (Params (pNetworkId))
import Ledger.Slot (SlotRange)
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Internal
import Ledger.Validation qualified
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Api qualified as V1
import Plutus.V1.Ledger.Tx hiding (TxIn, TxInType (..), TxOut, inRef, inScripts, inType, pubKeyTxIn, pubKeyTxIns,
                            scriptTxIn, scriptTxIns, txInRef, txInType, txOutAddress, txOutDatum, txOutDatumHash,
                            txOutPubKey, txOutValue)
import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))

type PrivateKey = Crypto.XPrv

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
      _ciTxOutValue           :: V1.Value,
      -- | Optional datum attached to the transaction output.
      _ciTxOutPublicKeyDatum  :: Maybe (V1.DatumHash, Maybe V1.Datum),
      -- | Optional reference script attached to the transaction output.
      _ciTxOutReferenceScript :: Maybe V1.Script
    }
  | ScriptChainIndexTxOut {
      -- | Address of the transaction output. The address is protected by a
      -- script.
      _ciTxOutAddress         :: Address,
      -- | Value of the transaction output.
      _ciTxOutValue           :: V1.Value,
      -- | Datum attached to the transaction output, either in full or as a
      -- hash reference. A transaction output protected by a Plutus script
      -- is guardateed to have an associated datum.
      _ciTxOutScriptDatum     :: (V1.DatumHash, Maybe V1.Datum),
      -- | Optional reference script attached to the transaction output.
      -- The reference script is, in genereal, unrelated to the validator
      -- script althought it could also be the same.
      _ciTxOutReferenceScript :: Maybe V1.Script,
      -- | Validator protecting the transaction output, either in full or
      -- as a hash reference.
      _ciTxOutValidator       :: (V1.ValidatorHash, Maybe V1.Validator)
    }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

makeLenses ''ChainIndexTxOut
makePrisms ''ChainIndexTxOut

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that 'ChainIndexTxOut' supports features such inline datums and
-- reference scripts which are not supported by V1 TxOut. Converting from
-- 'ChainIndexTxOut' to 'TxOut' and back is therefore lossy.
toTxOut :: ChainIndexTxOut -> V1.TxOut
toTxOut (PublicKeyChainIndexTxOut addr v datum _referenceScript) =
  V1.TxOut addr v (fst <$> datum)
toTxOut (ScriptChainIndexTxOut addr v (dh, _) _referenceScript _validator)     =
  V1.TxOut addr v (Just dh)

-- | Converts a plutus-ledger-api transaction output to the chain index
-- transaction output.
fromTxOut :: V1.TxOut -> Maybe ChainIndexTxOut
fromTxOut V1.TxOut { txOutAddress=a, txOutValue=v, txOutDatumHash=mdh } =
  case V1.addressCredential a of
    V1.PubKeyCredential _ ->
      -- V1 transactions don't support inline datums and reference scripts
      pure $ PublicKeyChainIndexTxOut a v ((, Nothing) <$> mdh) Nothing
    V1.ScriptCredential vh ->
      mdh >>= \dh ->
        pure $ ScriptChainIndexTxOut a v (dh, Nothing) Nothing (vh, Nothing)

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

newtype EmulatorEraCardanoApiTx = EmulatorEraCardanoApiTx (C.Tx C.BabbageEra)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance Serialise EmulatorEraCardanoApiTx where
  encode (EmulatorEraCardanoApiTx tx) = Encoding (TkBytes (C.serialiseToCBOR tx))
  decode = do
    bytes <- decodeBytes
    either
      (\l -> fail $ "Failed to decode Cardano.Api.Tx (" ++ show l ++ ")")
      (\r -> pure $ EmulatorEraCardanoApiTx r)
      $ C.deserialiseFromCBOR (C.AsTx C.AsBabbageEra) bytes

instance FromJSON EmulatorEraCardanoApiTx where
  parseJSON = withObject "EmulatorEraCardanoApiTx" $ \o -> do
    envelope <- o .: "tx"
    let eTx = C.deserialiseFromTextEnvelope (C.AsTx C.AsBabbageEra) envelope
    either
      (\l -> fail $ "Failed to parse BabbageEra 'tx' field from EmulatorEraCardanoApiTx (" ++ show l ++ ")")
      (\r -> pure $ EmulatorEraCardanoApiTx r)
      eTx

instance ToJSON EmulatorEraCardanoApiTx where
  toJSON (EmulatorEraCardanoApiTx tx) =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx ]

data CardanoTx
    = EmulatorTx { _emulatorTx :: Tx }
    | CardanoApiTx { _cardanoApiTx :: EmulatorEraCardanoApiTx }
    | Both { _emulatorTx :: Tx, _cardanoApiTx :: EmulatorEraCardanoApiTx }
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
                , "mint:" <+> pretty (getCardanoTxMint tx)
                , "fee:" <+> pretty (getCardanoTxFee tx)
                ] ++ onCardanoTx (\tx' ->
                    [ hang 2 (vsep ("mps:": fmap pretty (Map.toList (txMintScripts tx'))))
                    , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList (txSignatures tx'))))
                    ]) (const []) tx ++
                [ "validity range:" <+> viaShow (getCardanoTxValidityRange tx)
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList $ getCardanoTxRedeemers tx) ))
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList (getCardanoTxData tx))))
                ]
        in nest 2 $ vsep ["Tx" <+> pretty (getCardanoTxId tx) <> colon, braces (vsep lines')]

onCardanoTx :: (Tx -> r) -> (EmulatorEraCardanoApiTx -> r) -> CardanoTx -> r
onCardanoTx l r = mergeCardanoTxWith l r const

mergeCardanoTxWith :: (Tx -> a) -> (EmulatorEraCardanoApiTx -> a) -> (a -> a -> a) -> CardanoTx -> a
mergeCardanoTxWith l _ _ (EmulatorTx tx)    = l tx
mergeCardanoTxWith l r m (Both tx ctx)      = m (l tx) (r ctx)
mergeCardanoTxWith _ r _ (CardanoApiTx ctx) = r ctx

cardanoTxMap :: (Tx -> Tx) -> (EmulatorEraCardanoApiTx -> EmulatorEraCardanoApiTx) -> CardanoTx -> CardanoTx
cardanoTxMap l _ (EmulatorTx tx)    = EmulatorTx (l tx)
cardanoTxMap l r (Both tx ctx)      = Both (l tx) (r ctx)
cardanoTxMap _ r (CardanoApiTx ctx) = CardanoApiTx (r ctx)

getCardanoTxId :: CardanoTx -> TxId
getCardanoTxId = onCardanoTx txId
  (\(EmulatorEraCardanoApiTx (C.Tx body _ )) -> CardanoAPI.fromCardanoTxId $ C.getTxId body)

getCardanoTxInputs :: CardanoTx -> [TxIn]
getCardanoTxInputs = onCardanoTx txInputs
    (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) ->
        fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns)

getCardanoTxCollateralInputs :: CardanoTx -> [TxIn]
getCardanoTxCollateralInputs = onCardanoTx txCollateral
    (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) ->
        fromCardanoTxInsCollateral txInsCollateral)

getCardanoTxReferenceInputs :: CardanoTx -> [TxIn]
getCardanoTxReferenceInputs = onCardanoTx txReferenceInputs
    (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) ->
        txInsReferenceToPlutusTxIns txInsReference)
 where
     txInsReferenceToPlutusTxIns C.TxInsReferenceNone = []
     txInsReferenceToPlutusTxIns (C.TxInsReference _ txIns) =
         fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn) txIns

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, TxOutRef)]
getCardanoTxOutRefs = onCardanoTx txOutRefs
  (\(EmulatorEraCardanoApiTx (C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _)) ->
    let mkOut (i, o) = (o, TxOutRef (CardanoAPI.fromCardanoTxId $ C.getTxId txBody) i)
    in mkOut <$> zip [0..] (map TxOut txOuts))

getCardanoTxOutputs :: CardanoTx -> [TxOut]
getCardanoTxOutputs = fmap fst . getCardanoTxOutRefs

getCardanoTxUnspentOutputsTx :: CardanoTx -> Map TxOutRef TxOut
getCardanoTxUnspentOutputsTx = Map.fromList . map swap . getCardanoTxOutRefs

getCardanoTxSpentOutputs :: CardanoTx -> Set TxOutRef
getCardanoTxSpentOutputs = Set.fromList . map txInRef . getCardanoTxInputs

getCardanoTxFee :: CardanoTx -> V1.Value
getCardanoTxFee = onCardanoTx txFee
  (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) -> CardanoAPI.fromCardanoFee txFee)

getCardanoTxMint :: CardanoTx -> V1.Value
getCardanoTxMint = onCardanoTx txMint
  (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) -> CardanoAPI.fromCardanoMintValue txMintValue)

getCardanoTxValidityRange :: CardanoTx -> SlotRange
getCardanoTxValidityRange = onCardanoTx txValidRange
    (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) -> CardanoAPI.fromCardanoValidityRange txValidityRange)

getCardanoTxData :: CardanoTx -> Map V1.DatumHash V1.Datum
getCardanoTxData = onCardanoTx txData
    (\(EmulatorEraCardanoApiTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) ->
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

getCardanoTxRedeemers :: CardanoTx -> Redeemers
getCardanoTxRedeemers = onCardanoTx txRedeemers (const Map.empty) -- TODO: To implement

instance Pretty Tx where
    pretty tx =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (txInputs tx)))
                , hang 2 (vsep ("reference inputs:" : fmap pretty (txReferenceInputs tx)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (txCollateral tx)))
                , hang 2 (vsep ("outputs:" : fmap pretty (txOutputs tx)))
                , "mint:" <+> pretty (txMint tx)
                , "fee:" <+> pretty (txFee tx)
                , hang 2 (vsep ("mps:": fmap pretty (Map.toList $ txMintScripts tx)))
                , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList $ txSignatures tx)))
                , "validity range:" <+> viaShow (txValidRange tx)
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList $ txRedeemers tx) ))
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList $ txData tx) ))
                ]
            txid = txId tx
        in nest 2 $ vsep ["Tx" <+> pretty txid <> colon, braces (vsep lines')]

-- | Compute the id of a transaction.
txId :: Tx -> TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx = TxId $ V1.toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: CardanoTx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx) `Map.union` getCardanoTxUnspentOutputsTx tx

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx = Map.fromList . map swap . txOutRefs

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: V1.Value -> PaymentPubKey -> Maybe StakePubKey -> Either ToCardanoError TxOut
pubKeyTxOut v pk sk = do
  aie <- CardanoAPI.toCardanoAddressInEra (pNetworkId def) $ pubKeyAddress pk sk
  txov <- CardanoAPI.toCardanoValue v
  pure $ TxOut $ C.TxOut aie (C.TxOutValue C.MultiAssetInBabbageEra txov) C.TxOutDatumNone C.ReferenceScriptNone

addCardanoTxSignature :: PrivateKey -> CardanoTx -> CardanoTx
addCardanoTxSignature privKey = cardanoTxMap (addSignature' privKey) addSignatureCardano
    where
        addSignatureCardano :: EmulatorEraCardanoApiTx -> EmulatorEraCardanoApiTx
        addSignatureCardano (EmulatorEraCardanoApiTx ctx)
            = EmulatorEraCardanoApiTx (Ledger.Validation.addSignature privKey ctx)

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

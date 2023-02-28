{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
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
    , cardanoTx
    , getCardanoTxId
    , getCardanoTxInputs
    , getCardanoTxCollateralInputs
    , getCardanoTxOutRefs
    , getCardanoTxOutputs
    , getCardanoTxRedeemers
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
    , addCardanoTxSignature
    , pubKeyTxOut
    , updateUtxo
    , updateUtxoCollateral
    -- * TxBodyContent functions
    , getTxBodyContentInputs
    , getTxBodyContentCollateralInputs
    , getTxBodyContentReturnCollateral
    , getTxBodyContentMint
    , txBodyContentIns
    , txBodyContentCollateralIns
    , txBodyContentOuts
    -- * Utility
    , decoratedTxOutPlutusValue
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxWitness (txwitsVKey)
import Codec.Serialise (Serialise)

import Control.Lens (Getter, Lens', Traversal', lens, makeLenses, makePrisms, to, view, views, (^.), (^?))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)

import Ledger.Address (Address, CardanoAddress, PaymentPubKey, cardanoAddressCredential, cardanoStakingCredential,
                       pubKeyAddress)
import Ledger.Orphans ()
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
import Plutus.V1.Ledger.Value (Value)

-- | A datum in a transaction output that comes from a chain index query.
data DatumFromQuery
    = DatumUnknown
    | DatumInline V2.Datum
    | DatumInBody V2.Datum
    deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON)

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
      _decoratedTxOutValue             :: C.Value,
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
      _decoratedTxOutValue             :: C.Value,
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
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON)

makeLenses ''DecoratedTxOut
makePrisms ''DecoratedTxOut


mkDecoratedTxOut
    :: CardanoAddress -> C.Value -> (V2.DatumHash, DatumFromQuery) -> Maybe (Versioned V1.Script)
    -> DecoratedTxOut
mkDecoratedTxOut a v dat rs = let
  sc = cardanoStakingCredential a
  in case cardanoAddressCredential a of
  (V2.PubKeyCredential c) -> PublicKeyDecoratedTxOut c sc v (Just dat) rs
  (V2.ScriptCredential c) -> ScriptDecoratedTxOut c sc v dat rs Nothing

mkPubkeyDecoratedTxOut
    :: CardanoAddress -> C.Value -> Maybe (V2.DatumHash, DatumFromQuery) -> Maybe (Versioned V1.Script)
    -> Maybe DecoratedTxOut
mkPubkeyDecoratedTxOut a v dat rs = let
  sc = cardanoStakingCredential a
  in case cardanoAddressCredential a of
  (V2.PubKeyCredential c) -> Just $ PublicKeyDecoratedTxOut c sc v dat rs
  _                       -> Nothing

mkScriptDecoratedTxOut
    :: CardanoAddress
    -> C.Value
    -> (V2.DatumHash, DatumFromQuery)
    -> Maybe (Versioned V1.Script)
    -> Maybe (Versioned V1.Validator)
    -> Maybe DecoratedTxOut
mkScriptDecoratedTxOut a v dat rs val = let
  sc = cardanoStakingCredential a
  in case cardanoAddressCredential a of
  (V2.ScriptCredential c) -> pure $ ScriptDecoratedTxOut c sc v dat rs val
  _                       -> Nothing

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
    <*> pure (CardanoAPI.toCardanoTxOutValue (p ^. decoratedTxOutValue))
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
    V2.Tx.TxOut (p ^. decoratedTxOutAddress) (CardanoAPI.fromCardanoValue $ p ^. decoratedTxOutValue)
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

newtype CardanoTx = CardanoTx { _cardanoTx :: SomeCardanoApiTx }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise)

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
        let
          renderScriptWitnesses (_cardanoTx -> CardanoApiEmulatorEraTx (C.Api.Tx (C.Api.ShelleyTxBody _ _ scripts _ _ _) _)) =
                [ hang 2 (vsep ("attached scripts:": fmap viaShow scripts)) | not (null scripts) ]
          lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (getCardanoTxInputs tx)))
                , hang 2 (vsep ("reference inputs:" : fmap pretty (getCardanoTxReferenceInputs tx)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (getCardanoTxCollateralInputs tx)))
                , hang 2 (vsep ("outputs:" : fmap pretty (getCardanoTxOutputs tx)))
                ]
                <> maybe [] (\out -> [hang 2 (vsep ["return collateral:", pretty out])]) (getCardanoTxReturnCollateral tx)
                <> maybe [] (\val -> ["total collateral:" <+> pretty val]) (getCardanoTxTotalCollateral tx)
                ++ [ "mint:" <+> pretty (getCardanoTxMint tx)
                , "fee:" <+> pretty (getCardanoTxFee tx)
                , "validity range:" <+> viaShow (getCardanoTxValidityRange tx)
                , hang 2 (vsep ("data:": fmap pretty (Map.toList (getCardanoTxData tx))))
                , hang 2 (vsep ("redeemers:": fmap (\(k, V2.Redeemer red) -> viaShow k <+> ":" <+> viaShow red) (Map.toList $ getCardanoTxRedeemers tx)))
                ] ++ renderScriptWitnesses tx
        in nest 2 $ vsep ["Tx" <+> pretty (getCardanoTxId tx) <> colon, braces (vsep lines')]

instance Pretty SomeCardanoApiTx where
  pretty = pretty . CardanoTx

instance Pretty CardanoAPI.CardanoBuildTx where
  pretty txBodyContent = case C.makeSignedTransaction [] <$> CardanoAPI.makeTransactionBody Nothing mempty txBodyContent of
    Right tx -> pretty $ CardanoApiEmulatorEraTx tx
    _        -> viaShow txBodyContent

getTxBodyContent :: SomeCardanoApiTx -> C.TxBodyContent C.ViewTx C.BabbageEra
getTxBodyContent (CardanoApiEmulatorEraTx (C.Tx (C.TxBody bodyContent) _)) = bodyContent

getCardanoTxId :: CardanoTx -> V1.Tx.TxId
getCardanoTxId = getCardanoApiTxId . _cardanoTx

getCardanoApiTxId :: SomeCardanoApiTx -> V1.Tx.TxId
getCardanoApiTxId (SomeTx (C.Tx body _) _) = CardanoAPI.fromCardanoTxId $ C.getTxId body

getCardanoTxInputs :: CardanoTx -> [TxIn]
getCardanoTxInputs = getTxBodyContentInputs . getTxBodyContent . _cardanoTx

getTxBodyContentInputs :: C.TxBodyContent ctx era -> [TxIn]
getTxBodyContentInputs C.TxBodyContent {..} =
    fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns

getCardanoTxCollateralInputs :: CardanoTx -> [TxIn]
getCardanoTxCollateralInputs = getTxBodyContentCollateralInputs . getTxBodyContent . _cardanoTx

getTxBodyContentCollateralInputs :: C.TxBodyContent ctx era -> [TxIn]
getTxBodyContentCollateralInputs C.TxBodyContent {..} = CardanoAPI.fromCardanoTxInsCollateral txInsCollateral

getCardanoTxReferenceInputs :: CardanoTx -> [TxIn]
getCardanoTxReferenceInputs (CardanoTx (SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _)) =
    txInsReferenceToPlutusTxIns txInsReference
  where
    txInsReferenceToPlutusTxIns C.TxInsReferenceNone = []
    txInsReferenceToPlutusTxIns (C.TxInsReference _ txIns') =
      fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn) txIns'

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, V1.Tx.TxOutRef)]
getCardanoTxOutRefs = cardanoApiTxOutRefs . _cardanoTx
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
getCardanoTxReturnCollateral = getTxBodyContentReturnCollateral . getTxBodyContent . _cardanoTx

getTxBodyContentReturnCollateral :: C.TxBodyContent ctx C.Api.BabbageEra -> Maybe TxOut
getTxBodyContentReturnCollateral C.TxBodyContent {..} =
    case txReturnCollateral of
        C.TxReturnCollateralNone     -> Nothing
        C.TxReturnCollateral _ txOut -> Just $ TxOut txOut

getCardanoTxProducedReturnCollateral :: CardanoTx -> Map V1.Tx.TxOutRef TxOut
getCardanoTxProducedReturnCollateral tx = maybe Map.empty (Map.singleton (V1.TxOutRef (getCardanoTxId tx) 0)) $
    getCardanoTxReturnCollateral tx

getCardanoTxTotalCollateral :: CardanoTx -> Maybe C.Lovelace
getCardanoTxTotalCollateral (_cardanoTx -> CardanoApiEmulatorEraTx (C.Tx (C.TxBody C.TxBodyContent {..}) _)) =
  CardanoAPI.fromCardanoTotalCollateral txTotalCollateral

getCardanoTxFee :: CardanoTx -> C.Lovelace
getCardanoTxFee (CardanoTx (SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _)) = CardanoAPI.fromCardanoFee txFee

getCardanoTxMint :: CardanoTx -> C.Value
getCardanoTxMint = getTxBodyContentMint . getTxBodyContent . _cardanoTx

getTxBodyContentMint :: C.TxBodyContent ctx era -> C.Value
getTxBodyContentMint C.TxBodyContent {..} = CardanoAPI.fromCardanoMintValue txMintValue

getCardanoTxValidityRange :: CardanoTx -> SlotRange
getCardanoTxValidityRange (CardanoTx (SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _)) = CardanoAPI.fromCardanoValidityRange txValidityRange

getCardanoTxData :: CardanoTx -> Map V1.DatumHash V1.Datum
getCardanoTxData (CardanoTx (SomeTx (C.Tx txBody _) _)) = fst $ CardanoAPI.scriptDataFromCardanoTxBody txBody
    -- TODO: add txMetaData

txBodyContentIns :: Lens' (C.TxBodyContent C.BuildTx C.BabbageEra) [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
txBodyContentIns = lens C.txIns (\bodyContent ins -> bodyContent { C.txIns = ins })

txBodyContentCollateralIns :: Lens' (C.TxBodyContent C.BuildTx C.BabbageEra) [C.TxIn]
txBodyContentCollateralIns = lens
    (\bodyContent -> case C.txInsCollateral bodyContent of C.TxInsCollateralNone -> []; C.TxInsCollateral _ txIns -> txIns)
    (\bodyContent ins -> bodyContent { C.txInsCollateral = case ins of [] -> C.TxInsCollateralNone; _ -> C.TxInsCollateral C.CollateralInBabbageEra ins })

txBodyContentOuts :: Lens' (C.TxBodyContent ctx C.BabbageEra) [TxOut]
txBodyContentOuts = lens (map TxOut . C.txOuts) (\bodyContent outs -> bodyContent { C.txOuts = map getTxOut outs })

getCardanoTxRedeemers :: CardanoTx -> V2.Tx.Redeemers
getCardanoTxRedeemers (CardanoTx (SomeTx (C.Tx txBody _) _)) = snd $ CardanoAPI.scriptDataFromCardanoTxBody txBody

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

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: C.Value -> PaymentPubKey -> Maybe V1.StakingCredential -> Either ToCardanoError TxOut
pubKeyTxOut v pk sk = do
  aie <- CardanoAPI.toCardanoAddressInEra (C.Testnet $ C.NetworkMagic 1) $ pubKeyAddress pk sk
  pure $ TxOut $ C.TxOut aie (CardanoAPI.toCardanoTxOutValue v) C.TxOutDatumNone C.Api.ReferenceScriptNone

type PrivateKey = Crypto.XPrv

addCardanoTxSignature :: PrivateKey -> CardanoTx -> CardanoTx
addCardanoTxSignature privKey = CardanoTx . addSignatureCardano . _cardanoTx
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

        fromPaymentPrivateKey xprv txBody
          = C.Api.makeShelleyKeyWitness
              (C.Api.ShelleyTxBody C.Api.ShelleyBasedEraBabbage txBody notUsed notUsed notUsed notUsed)
              (C.Api.WitnessPaymentExtendedKey (C.Api.PaymentExtendedSigningKey xprv))
          where
            notUsed = undefined -- hack so we can reuse code from cardano-api

decoratedTxOutPlutusValue :: DecoratedTxOut -> Value
decoratedTxOutPlutusValue = CardanoAPI.fromCardanoValue . view decoratedTxOutValue

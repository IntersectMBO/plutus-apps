{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Ledger.Tx
    ( module Ledger.Tx.Internal
    , module Plutus.V2.Ledger.Tx
    , module Plutus.Script.Utils.V2.Scripts
    -- * ChainIndexTxOut
    , ChainIndexTxOut(..)
    , toTxOut
    -- , fromTxOut FIXME we need to figure this out
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
    , CardanoAPI.SomeCardanoApiTx(.., CardanoApiEmulatorEraTx)
    , CardanoAPI.ToCardanoError(..)
    -- * Transactions
    , addSignature
    , addSignature'
    , pubKeyTxOut
    , updateUtxo
    , updateUtxoCollateral
    , txOutRefs
    , unspentOutputsTx
    , getOutputDatumHash
    , getOutputDatumOrHash
    -- * Hashing transactions
    , txId
    ) where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise (encode))
import Control.Lens (At (at), makeLenses, makePrisms, (&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Address (PaymentPubKey, StakePubKey, pubKeyAddress)
import Ledger.Crypto (Passphrase, signTx, signTx', toPublicKey)
import Ledger.Orphans ()
import Ledger.Slot (SlotRange)
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Internal hiding (updateUtxoCollateral)
import Plutus.Script.Utils.V2.Scripts
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Tx
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
      _ciTxOutValue           :: Value,
      -- | Optional datum attached to the transaction output.
      _ciTxOutPublicKeyDatum  :: OutputDatum,
      -- | Optional reference script attached to the transaction output.
      _ciTxOutReferenceScript :: Maybe Script
    }
  | ScriptChainIndexTxOut {
      -- | Address of the transaction output. The address is protected by a
      -- script.
      _ciTxOutAddress         :: Address,
      -- | Value of the transaction output.
      _ciTxOutValue           :: Value,
      -- | Datum attached to the transaction output, either in full or as a
      -- hash reference. A transaction output protected by a Plutus script
      -- is guardateed to have an associated datum.
      _ciTxOutScriptDatum     :: Either DatumHash Datum,
      -- | Optional reference script attached to the transaction output.
      -- The reference script is, in genereal, unrelated to the validator
      -- script althought it could also be the same.
      _ciTxOutReferenceScript :: Maybe Script,
      -- | Validator protecting the transaction output, either in full or
      -- as a hash reference.
      _ciTxOutValidator       :: Either ValidatorHash Validator
    }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

makeLenses ''ChainIndexTxOut
makePrisms ''ChainIndexTxOut

toTxOut :: ChainIndexTxOut -> TxOut
toTxOut (PublicKeyChainIndexTxOut addr v outputDatum referenceScript) =
  TxOut addr v outputDatum (fmap scriptHash referenceScript)
toTxOut (ScriptChainIndexTxOut addr v (Left dh) referenceScript _validator) =
  TxOut addr v (OutputDatumHash dh) (fmap scriptHash referenceScript)
toTxOut (ScriptChainIndexTxOut addr v (Right d) referenceScript _validator) =
  TxOut addr v (OutputDatum d) (fmap scriptHash referenceScript)

-- fromTxOut :: TxOut -> Maybe ChainIndexTxOut
-- fromTxOut TxOut { txOutAddress, txOutValue, txOutDatum, txOutReferenceScript } =
--   case addressCredential txOutAddress of
--     PubKeyCredential _ ->
--       pure $ PublicKeyChainIndexTxOut txOutAddress txOutValue txOutDatum refScript
--     ScriptCredential vh -> do
--       doh <- getOutputDatumHash txOutDatum
--       pure $ ScriptChainIndexTxOut txOutAddress txOutValue doh refScript (Left vh)
--   where
--     refScript = _ txOutReferenceScript

instance Pretty ChainIndexTxOut where
    pretty PublicKeyChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]
    pretty ScriptChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]

{- Note [Why we have the Both constructor in CardanoTx]

We want to do validation with both the emulator and with the cardano-ledger library, at least as long
as we don't have Phase2 validation errors via the cardano-ledger library.

To do that we need the required signers which are only available in UnbalancedTx during balancing.
So during balancing we can create the CardanoAPI.SomeCardanoApiTx, while proper validation can only happen in
Wallet.Emulator.Chain.validateBlock, since that's when we know the right Slot number. This means that
we need both transaction types in the path from balancing to validateBlock. -}

data CardanoTx
    = EmulatorTx { _emulatorTx :: Tx }
    | CardanoApiTx { _cardanoApiTx :: CardanoAPI.SomeCardanoApiTx }
    | Both { _emulatorTx :: Tx, _cardanoApiTx :: CardanoAPI.SomeCardanoApiTx }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema, Serialise)

makeLenses ''CardanoTx

getEmulatorEraTx :: CardanoAPI.SomeCardanoApiTx -> C.Tx C.BabbageEra
getEmulatorEraTx (CardanoAPI.SomeTx tx C.BabbageEraInCardanoMode) = tx
getEmulatorEraTx _                                                = error "getEmulatorEraTx: Expected a Babbage tx"

pattern CardanoApiEmulatorEraTx :: C.Tx C.BabbageEra -> CardanoAPI.SomeCardanoApiTx
pattern CardanoApiEmulatorEraTx tx <- (getEmulatorEraTx -> tx) where
    CardanoApiEmulatorEraTx tx = CardanoAPI.SomeTx tx C.BabbageEraInCardanoMode

{-# COMPLETE CardanoApiEmulatorEraTx #-}

instance Pretty CardanoTx where
    pretty tx =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList (getCardanoTxInputs tx))))
                , hang 2 (vsep ("reference inputs:" : fmap pretty (Set.toList (getCardanoTxReferenceInputs tx))))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (Set.toList (getCardanoTxCollateralInputs tx))))
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

onCardanoTx :: (Tx -> r) -> (CardanoAPI.SomeCardanoApiTx -> r) -> CardanoTx -> r
onCardanoTx l r = mergeCardanoTxWith l r const

mergeCardanoTxWith :: (Tx -> a) -> (CardanoAPI.SomeCardanoApiTx -> a) -> (a -> a -> a) -> CardanoTx -> a
mergeCardanoTxWith l _ _ (EmulatorTx tx)    = l tx
mergeCardanoTxWith l r m (Both tx ctx)      = m (l tx) (r ctx)
mergeCardanoTxWith _ r _ (CardanoApiTx ctx) = r ctx

cardanoTxMap :: (Tx -> Tx) -> (CardanoAPI.SomeCardanoApiTx -> CardanoAPI.SomeCardanoApiTx) -> CardanoTx -> CardanoTx
cardanoTxMap l _ (EmulatorTx tx)    = EmulatorTx (l tx)
cardanoTxMap l r (Both tx ctx)      = Both (l tx) (r ctx)
cardanoTxMap _ r (CardanoApiTx ctx) = CardanoApiTx (r ctx)

getCardanoTxId :: CardanoTx -> TxId
getCardanoTxId = onCardanoTx txId getCardanoApiTxId

getCardanoApiTxId :: CardanoAPI.SomeCardanoApiTx -> TxId
getCardanoApiTxId (CardanoAPI.SomeTx (C.Tx body _) _) = CardanoAPI.fromCardanoTxId $ C.getTxId body

getCardanoTxInputs :: CardanoTx -> Set CardanoAPI.TxIn
getCardanoTxInputs = onCardanoTx txInputs
    (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        Set.fromList $ fmap ((`CardanoAPI.TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns)

getCardanoTxCollateralInputs :: CardanoTx -> Set CardanoAPI.TxIn
getCardanoTxCollateralInputs = onCardanoTx txCollateral
    (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        CardanoAPI.fromCardanoTxInsCollateral txInsCollateral)

getCardanoTxReferenceInputs :: CardanoTx -> Set CardanoAPI.TxIn
getCardanoTxReferenceInputs = onCardanoTx txReferenceInputs
    (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        txInsReferenceToPlutusTxIns txInsReference)
 where
     txInsReferenceToPlutusTxIns C.TxInsReferenceNone = Set.empty
     txInsReferenceToPlutusTxIns (C.TxInsReference _ txIns) =
         Set.fromList $ fmap ((`CardanoAPI.TxIn` Nothing) . CardanoAPI.fromCardanoTxIn) txIns

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, TxOutRef)]
getCardanoTxOutRefs = onCardanoTx txOutRefs CardanoAPI.txOutRefs

getCardanoTxOutputs :: CardanoTx -> [TxOut]
getCardanoTxOutputs = fmap fst . getCardanoTxOutRefs

getCardanoTxUnspentOutputsTx :: CardanoTx -> Map.Map TxOutRef TxOut
getCardanoTxUnspentOutputsTx = onCardanoTx unspentOutputsTx CardanoAPI.unspentOutputsTx

getCardanoTxSpentOutputs :: CardanoTx -> Set TxOutRef
getCardanoTxSpentOutputs = Set.map CardanoAPI.txInRef . getCardanoTxInputs

getCardanoTxFee :: CardanoTx -> Value
getCardanoTxFee = onCardanoTx txFee (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoFee txFee)

getCardanoTxMint :: CardanoTx -> Value
getCardanoTxMint = onCardanoTx txMint (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoMintValue txMintValue)

getCardanoTxValidityRange :: CardanoTx -> SlotRange
getCardanoTxValidityRange = onCardanoTx txValidRange
    (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoValidityRange txValidityRange)

getCardanoTxData :: CardanoTx -> Map.Map DatumHash Datum
getCardanoTxData = onCardanoTx txData
    (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        Map.fromList $ mapMaybe (\(C.TxOut _ _ d _) -> fromCardanoTxOutDatum d) txOuts)
    where
      -- FIXME duplicated code
        fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (DatumHash, Datum)
        fromCardanoTxOutDatum C.TxOutDatumNone = Nothing
        fromCardanoTxOutDatum (C.TxOutDatumHash _ _) = Nothing
        fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
            let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
        fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
            let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
    -- TODO: add txMetaData

getCardanoTxRedeemers :: CardanoTx -> Redeemers
getCardanoTxRedeemers = onCardanoTx txRedeemers (const Map.empty) -- TODO: To implement
    -- (\(CardanoAPI.SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
    --     Map.fromList $ mapMaybe (\(C.TxOut _ _ d _) -> fromCardanoTxOutDatum d) txOuts)
    -- where
    --     fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (DatumHash, Datum)
    --     fromCardanoTxOutDatum C.TxOutDatumNone = Nothing
    --     fromCardanoTxOutDatum (C.TxOutDatumHash _ _) = Nothing
    --     fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
    --         let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
    --     fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
    --         let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')

instance Pretty Tx where
    pretty tx =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList $ txInputs tx)))
                , hang 2 (vsep ("reference inputs:" : fmap pretty (Set.toList $ txReferenceInputs tx)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (Set.toList $ txCollateral tx)))
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
txId tx = TxId $ toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: CardanoTx -> Map.Map TxOutRef TxOut -> Map.Map TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx) `Map.union` getCardanoTxUnspentOutputsTx tx

-- | Update a map of unspent transaction outputs and signatures based
--   on the collateral inputs of a transaction (for when it is invalid).
updateUtxoCollateral :: CardanoTx -> Map.Map TxOutRef TxOut -> Map.Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.map CardanoAPI.txInRef $ getCardanoTxCollateralInputs tx)

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map.Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: Value -> PaymentPubKey -> Maybe StakePubKey -> TxOut
pubKeyTxOut v pk sk = TxOut (pubKeyAddress pk sk) v NoOutputDatum Nothing

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

getOutputDatumHash :: OutputDatum -> Maybe DatumHash
getOutputDatumHash (getOutputDatumOrHash -> mdoe) =
  mdoe >>= either Just (Just . datumHash)

getOutputDatumOrHash :: OutputDatum -> Maybe (Either DatumHash Datum)
getOutputDatumOrHash NoOutputDatum        = Nothing
getOutputDatumOrHash (OutputDatumHash dh) = Just (Left dh)
getOutputDatumOrHash (OutputDatum da)     = Just (Right da)

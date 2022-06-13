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
    ( module Export
    -- * ChainIndexTxOut
    , ChainIndexTxOut(..)
    , toTxOut
    , fromTxOut
    -- ** Lenses and Prisms
    , ciTxOutAddress
    , ciTxOutValue
    , ciTxOutDatum
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
    , SomeCardanoApiTx(.., CardanoApiEmulatorEraTx)
    , ToCardanoError(..)
    -- * Transactions
    , addSignature
    , addSignature'
    , pubKeyTxOut
    , scriptTxOut
    , scriptTxOut'
    , updateUtxo
    , updateUtxoCollateral
    , txOutRefs
    , unspentOutputsTx
    -- * Hashing transactions
    , txId
    ) where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash (SHA256, digest)
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise (encode))
import Control.Lens (At (at), makeLenses, makePrisms, (&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Address (PaymentPubKey, StakePubKey, pubKeyAddress, scriptAddress)
import Ledger.Crypto (Passphrase, PrivateKey, signTx, signTx', toPublicKey)
import Ledger.Orphans ()
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum (Datum), DatumHash, TxId (TxId),
                             Validator, ValidatorHash, Value, addressCredential, toBuiltin)
import Plutus.V1.Ledger.Slot (SlotRange)
import Plutus.V1.Ledger.Tx as Export hiding (updateUtxoCollateral)
import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))

-- | Transaction output that comes from a chain index query.
--
-- It is defined here instead of the plutus-chain-index because plutus-ledger
-- uses that datatype, and plutus-ledger can't depend on plutus-chain-index
-- because of a cyclic dependency.
--
-- This datatype was created in order to be used in
-- 'Ledger.Constraints.processConstraint', specifically with the constraints
-- 'MustSpendPubKeyOutput' and 'MustSpendScriptOutput'.
--
-- TODO Add 'Either DatumHash Datum' field for 'PublicKeyChainIndexTxOut'.
data ChainIndexTxOut =
    PublicKeyChainIndexTxOut { _ciTxOutAddress :: Address
                             , _ciTxOutValue   :: Value
                             }
  | ScriptChainIndexTxOut { _ciTxOutAddress   :: Address
                          , _ciTxOutValidator :: Either ValidatorHash Validator
                          , _ciTxOutDatum     :: Either DatumHash Datum
                          , _ciTxOutValue     :: Value
                          }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

makeLenses ''ChainIndexTxOut
makePrisms ''ChainIndexTxOut

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that converting from 'ChainIndexTxOut' to 'TxOut' and back to
-- 'ChainIndexTxOut' loses precision ('Datum' and 'Validator' are changed to 'DatumHash' and 'ValidatorHash' respectively)
toTxOut :: ChainIndexTxOut -> TxOut
toTxOut (PublicKeyChainIndexTxOut addr v)          = TxOut addr v Nothing
toTxOut (ScriptChainIndexTxOut addr _ (Left dh) v) = TxOut addr v (Just dh)
toTxOut (ScriptChainIndexTxOut addr _ (Right d) v) = TxOut addr v (Just $ datumHash d)

-- | Converts a plutus-ledger-api transaction output to the chain index
-- transaction output.
fromTxOut :: TxOut -> Maybe ChainIndexTxOut
fromTxOut TxOut { txOutAddress, txOutValue, txOutDatumHash } =
  case addressCredential txOutAddress of
    PubKeyCredential _ -> pure $ PublicKeyChainIndexTxOut txOutAddress txOutValue
    ScriptCredential vh ->
      txOutDatumHash >>= \dh ->
        pure $ ScriptChainIndexTxOut txOutAddress (Left vh) (Left dh) txOutValue

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
    | Both { _emulatorTx :: Tx, _cardanoApiTx :: SomeCardanoApiTx }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema, Serialise)

makeLenses ''CardanoTx

getEmulatorEraTx :: SomeCardanoApiTx -> C.Tx C.AlonzoEra
getEmulatorEraTx (SomeTx tx C.AlonzoEraInCardanoMode) = tx
getEmulatorEraTx _                                    = error "getEmulatorEraTx: Expected an Alonzo tx"

pattern CardanoApiEmulatorEraTx :: C.Tx C.AlonzoEra -> SomeCardanoApiTx
pattern CardanoApiEmulatorEraTx tx <- (getEmulatorEraTx -> tx) where
    CardanoApiEmulatorEraTx tx = SomeTx tx C.AlonzoEraInCardanoMode

{-# COMPLETE CardanoApiEmulatorEraTx #-}

instance Pretty CardanoTx where
    pretty = onCardanoTx pretty (pretty . getCardanoApiTxId)

onCardanoTx :: (Tx -> r) -> (SomeCardanoApiTx -> r) -> CardanoTx -> r
onCardanoTx l r = mergeCardanoTxWith l r const

mergeCardanoTxWith :: (Tx -> a) -> (SomeCardanoApiTx -> a) -> (a -> a -> a) -> CardanoTx -> a
mergeCardanoTxWith l _ _ (EmulatorTx tx)    = l tx
mergeCardanoTxWith l r m (Both tx ctx)      = m (l tx) (r ctx)
mergeCardanoTxWith _ r _ (CardanoApiTx ctx) = r ctx

cardanoTxMap :: (Tx -> Tx) -> (SomeCardanoApiTx -> SomeCardanoApiTx) -> CardanoTx -> CardanoTx
cardanoTxMap l _ (EmulatorTx tx)    = EmulatorTx (l tx)
cardanoTxMap l r (Both tx ctx)      = Both (l tx) (r ctx)
cardanoTxMap _ r (CardanoApiTx ctx) = CardanoApiTx (r ctx)

getCardanoTxId :: CardanoTx -> TxId
getCardanoTxId = onCardanoTx txId getCardanoApiTxId

getCardanoApiTxId :: SomeCardanoApiTx -> TxId
getCardanoApiTxId (SomeTx (C.Tx body _) _) = CardanoAPI.fromCardanoTxId $ C.getTxId body

getCardanoTxInputs :: CardanoTx -> Set TxIn
getCardanoTxInputs = onCardanoTx txInputs
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        Set.fromList $ fmap ((`TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns)

getCardanoTxCollateralInputs :: CardanoTx -> Set TxIn
getCardanoTxCollateralInputs = onCardanoTx txCollateral
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        CardanoAPI.fromCardanoTxInsCollateral txInsCollateral)

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, TxOutRef)]
getCardanoTxOutRefs = onCardanoTx txOutRefs CardanoAPI.txOutRefs

getCardanoTxOutputs :: CardanoTx -> [TxOut]
getCardanoTxOutputs = fmap fst . getCardanoTxOutRefs

getCardanoTxUnspentOutputsTx :: CardanoTx -> Map TxOutRef TxOut
getCardanoTxUnspentOutputsTx = onCardanoTx unspentOutputsTx CardanoAPI.unspentOutputsTx

getCardanoTxSpentOutputs :: CardanoTx -> Set TxOutRef
getCardanoTxSpentOutputs = Set.map txInRef . getCardanoTxInputs

getCardanoTxFee :: CardanoTx -> Value
getCardanoTxFee = onCardanoTx txFee (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoFee txFee)

getCardanoTxMint :: CardanoTx -> Value
getCardanoTxMint = onCardanoTx txMint (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoMintValue txMintValue)

getCardanoTxValidityRange :: CardanoTx -> SlotRange
getCardanoTxValidityRange = onCardanoTx txValidRange
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> CardanoAPI.fromCardanoValidityRange txValidityRange)

getCardanoTxData :: CardanoTx -> Map DatumHash Datum
getCardanoTxData = onCardanoTx txData
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) -> Map.fromList $ mapMaybe (\(C.TxOut _ _ d) -> fromCardanoTxOutDatum d) txOuts)
    where
        fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (DatumHash, Datum)
        fromCardanoTxOutDatum (C.TxOutDatum _ d) = let d' = Datum $ CardanoAPI.fromCardanoScriptData d in Just (datumHash d', d')
        fromCardanoTxOutDatum _ = Nothing
    -- TODO: add txMetaData

instance Pretty Tx where
    pretty t@Tx{txInputs, txCollateral, txOutputs, txMint, txFee, txValidRange, txSignatures, txMintScripts, txData} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList txInputs)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (Set.toList txCollateral)))
                , hang 2 (vsep ("outputs:" : fmap pretty txOutputs))
                , "mint:" <+> pretty txMint
                , "fee:" <+> pretty txFee
                , hang 2 (vsep ("mps:": fmap pretty (Set.toList txMintScripts)))
                , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList txSignatures)))
                , "validity range:" <+> viaShow txValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList txData) ))
                ]
            txid = txId t
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
updateUtxo :: CardanoTx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx) `Map.union` getCardanoTxUnspentOutputsTx tx

-- | Update a map of unspent transaction outputs and signatures based
--   on the collateral inputs of a transaction (for when it is invalid).
updateUtxoCollateral :: CardanoTx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.map txInRef $ getCardanoTxCollateralInputs tx)

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a validator script hash
--   with the given data script attached.
scriptTxOut' :: Value -> Address -> Datum -> TxOut
scriptTxOut' v a ds = TxOut a v (Just (datumHash ds))

-- | Create a transaction output locked by a validator script and with the given data script attached.
scriptTxOut :: Value -> Validator -> Datum -> TxOut
scriptTxOut v vs = scriptTxOut' v (scriptAddress vs)

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: Value -> PaymentPubKey -> Maybe StakePubKey -> TxOut
pubKeyTxOut v pk sk = TxOut (pubKeyAddress pk sk) v Nothing

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

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
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- https://gitlab.haskell.org/ghc/ghc/-/issues/14630

module Ledger.Tx
    ( module Ledger.Tx.Internal
    , module Plutus.V1.Ledger.Tx
    , OffChainTxOut(PublicKeyOffChainTxOut, ScriptOffChainTxOut)
    -- * PublicKeyOffChainTxOut
    , ocTxOutPublicKeyHash
    , ocTxOutPublicKeyValue
    , ocTxOutPublicKeyDatumHash
    , ocTxOutPublicKeyReferenceScript
    , ocTxOutPublicKeyStakingCredential
    -- * ScriptOffChainTxOut
    , ocTxOutValidatorHash
    , ocTxOutScriptValue
    , ocTxOutScriptDatumHash
    , ocTxOutValidator
    , ocTxOutScriptDatum
    , ocTxOutScriptReferenceScript
    , ocTxOutScriptStakingCredential
    -- * Common accessors
    , ocTxOutAddress
    , ocTxOutValue
    , ocTxOutDatum
    , ocTxOutDatumHash
    , ocTxOutReferenceScript
    , ocTxOutStakingCredential
    -- * OffChainTxOut smart constructors
    , mkPublicKeyOffChainTxOut
    , mkScriptOffChainTxOut
    , toTxOut
    , fromTxOut
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
    , updateUtxo
    , updateUtxoCollateral
    , txOutRefs
    , unspentOutputsTx
    -- * Hashing transactions
    , txId
    ) where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise (encode))
import Control.Lens (At (at), makeLenses, (&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
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
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Internal hiding (updateUtxoCollateral)
import Plutus.Script.Utils.V1.Scripts (datumHash, validatorHash)
import Plutus.V1.Ledger.Api qualified as V1
-- for re-export
import Plutus.V1.Ledger.Address qualified as V1
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Tx qualified as V1.Tx
import Prettyprinter (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))

type PrivateKey = Crypto.XPrv

-- | Transaction output represented off-chain. The constructors are not
-- exported to preserve consistency between the validator, datum and their
-- hashes. Please use the smart constructors 'mkPublicKeyOffChainTxOut',
-- 'mkScriptOffChainTxOut' and the patterns below.
data OffChainTxOut =
    PublicKeyOffChainTxOut'
      V1.PubKeyHash                -- ^ Hash of the public key protecting the
                                   -- transaction output.
      V1.Value                     -- ^ Value of the transaction output.
      (Maybe V1.DatumHash)         -- ^ Optional datum hash attached to the
                                   -- transaction output.
      (Maybe V1.Datum)             -- ^ Optional datum attached to the transaction
                                   -- output, either found inline or resolved from
                                   -- its hash offchain.
      (Maybe V1.Script)            -- ^ Optional reference script attached to the
                                   -- transaction output.
      (Maybe V1.StakingCredential) -- staking credential of the address (if any)
  | ScriptOffChainTxOut'
      V1.ValidatorHash             -- ^ Hash of the validator protecting the
                                   -- transaction output.
      V1.Value                     -- ^ Value of the transaction output.
      V1.DatumHash                 -- ^ FIXME Hash of the datum attached to the
                                   -- transaction output.
      (Maybe V1.Validator)         -- ^ Validator script protecting the transaction
                                   -- output, resolved from its hash off-chain
      (Maybe V1.Datum)             -- ^ Datum attached to the transaction output,
                                   -- either found inline or resolved from its hash
                                   -- off-chain. A transaction output protected by
                                   -- a Plutus script is need to have an associated
                                   -- datum to be spendable.
      (Maybe V1.Script)            -- ^ Optional reference script attached to the
                                   -- transaction output. The reference script is,
                                   -- in genereal, unrelated to the validator
                                   -- script althought it could also be the same.
      (Maybe V1.StakingCredential) -- staking credential of the address (if any)
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

pattern PublicKeyOffChainTxOut :: V1.PubKeyHash -> V1.Value -> Maybe V1.DatumHash -> Maybe V1.Datum -> Maybe V1.Script -> Maybe V1.StakingCredential -> OffChainTxOut
pattern PublicKeyOffChainTxOut { ocTxOutPublicKeyHash, ocTxOutPublicKeyValue, ocTxOutPublicKeyDatumHash, ocTxOutDatum, ocTxOutPublicKeyReferenceScript, ocTxOutPublicKeyStakingCredential } <- PublicKeyOffChainTxOut' ocTxOutPublicKeyHash ocTxOutPublicKeyValue ocTxOutPublicKeyDatumHash ocTxOutDatum ocTxOutPublicKeyReferenceScript ocTxOutPublicKeyStakingCredential

pattern ScriptOffChainTxOut :: V1.ValidatorHash -> V1.Value -> V1.DatumHash -> Maybe V1.Validator -> Maybe V1.Datum -> Maybe V1.Script -> Maybe V1.StakingCredential -> OffChainTxOut
pattern ScriptOffChainTxOut { ocTxOutValidatorHash, ocTxOutScriptValue, ocTxOutScriptDatumHash, ocTxOutValidator, ocTxOutScriptDatum, ocTxOutScriptReferenceScript, ocTxOutScriptStakingCredential } <- ScriptOffChainTxOut' ocTxOutValidatorHash ocTxOutScriptValue ocTxOutScriptDatumHash ocTxOutValidator ocTxOutScriptDatum ocTxOutScriptReferenceScript ocTxOutScriptStakingCredential

{-# COMPLETE PublicKeyOffChainTxOut, ScriptOffChainTxOut #-}

mkPublicKeyOffChainTxOut :: V1.PubKeyHash -> V1.Value -> Maybe (Either V1.DatumHash V1.Datum) -> Maybe V1.Script -> OffChainTxOut
mkPublicKeyOffChainTxOut pkh va m_doe m_sc =
  PublicKeyOffChainTxOut'
    pkh
    va
    (either id datumHash <$> m_doe)
    (either (const Nothing) Just =<< m_doe)
    m_sc
    Nothing -- FIXME staking credential

mkScriptOffChainTxOut :: Either V1.ValidatorHash V1.Validator -> V1.Value -> Either V1.DatumHash V1.Datum -> Maybe V1.Script -> OffChainTxOut
mkScriptOffChainTxOut voh va doh m_sc =
  ScriptOffChainTxOut'
    (either id validatorHash voh)
    va
    (either id datumHash doh)
    (either (const Nothing) Just voh)
    (either (const Nothing) Just doh)
    m_sc
    Nothing -- FIXME staking credential

ocTxOutAddress :: OffChainTxOut -> V1.Address
ocTxOutAddress PublicKeyOffChainTxOut { ocTxOutPublicKeyHash } =
  V1.pubKeyHashAddress ocTxOutPublicKeyHash
ocTxOutAddress ScriptOffChainTxOut { ocTxOutValidatorHash } =
  V1.scriptHashAddress ocTxOutValidatorHash

ocTxOutDatumHash :: OffChainTxOut -> Maybe V1.DatumHash
ocTxOutDatumHash PublicKeyOffChainTxOut { ocTxOutPublicKeyDatumHash } =
  ocTxOutPublicKeyDatumHash
ocTxOutDatumHash ScriptOffChainTxOut { ocTxOutScriptDatumHash } =
  Just ocTxOutScriptDatumHash

ocTxOutValue :: OffChainTxOut -> V1.Value
ocTxOutValue PublicKeyOffChainTxOut { ocTxOutPublicKeyValue } =
  ocTxOutPublicKeyValue
ocTxOutValue ScriptOffChainTxOut { ocTxOutScriptValue } =
  ocTxOutScriptValue

ocTxOutReferenceScript :: OffChainTxOut -> Maybe V1.Script
ocTxOutReferenceScript PublicKeyOffChainTxOut { ocTxOutPublicKeyReferenceScript } =
  ocTxOutPublicKeyReferenceScript
ocTxOutReferenceScript ScriptOffChainTxOut { ocTxOutScriptReferenceScript } =
  ocTxOutScriptReferenceScript

ocTxOutStakingCredential :: OffChainTxOut -> Maybe V1.StakingCredential
ocTxOutStakingCredential PublicKeyOffChainTxOut { ocTxOutPublicKeyStakingCredential } =
  ocTxOutPublicKeyStakingCredential
ocTxOutStakingCredential ScriptOffChainTxOut { ocTxOutScriptStakingCredential } =
  ocTxOutScriptStakingCredential

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that 'OffChainTxOut' supports features such inline datums and
-- reference scripts which are not supported by V1 TxOut. Converting from
-- 'OffChainTxOut' to 'TxOut' and back is therefore lossy.
toTxOut :: OffChainTxOut -> V1.Tx.TxOut
toTxOut PublicKeyOffChainTxOut { ocTxOutPublicKeyHash, ocTxOutPublicKeyValue, ocTxOutPublicKeyDatumHash } =
  V1.Tx.TxOut (V1.pubKeyHashAddress ocTxOutPublicKeyHash) ocTxOutPublicKeyValue ocTxOutPublicKeyDatumHash
toTxOut ScriptOffChainTxOut { ocTxOutValidatorHash, ocTxOutScriptValue, ocTxOutScriptDatumHash } =
  V1.Tx.TxOut (V1.scriptHashAddress ocTxOutValidatorHash) ocTxOutScriptValue (Just ocTxOutScriptDatumHash)

-- | Converts a plutus-ledger-api transaction output to the chain index
-- transaction output.
fromTxOut :: V1.Tx.TxOut -> Maybe OffChainTxOut
fromTxOut (V1.Tx.TxOut address value mDatumHash) =
  case V1.addressCredential address of
    V1.PubKeyCredential pkh ->
      pure $ PublicKeyOffChainTxOut' pkh value mDatumHash Nothing Nothing (V1.stakingCredential address)
    V1.ScriptCredential vh -> do
      dh <- mDatumHash
      pure $ ScriptOffChainTxOut' vh value dh Nothing Nothing Nothing (V1.stakingCredential address)

instance Pretty OffChainTxOut where
    pretty txOut@PublicKeyOffChainTxOut {} =
      hang 2 $ vsep
        [ "-" <+> pretty (ocTxOutValue txOut) <+> "addressed to"
        , pretty (ocTxOutAddress txOut)
        ]
    pretty txOut@ScriptOffChainTxOut {} =
      hang 2 $ vsep
        [ "-" <+> pretty (ocTxOutValue txOut) <+> "addressed to"
        , pretty (ocTxOutAddress txOut)
        ]

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
    pretty tx =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList (getCardanoTxInputs tx))))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (Set.toList (getCardanoTxCollateralInputs tx))))
                , hang 2 (vsep ("outputs:" : fmap pretty (getCardanoTxOutputs tx)))
                , "mint:" <+> pretty (getCardanoTxMint tx)
                , "fee:" <+> pretty (getCardanoTxFee tx)
                ] ++ onCardanoTx (\tx' ->
                    [ hang 2 (vsep ("mps:": fmap pretty (Set.toList (txMintScripts tx'))))
                    , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList (txSignatures tx'))))
                    ]) (const []) tx ++
                [ "validity range:" <+> viaShow (getCardanoTxValidityRange tx)
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList (getCardanoTxData tx))))
                ]
        in nest 2 $ vsep ["Tx" <+> pretty (getCardanoTxId tx) <> colon, braces (vsep lines')]

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

getCardanoTxId :: CardanoTx -> V1.Tx.TxId
getCardanoTxId = onCardanoTx txId getCardanoApiTxId

getCardanoApiTxId :: SomeCardanoApiTx -> V1.Tx.TxId
getCardanoApiTxId (SomeTx (C.Tx body _) _) = CardanoAPI.fromCardanoTxId $ C.getTxId body

getCardanoTxInputs :: CardanoTx -> Set V1.Tx.TxIn
getCardanoTxInputs = onCardanoTx txInputs
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        Set.fromList $ fmap ((`V1.Tx.TxIn` Nothing) . CardanoAPI.fromCardanoTxIn . fst) txIns)

getCardanoTxCollateralInputs :: CardanoTx -> Set V1.Tx.TxIn
getCardanoTxCollateralInputs = onCardanoTx txCollateral
    (\(SomeTx (C.Tx (C.TxBody C.TxBodyContent {..}) _) _) ->
        CardanoAPI.fromCardanoTxInsCollateral txInsCollateral)

getCardanoTxOutRefs :: CardanoTx -> [(V1.Tx.TxOut, V1.Tx.TxOutRef)]
getCardanoTxOutRefs = onCardanoTx txOutRefs CardanoAPI.txOutRefs

getCardanoTxOutputs :: CardanoTx -> [V1.Tx.TxOut]
getCardanoTxOutputs = fmap fst . getCardanoTxOutRefs

getCardanoTxUnspentOutputsTx :: CardanoTx -> Map V1.Tx.TxOutRef V1.Tx.TxOut
getCardanoTxUnspentOutputsTx = onCardanoTx unspentOutputsTx CardanoAPI.unspentOutputsTx

getCardanoTxSpentOutputs :: CardanoTx -> Set V1.Tx.TxOutRef
getCardanoTxSpentOutputs = Set.map V1.Tx.txInRef . getCardanoTxInputs

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
txId :: Tx -> V1.Tx.TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx = V1.Tx.TxId $ V1.toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: CardanoTx -> Map V1.Tx.TxOutRef V1.Tx.TxOut -> Map V1.Tx.TxOutRef V1.Tx.TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx) `Map.union` getCardanoTxUnspentOutputsTx tx

-- | Update a map of unspent transaction outputs and signatures based
--   on the collateral inputs of a transaction (for when it is invalid).
updateUtxoCollateral :: CardanoTx -> Map V1.Tx.TxOutRef V1.Tx.TxOut -> Map V1.Tx.TxOutRef V1.Tx.TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.map V1.Tx.txInRef $ getCardanoTxCollateralInputs tx)

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(V1.Tx.TxOut, V1.Tx.TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, V1.Tx.TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map V1.Tx.TxOutRef V1.Tx.TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (V1.Tx.TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: V1.Value -> PaymentPubKey -> Maybe StakePubKey -> V1.Tx.TxOut
pubKeyTxOut v pk sk = V1.Tx.TxOut (pubKeyAddress pk sk) v Nothing

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

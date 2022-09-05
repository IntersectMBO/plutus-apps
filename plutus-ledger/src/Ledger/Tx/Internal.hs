{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Tx.Internal
    ( module Ledger.Tx.Internal
    , Language(..)
    , TxOut (..)
    , TxOutRef (..)
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Genesis ()
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, decode, encode)
import Control.DeepSeq (NFData, rnf)
import Control.Lens qualified as L
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.OpenApi qualified as OpenApi
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.Slot
import Ledger.Tx.CardanoAPI.Internal (fromCardanoAddressInEra, fromCardanoValue)
import Ledger.Tx.Orphans ()
import Ledger.Tx.Orphans.V2 ()
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Address (toPubKeyHash)
import Plutus.V1.Ledger.Address qualified as V1
import Plutus.V1.Ledger.Api (dataToBuiltinData)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx hiding (TxIn (..), TxInType (..), TxOut (..), inRef, inScripts, inType, pubKeyTxIn,
                            pubKeyTxIns, scriptTxIn, scriptTxIns)
import Plutus.V1.Ledger.Value as V
import PlutusTx.Lattice
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (..), hang, viaShow, vsep, (<+>))

deriving instance Serialise Language

-- | The type of a transaction input.
data TxInType =
      ConsumeScriptAddress !Language !Validator !Redeemer !Datum
      -- ^ A transaction input that consumes a script address with the given the language type, validator, redeemer, and datum.
    | ConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | ConsumeSimpleScriptAddress -- ^ Consume a simple script
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

-- | A transaction input, consisting of a transaction output reference and an input type.
data TxIn = TxIn {
    txInRef  :: !TxOutRef,
    txInType :: Maybe TxInType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)


instance Pretty TxIn where
    pretty TxIn{txInRef,txInType} =
                let rest =
                        case txInType of
                            Just (ConsumeScriptAddress _ _ redeemer _) ->
                                pretty redeemer
                            _ -> mempty
                in hang 2 $ vsep ["-" <+> pretty txInRef, rest]

-- | The 'TxOutRef' spent by a transaction input.
inRef :: L.Lens' TxIn TxOutRef
inRef = L.lens txInRef s where
    s txi r = txi { txInRef = r }

-- | The type of a transaction input.
inType :: L.Lens' TxIn (Maybe TxInType)
inType = L.lens txInType s where
    s txi t = txi { txInType = t }

-- | Validator, redeemer, and data scripts of a transaction input that spends a
--   "pay to script" output.
inScripts :: TxIn -> Maybe (Language, Validator, Redeemer, Datum)
inScripts TxIn{ txInType = t } = case t of
    Just (ConsumeScriptAddress l v r d) -> Just (l, v, r, d)
    _                                   -> Nothing

-- | A transaction input that spends a "pay to public key" output, given the witness.
pubKeyTxIn :: TxOutRef -> TxIn
pubKeyTxIn r = TxIn r (Just ConsumePublicKeyAddress)

-- | A transaction input that spends a "pay to script" output, given witnesses.
scriptTxIn :: TxOutRef -> Language -> Validator -> Redeemer -> Datum -> TxIn
scriptTxIn ref l v r d = TxIn ref . Just $ ConsumeScriptAddress l v r d

-- | Filter to get only the pubkey inputs.
pubKeyTxIns :: L.Fold (Set.Set TxIn) TxIn
pubKeyTxIns = L.folding (Set.filter (\TxIn{ txInType = t } -> t == Just ConsumePublicKeyAddress))

-- | Filter to get only the script inputs.
scriptTxIns :: L.Fold (Set.Set TxIn) TxIn
scriptTxIns = (\x -> L.folding x) . Set.filter $ \case
    TxIn{ txInType = Just ConsumeScriptAddress{} } -> True
    _                                              -> False

newtype TxOut = TxOut {getTxOut :: C.TxOut C.CtxTx C.BabbageEra}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
    -- deriving anyclass ( Serialise, NFData)

instance Serialise TxOut where
  encode = undefined -- FIXME
  decode = undefined -- FIXME

instance NFData TxOut where
  rnf = undefined -- FIXME

instance OpenApi.ToSchema TxOut where
    declareNamedSchema _ = undefined -- FIXME

instance Pretty TxOut where
  pretty = viaShow . getTxOut

-- | A Babbage era transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs          :: [TxIn],
    -- ^ The inputs to this transaction.
    txReferenceInputs :: [TxIn],
    -- ^ The reference inputs to this transaction.
    txCollateral      :: [TxIn],
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs         :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint            :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee             :: !Value,
    -- ^ The fee for this transaction.
    txValidRange      :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintScripts     :: Map.Map MintingPolicyHash MintingPolicy,
    -- ^ The scripts that must be run to check minting conditions.
    -- We include the minting policy hash in order to be able to include
    -- PlutusV1 AND PlutusV2 minting policy scripts, because the hashing
    -- function is different for each Plutus script version.
    txSignatures      :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txRedeemers       :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    txData            :: Map DatumHash Datum
    -- ^ Datum objects recorded on this transaction.
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise, NFData)


instance Semigroup Tx where
    tx1 <> tx2 = Tx {
        txInputs = txInputs tx1 <> txInputs tx2,
        txReferenceInputs = txReferenceInputs tx1 <> txReferenceInputs tx2,
        txCollateral = txCollateral tx1 <> txCollateral tx2,
        txOutputs = txOutputs tx1 <> txOutputs tx2,
        txMint = txMint tx1 <> txMint tx2,
        txFee = txFee tx1 <> txFee tx2,
        txValidRange = txValidRange tx1 /\ txValidRange tx2,
        txMintScripts = txMintScripts tx1 <> txMintScripts tx2,
        txSignatures = txSignatures tx1 <> txSignatures tx2,
        txRedeemers = txRedeemers tx1 <> txRedeemers tx2,
        txData = txData tx1 <> txData tx2
        }

instance Monoid Tx where
    mempty = Tx mempty mempty mempty mempty mempty mempty top mempty mempty mempty mempty

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The inputs of a transaction.
inputs :: L.Lens' Tx [TxIn]
inputs = L.lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The reference inputs of a transaction.
referenceInputs :: L.Lens' Tx [TxIn]
referenceInputs = L.lens g s where
    g = txReferenceInputs
    s tx i = tx { txReferenceInputs = i }

-- | The collateral inputs of a transaction for paying fees when validating the transaction fails.
collateralInputs :: L.Lens' Tx [TxIn]
collateralInputs = L.lens g s where
    g = txCollateral
    s tx i = tx { txCollateral = i }

-- | The outputs of a transaction.
outputs :: L.Lens' Tx [TxOut]
outputs = L.lens g s where
    g = txOutputs
    s tx o = tx { txOutputs = o }

-- | The validity range of a transaction.
validRange :: L.Lens' Tx SlotRange
validRange = L.lens g s where
    g = txValidRange
    s tx o = tx { txValidRange = o }

signatures :: L.Lens' Tx (Map PubKey Signature)
signatures = L.lens g s where
    g = txSignatures
    s tx sig = tx { txSignatures = sig }

fee :: L.Lens' Tx Value
fee = L.lens g s where
    g = txFee
    s tx v = tx { txFee = v }

mint :: L.Lens' Tx Value
mint = L.lens g s where
    g = txMint
    s tx v = tx { txMint = v }

mintScripts :: L.Lens' Tx (Map.Map MintingPolicyHash MintingPolicy)
mintScripts = L.lens g s where
    g = txMintScripts
    s tx fs = tx { txMintScripts = fs }

redeemers :: L.Lens' Tx Redeemers
redeemers = L.lens g s where
    g = txRedeemers
    s tx reds = tx { txRedeemers = reds }

datumWitnesses :: L.Lens' Tx (Map DatumHash Datum)
datumWitnesses = L.lens g s where
    g = txData
    s tx dat = tx { txData = dat }

lookupSignature :: PubKey -> Tx -> Maybe Signature
lookupSignature s Tx{txSignatures} = Map.lookup s txSignatures

lookupDatum :: Tx -> DatumHash -> Maybe Datum
lookupDatum Tx{txData} h = Map.lookup h txData

lookupRedeemer :: Tx -> RedeemerPtr -> Maybe Redeemer
lookupRedeemer tx p = Map.lookup p (txRedeemers tx)

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs  && nonNegative txFee
    where
      nonNegative i = V.geq i mempty

txOutValue :: TxOut -> Value
txOutValue (TxOut (C.TxOut _aie tov _tod _rs)) =
  fromCardanoValue $ C.txOutValueToValue tov

outValue :: L.Lens TxOut TxOut Value (C.TxOutValue C.BabbageEra)
outValue = L.lens
  txOutValue
  (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))

-- | A babbage era transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs          :: [TxOutRef],
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedReferenceInputs :: [TxOutRef],
    -- ^ The reference inputs to this transaction, as transaction output references only.
    txStrippedOutputs         :: [TxOut],
    -- ^ The outputs of this transation.
    txStrippedMint            :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txStrippedFee             :: !Value
    -- ^ The fee for this transaction.
    } deriving (Show, Eq, Generic, Serialise)

strip :: Tx -> TxStripped
strip Tx{..} = TxStripped i ri txOutputs txMint txFee where
    i = map txInRef txInputs
    ri = map txInRef txReferenceInputs

-- | A 'TxOut' along with the 'Tx' it comes from, which may have additional information e.g.
-- the full data script that goes with the 'TxOut'.
data TxOutTx = TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)

txOutTxDatum :: TxOutTx -> Maybe Datum
txOutTxDatum (TxOutTx tx (TxOut (C.TxOut _aie _tov tod _rs))) =
  case tod of
    C.TxOutDatumNone ->
      Nothing
    C.TxOutDatumHash _era scriptDataHash ->
      lookupDatum tx $ DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptDataHash)
    C.TxOutDatumInline _era scriptData ->
      Just $ Datum $ dataToBuiltinData $ C.toPlutusData scriptData
    C.TxOutDatumInTx _era scriptData ->
      Just $ Datum $ dataToBuiltinData $ C.toPlutusData scriptData

-- | Get a hash from the stored TxOutDatum (either dirctly or by hashing the inlined datum)
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash (TxOut (C.TxOut _aie _tov tod _rs)) =
  case tod of
    C.TxOutDatumNone ->
      Nothing
    C.TxOutDatumHash _era scriptDataHash ->
      Just $ DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptDataHash)
    C.TxOutDatumInline _era scriptData ->
      Just $ datumHash $ Datum $ dataToBuiltinData $ C.toPlutusData scriptData
    C.TxOutDatumInTx _era scriptData ->
      Just $ datumHash $ Datum $ dataToBuiltinData $ C.toPlutusData scriptData

txOutPubKey :: TxOut -> Maybe PubKeyHash
txOutPubKey (TxOut (C.TxOut aie _ _ _)) = toPubKeyHash $ fromCardanoAddressInEra aie

txOutAddress :: TxOut -> V1.Address
txOutAddress (TxOut (C.TxOut aie _tov _tod _rs)) = fromCardanoAddressInEra aie

outAddress :: L.Lens TxOut TxOut V1.Address (C.AddressInEra C.BabbageEra)
outAddress = L.lens
  txOutAddress
  (\(TxOut (C.TxOut _ tov tod rs)) aie -> TxOut (C.TxOut aie tov tod rs))

outDatumHash :: L.Lens TxOut TxOut (Maybe DatumHash) (C.TxOutDatum C.CtxTx C.BabbageEra)
outDatumHash = L.lens
  txOutDatumHash
  (\(TxOut (C.TxOut aie tov _ rs)) tod -> TxOut (C.TxOut aie tov tod rs))

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> [TxOutRef]
spentOutputs = map txInRef . txInputs

-- | The transaction output references referenced by a transaction.
referencedOutputs :: Tx -> [TxOutRef]
referencedOutputs = map txInRef . txReferenceInputs

-- | Update a map of unspent transaction outputs and signatures
--   for a failed transaction using its collateral inputs.
updateUtxoCollateral :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.fromList . map txInRef . txCollateral $ tx)


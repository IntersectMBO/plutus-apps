{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ledger.Tx.Internal
    ( module Ledger.Tx.Internal
    , Language(..)
    , TxOut (..)
    , TxOutRef (..)
    , Versioned(..)
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C hiding (toShelleyTxOut)
import Cardano.Binary qualified as C
import Cardano.Ledger.Alonzo.Genesis ()
import Codec.Serialise (Serialise, decode, encode)

import Cardano.Ledger.Core qualified as Ledger (TxOut)
import Cardano.Ledger.Serialization qualified as Ledger (Sized, mkSized)
import Ouroboros.Consensus.Shelley.Eras qualified as Ledger

import Control.Lens qualified as L
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)

import Ledger.Address (CardanoAddress, cardanoPubKeyHash, toPlutusAddress)
import Ledger.Contexts.Orphans ()
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Tx.CardanoAPI.Internal (fromCardanoTxOutDatum, fromCardanoTxOutValue)
import Ledger.Tx.CardanoAPITemp qualified as C
import Ledger.Tx.Orphans ()
import Ledger.Tx.Orphans.V2 ()

import Plutus.Script.Utils.Scripts
import Plutus.V1.Ledger.Api (Credential, DCert, dataToBuiltinData)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx hiding (TxIn (..), TxInType (..), TxOut (..), inRef, inScripts, inType, pubKeyTxIn,
                            pubKeyTxIns, scriptTxIn, scriptTxIns)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Prelude qualified as PlutusTx

import Prettyprinter (Pretty (..), hang, viaShow, vsep, (<+>))

txOutValue :: TxOut -> C.Value
txOutValue (TxOut (C.TxOut _aie tov _tod _rs)) =
  C.txOutValueToValue tov

outValue :: L.Lens TxOut TxOut C.Value (C.TxOutValue C.BabbageEra)
outValue = L.lens
  txOutValue
  (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))

outValue' :: L.Lens' TxOut (C.TxOutValue C.BabbageEra)
outValue' = L.lens
  (\(TxOut (C.TxOut _aie tov _tod _rs)) -> tov)
  (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))

-- | The type of a transaction input.
data TxInType =
      ScriptAddress !(Either (Versioned Validator) (Versioned TxOutRef)) !Redeemer !(Maybe Datum)
      -- ^ A transaction input that consumes (with a validator) or references (with a txOutRef)
      -- a script address with the given the redeemer and datum.
      -- Datum is optional if the input refers to a script output which contains an inline datum
    | ConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | ConsumeSimpleScriptAddress -- ^ Consume a simple script
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)

-- | A transaction input, consisting of a transaction output reference and an input type.
data TxIn = TxIn {
    txInRef  :: !TxOutRef,
    txInType :: Maybe TxInType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)


instance Pretty TxIn where
    pretty TxIn{txInRef,txInType} =
                let rest =
                        case txInType of
                            Just (ScriptAddress _ redeemer _) ->
                                pretty redeemer
                            _ -> mempty
                in hang 2 $ vsep ["-" <+> pretty txInRef, rest]

-- | A transaction input that spends a "pay to public key" output, given the witness.
pubKeyTxIn :: TxOutRef -> TxIn
pubKeyTxIn r = TxIn r (Just ConsumePublicKeyAddress)

-- | A transaction input that spends a "pay to script" output, given witnesses.
-- Datum is optional if the input refers to a script output which contains an inline datum
scriptTxIn :: TxOutRef -> Versioned Validator -> Redeemer -> Maybe Datum -> TxIn
scriptTxIn ref v r d = TxIn ref . Just $ ScriptAddress (Left v) r d

-- | The type of a transaction input with hashes.
data TxInputType =
      TxScriptAddress !Redeemer !(Either ValidatorHash (Versioned TxOutRef)) !(Maybe DatumHash)
      -- ^ A transaction input that consumes (with a validator hash) or references (with a txOutRef)
      -- a script address with the given the redeemer and datum hash.
    | TxConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | TxConsumeSimpleScriptAddress -- ^ Consume a simple script
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)

-- | A transaction input, consisting of a transaction output reference and an input type.
-- Differs with TxIn by: TxIn *maybe* contains *full* data witnesses, TxInput always contains redeemer witness, but datum/validator hashes.
data TxInput = TxInput {
    txInputRef  :: !TxOutRef,
    txInputType :: !TxInputType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)

-- same as TxIn
instance Pretty TxInput where
    pretty TxInput{txInputRef,txInputType} =
        let rest =
                case txInputType of
                    TxScriptAddress redeemer _ _ ->
                        pretty redeemer
                    _ -> mempty
        in hang 2 $ vsep ["-" <+> pretty txInputRef, rest]

-- | The 'TxOutRef' spent by a transaction input.
inputRef :: L.Lens' TxInput TxOutRef
inputRef = L.lens txInputRef s where
    s txi r = txi { txInputRef = r }

-- | The type of a transaction input.
inputType :: L.Lens' TxInput TxInputType
inputType = L.lens txInputType s where
    s txi t = txi { txInputType = t }

-- | Stake withdrawal, if applicable the script should be included in txScripts.
data Withdrawal = Withdrawal
  { withdrawalCredential :: Credential      -- ^ staking credential
  , withdrawalAmount     :: Integer         -- ^ amount of withdrawal in Lovelace, must withdraw all eligible amount
  , withdrawalRedeemer   :: Maybe Redeemer  -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty Withdrawal where
    pretty = viaShow

data Certificate = Certificate
  { certificateDcert    :: DCert
  , certificateRedeemer :: Maybe Redeemer           -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty Certificate where
    pretty = viaShow

-- | Validator, redeemer, and data scripts of a transaction input that spends a
--   "pay to script" output.
inScripts :: TxIn -> Maybe (Versioned Validator, Redeemer, Maybe Datum)
inScripts TxIn{ txInType = t } = case t of
    Just (ScriptAddress (Left v) r d) -> Just (v, r, d)
    _                                 -> Nothing

-- | The 'TxOutRef' spent by a transaction input.
inRef :: L.Lens' TxInput TxOutRef
inRef = L.lens txInputRef s where
    s txi r = txi { txInputRef = r }

-- | The type of a transaction input.
inType :: L.Lens' TxInput TxInputType
inType = L.lens txInputType s where
    s txi t = txi { txInputType = t }

-- | Filter to get only the pubkey inputs.
pubKeyTxInputs :: L.Fold [TxInput] TxInput
pubKeyTxInputs = L.folding (filter (\TxInput{ txInputType = t } -> t == TxConsumePublicKeyAddress))

-- | Filter to get only the scripts that consume or reference a script address
scriptTxInputs :: L.Fold [TxInput] TxInput
scriptTxInputs = (\x -> L.folding x) . filter $ \case
    TxInput{ txInputType = TxScriptAddress{} } -> True
    _                                          -> False

-- | Filter to get only the scripts that reference a script address
referenceScriptTxInputs :: L.Fold [TxInput] TxInput
referenceScriptTxInputs = (\x -> L.folding x) . filter $ \case
    TxInput{ txInputType = TxScriptAddress _ (Right _) _ } -> True
    _                                                      -> False

newtype TxOut = TxOut {getTxOut :: C.TxOut C.CtxTx C.BabbageEra}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance C.ToCBOR TxOut where
  toCBOR (TxOut txout) = C.toCBOR $ C.toShelleyTxOut C.ShelleyBasedEraBabbage txout

instance C.FromCBOR TxOut where
  fromCBOR = do
    txout <- C.fromCBOR
    pure $ TxOut $ C.fromShelleyTxOut C.ShelleyBasedEraBabbage txout

instance Serialise TxOut where
  encode = C.toCBOR
  decode = C.fromCBOR

instance Pretty TxOut where
  pretty (TxOut (C.TxOut addr v d rs)) =
    hang 2 $ vsep $
      ["-" <+> pretty (fromCardanoTxOutValue v) <+> "addressed to"
      , pretty (toPlutusAddress addr)
      ]
      <> case fromCardanoTxOutDatum d of
          PV2.NoOutputDatum      -> []
          PV2.OutputDatum dv     -> ["with inline datum" <+> viaShow dv]
          PV2.OutputDatumHash dh -> ["with datum hash" <+> pretty dh]
      <> case rs of
          C.ReferenceScript _ (C.ScriptInAnyLang _ s) ->
            ["with reference script hash" <+> viaShow (C.hashScript s)]
          C.ReferenceScriptNone -> []

toSizedTxOut :: TxOut -> Ledger.Sized (Ledger.TxOut Ledger.StandardBabbage)
toSizedTxOut = Ledger.mkSized . C.toShelleyTxOut C.ShelleyBasedEraBabbage . getTxOut


type ScriptsMap = Map ScriptHash (Versioned Script)
type MintingWitnessesMap = Map MintingPolicyHash (Redeemer, Maybe (Versioned TxOutRef))

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
txOutPubKey (TxOut (C.TxOut aie _ _ _)) = cardanoPubKeyHash aie

txOutAddress :: TxOut -> CardanoAddress
txOutAddress (TxOut (C.TxOut aie _tov _tod _rs)) = aie

outAddress :: L.Lens' TxOut (C.AddressInEra C.BabbageEra)
outAddress = L.lens
  txOutAddress
  (\(TxOut (C.TxOut _ tov tod rs)) aie -> TxOut (C.TxOut aie tov tod rs))

outDatumHash :: L.Lens TxOut TxOut (Maybe DatumHash) (C.TxOutDatum C.CtxTx C.BabbageEra)
outDatumHash = L.lens
  txOutDatumHash
  (\(TxOut (C.TxOut aie tov _ rs)) tod -> TxOut (C.TxOut aie tov tod rs))

type ReferenceScript = C.ReferenceScript C.BabbageEra

txOutReferenceScript :: TxOut -> ReferenceScript
txOutReferenceScript (TxOut (C.TxOut _aie _tov _tod rs)) = rs

outReferenceScript :: L.Lens' TxOut ReferenceScript
outReferenceScript = L.lens
  txOutReferenceScript
  (\(TxOut (C.TxOut aie tov tod _)) rs -> TxOut (C.TxOut aie tov tod rs))

lookupScript :: ScriptsMap -> ScriptHash -> Maybe (Versioned Script)
lookupScript txScripts hash = Map.lookup hash txScripts

lookupValidator :: ScriptsMap -> ValidatorHash -> Maybe (Versioned Validator)
lookupValidator txScripts = (fmap . fmap) Validator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (ValidatorHash b) = ScriptHash b

lookupMintingPolicy :: ScriptsMap -> MintingPolicyHash -> Maybe (Versioned MintingPolicy)
lookupMintingPolicy txScripts = (fmap . fmap) MintingPolicy . lookupScript txScripts . toScriptHash
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

lookupStakeValidator :: ScriptsMap -> StakeValidatorHash -> Maybe (Versioned StakeValidator)
lookupStakeValidator txScripts = (fmap . fmap) StakeValidator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (StakeValidatorHash b) = ScriptHash b

pubKeyTxInput :: TxOutRef -> TxInput
pubKeyTxInput outRef = TxInput outRef TxConsumePublicKeyAddress

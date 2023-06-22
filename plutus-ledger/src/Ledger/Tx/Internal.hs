{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}

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
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)

import Ledger.Address (CardanoAddress, cardanoPubKeyHash)
import Ledger.Contexts.Orphans ()
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Tx.CardanoAPITemp qualified as C
import Ledger.Tx.Orphans ()
import Ledger.Tx.Orphans.V2 ()

import Plutus.Script.Utils.Scripts
import Plutus.V1.Ledger.Api (Credential, DCert, dataToBuiltinData)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx hiding (TxIn (..), TxInType (..), TxOut (..), inRef, inType, pubKeyTxIn, scriptTxIn)
import PlutusTx.Prelude qualified as PlutusTx

import Prettyprinter (Pretty (..), viaShow)

cardanoTxOutValue :: C.TxOut ctx era -> C.Value
cardanoTxOutValue (C.TxOut _aie tov _tod _rs) =
  C.txOutValueToValue tov

txOutValue :: TxOut -> C.Value
txOutValue = cardanoTxOutValue . getTxOut

outValue :: L.Lens TxOut TxOut C.Value (C.TxOutValue C.BabbageEra)
outValue = L.lens
  txOutValue
  (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))

outValue' :: L.Lens' TxOut (C.TxOutValue C.BabbageEra)
outValue' = L.lens
  (\(TxOut (C.TxOut _aie tov _tod _rs)) -> tov)
  (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))


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

newtype TxOut = TxOut {getTxOut :: C.TxOut C.CtxTx C.BabbageEra}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Pretty)

instance C.ToCBOR TxOut where
  toCBOR (TxOut txout) = C.toCBOR $ C.toShelleyTxOut C.ShelleyBasedEraBabbage txout

instance C.FromCBOR TxOut where
  fromCBOR = do
    txout <- C.fromCBOR
    pure $ TxOut $ C.fromShelleyTxOut C.ShelleyBasedEraBabbage txout

instance Serialise TxOut where
  encode = C.toCBOR
  decode = C.fromCBOR

toSizedTxOut :: TxOut -> Ledger.Sized (Ledger.TxOut Ledger.StandardBabbage)
toSizedTxOut = Ledger.mkSized . C.toShelleyTxOut C.ShelleyBasedEraBabbage . getTxOut

toCtxUTxOTxOut :: TxOut -> C.TxOut C.CtxUTxO C.BabbageEra
toCtxUTxOTxOut = C.toCtxUTxOTxOut . getTxOut


type ScriptsMap = Map ScriptHash (Versioned Script)
type MintingWitnessesMap = Map MintingPolicyHash (Redeemer, Maybe (Versioned TxOutRef))

-- | Get a hash from the stored TxOutDatum (either directly or by hashing the inlined datum)
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

cardanoTxOutDatum :: TxOut -> Maybe Datum
cardanoTxOutDatum (TxOut (C.TxOut _aie _tov tod _rs)) =
  case tod of
    C.TxOutDatumNone ->
      Nothing
    C.TxOutDatumHash _era _scriptDataHash ->
      Nothing
    C.TxOutDatumInline _era scriptData ->
      Just $ Datum $ dataToBuiltinData $ C.toPlutusData scriptData
    C.TxOutDatumInTx _era _scriptData ->
      Nothing


cardanoTxOutDatumHash :: C.TxOutDatum C.CtxUTxO C.BabbageEra -> Maybe (C.Hash C.ScriptData)
cardanoTxOutDatumHash = \case
  C.TxOutDatumNone ->
    Nothing
  C.TxOutDatumHash _era scriptDataHash ->
    Just scriptDataHash
  C.TxOutDatumInline _era scriptData -> Just $ C.hashScriptData scriptData


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

emptyTxBodyContent :: C.TxBodyContent C.BuildTx C.BabbageEra
emptyTxBodyContent = C.TxBodyContent
   { txIns = []
   , txInsCollateral = C.TxInsCollateralNone
   , txMintValue = C.TxMintNone
   , txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra 0
   , txOuts = []
   , txProtocolParams = C.BuildTxWith $ Just $ C.fromLedgerPParams C.ShelleyBasedEraBabbage def
   , txInsReference = C.TxInsReferenceNone
   , txTotalCollateral = C.TxTotalCollateralNone
   , txReturnCollateral = C.TxReturnCollateralNone
   , txValidityRange = ( C.TxValidityNoLowerBound
                       , C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
   , txScriptValidity = C.TxScriptValidityNone
   , txExtraKeyWits = C.TxExtraKeyWitnessesNone
   , txMetadata = C.TxMetadataNone
   , txAuxScripts = C.TxAuxScriptsNone
   , txWithdrawals = C.TxWithdrawalsNone
   , txCertificates = C.TxCertificatesNone
   , txUpdateProposal = C.TxUpdateProposalNone
   }

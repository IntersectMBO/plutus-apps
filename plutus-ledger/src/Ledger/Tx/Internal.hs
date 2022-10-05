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
import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, decode, encode)
import Control.Applicative (empty, (<|>))
import Control.DeepSeq (NFData, rnf)
import Control.Lens ((&), (.~), (?~))
import Control.Lens qualified as L
import Control.Monad.State.Strict (execState, modify')
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Data (Proxy (Proxy))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Contexts.Orphans ()
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Slot
import Ledger.Tx.CardanoAPI.Internal (fromCardanoAddressInEra, fromCardanoTxOutDatum, fromCardanoTxOutValue,
                                      fromCardanoValue)
import Ledger.Tx.CardanoAPITemp qualified as C
import Ledger.Tx.Orphans ()
import Ledger.Tx.Orphans.V2 ()
import Plutus.Script.Utils.Scripts
import Plutus.V1.Ledger.Address (toPubKeyHash)
import Plutus.V1.Ledger.Address qualified as V1
import Plutus.V1.Ledger.Api (Credential, DCert, ScriptPurpose (..), StakingCredential (StakingHash), dataToBuiltinData)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx hiding (TxIn (..), TxInType (..), TxOut (..), inRef, inScripts, inType, pubKeyTxIn,
                            pubKeyTxIns, scriptTxIn, scriptTxIns)
import Plutus.V1.Ledger.Value as V
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Lattice
import PlutusTx.Prelude (BuiltinByteString)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (..), hang, viaShow, vsep, (<+>))

-- | The type of a transaction input.
data TxInType =
      ScriptAddress !(Either (Versioned Validator) (Versioned TxOutRef)) !Redeemer !Datum
      -- ^ A transaction input that consumes (with a validator) or references (with a txOutRef)
      -- a script address with the given the redeemer and datum.
    | ConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | ConsumeSimpleScriptAddress -- ^ Consume a simple script
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData, OpenApi.ToSchema)

-- | A transaction input, consisting of a transaction output reference and an input type.
data TxIn = TxIn {
    txInRef  :: !TxOutRef,
    txInType :: Maybe TxInType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData, OpenApi.ToSchema)


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
scriptTxIn :: TxOutRef -> Versioned Validator -> Redeemer -> Datum -> TxIn
scriptTxIn ref v r d = TxIn ref . Just $ ScriptAddress (Left v) r d

-- | The type of a transaction input with hashes.
data TxInputType =
      TxScriptAddress !Redeemer !(Either ValidatorHash (Versioned TxOutRef)) !DatumHash
      -- ^ A transaction input that consumes (with a validator hash) or references (with a txOutRef)
      -- a script address with the given the redeemer and datum hash.
    | TxConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | TxConsumeSimpleScriptAddress -- ^ Consume a simple script
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

-- | A transaction input, consisting of a transaction output reference and an input type.
-- Differs with TxIn by: TxIn *maybe* contains *full* data witnesses, TxInput always contains redeemer witness, but datum/validator hashes.
data TxInput = TxInput {
    txInputRef  :: !TxOutRef,
    txInputType :: !TxInputType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

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
  deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty Withdrawal where
    pretty = viaShow

data Certificate = Certificate
  { certificateDcert    :: DCert
  , certificateRedeemer :: Maybe Redeemer           -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty Certificate where
    pretty = viaShow

-- | Validator, redeemer, and data scripts of a transaction input that spends a
--   "pay to script" output.
inScripts :: TxIn -> Maybe (Versioned Validator, Redeemer, Datum)
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

-- | Filter to get only the script inputs.
scriptTxInputs :: L.Fold [TxInput] TxInput
scriptTxInputs = (\x -> L.folding x) . filter $ \case
    TxInput{ txInputType = TxScriptAddress{} } -> True
    _                                          -> False

-- | Validator, redeemer, and data scripts of a transaction input that spends a
--   "pay to script" output.
-- inScripts :: Tx -> TxInput -> Maybe (LedgerPlutusVersion, Validator, Redeemer, Datum)
-- inScripts tx i@TxInput{txInputType=TxConsumeScriptAddress pv _ _ _} = case txInType $ fillTxInputWitnesses tx i of
--     Just (ConsumeScriptAddress v r d) -> Just (pv, v, r, d)
--     _                                   -> Nothing
-- inScripts _ _ = Nothing

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

instance NFData TxOut where
  rnf (TxOut tx) = seq tx ()

instance OpenApi.ToSchema TxOut where
    declareNamedSchema _ = do
      addressSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy (C.AddressInEra C.BabbageEra))
      valueSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Value)
      bsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Datum)
      pure $ OpenApi.NamedSchema (Just "TxOut") $ mempty
        & OpenApi.type_ ?~ OpenApi.OpenApiObject
        & OpenApi.properties .~
          [ ("address", addressSchema)
          , ("value", valueSchema)
          , ("datum", bsSchema)
          , ("referenceScript", bsSchema)
          ]
        & OpenApi.required .~ ["address","value"]


instance Pretty TxOut where
  pretty (TxOut (C.TxOut addr v d rs)) =
    hang 2 $ vsep $
      ["-" <+> pretty (fromCardanoTxOutValue v) <+> "addressed to"
      , pretty (fromCardanoAddressInEra addr)
      ]
      <> case fromCardanoTxOutDatum d of
          PV2.NoOutputDatum      -> []
          PV2.OutputDatum dv     -> ["with inline datum" <+> viaShow dv]
          PV2.OutputDatumHash dh -> ["with datum hash" <+> pretty dh]
      <> case rs of
          C.ReferenceScript _ (C.ScriptInAnyLang _ s) ->
            ["with reference script hash" <+> viaShow (C.hashScript s)]
          C.ReferenceScriptNone -> []

-- | A Babbage-era transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs           :: [TxInput],
    -- ^ The inputs to this transaction.
    txReferenceInputs  :: [TxInput],
    -- ^ The reference inputs to this transaction.
    txCollateralInputs :: [TxInput],
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs          :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txReturnCollateral :: Maybe TxOut,
    -- ^ The output of the remaining collateral after covering fees in case validation of the transaction fails.
    txTotalCollateral  :: Maybe Value,
    -- ^ The total collateral to be paid in case validation of the transaction fails.
    txMint             :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee              :: !Value,
    -- ^ The fee for this transaction.
    txValidRange       :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintingScripts   :: Map MintingPolicyHash Redeemer,
    -- ^ The scripts that must be run to check minting conditions matched with their redeemers.
    txWithdrawals      :: [Withdrawal],
    -- ^ Withdrawals, contains redeemers.
    txCertificates     :: [Certificate],
    -- ^ Certificates, contains redeemers.
    txSignatures       :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txScripts          :: Map.Map ScriptHash (Versioned Script),
    -- ^ Scripts for all script credentials mentioned in this tx.
    txData             :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    txMetadata         :: Maybe BuiltinByteString
    -- ^ Metadata
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise, NFData)


instance Semigroup Tx where
    tx1 <> tx2 = Tx {
        txInputs = txInputs tx1 <> txInputs tx2,
        txReferenceInputs = txReferenceInputs tx1 <> txReferenceInputs tx2,
        txCollateralInputs = txCollateralInputs tx1 <> txCollateralInputs tx2,
        txOutputs = txOutputs tx1 <> txOutputs tx2,
        txReturnCollateral = txReturnCollateral tx1 <|> txReturnCollateral tx2,
        txTotalCollateral = txTotalCollateral tx1 <> txTotalCollateral tx2,
        txMint = txMint tx1 <> txMint tx2,
        txFee = txFee tx1 <> txFee tx2,
        txValidRange = txValidRange tx1 /\ txValidRange tx2,
        txMintingScripts = txMintingScripts tx1 <> txMintingScripts tx2,
        txSignatures = txSignatures tx1 <> txSignatures tx2,
        txData = txData tx1 <> txData tx2,
        txScripts = txScripts tx1 <> txScripts tx2,
        txWithdrawals = txWithdrawals tx1 <> txWithdrawals tx2,
        txCertificates = txCertificates tx1 <> txCertificates tx2,
        txMetadata = txMetadata tx1 <> txMetadata tx2
        }

instance Monoid Tx where
    mempty = Tx mempty mempty mempty mempty empty mempty mempty mempty top mempty mempty mempty mempty mempty mempty mempty

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The inputs of a transaction.
inputs :: L.Lens' Tx [TxInput]
inputs = L.lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The reference inputs of a transaction.
referenceInputs :: L.Lens' Tx [TxInput]
referenceInputs = L.lens g s where
    g = txReferenceInputs
    s tx i = tx { txReferenceInputs = i }

-- | The collateral inputs of a transaction for paying fees when validating the transaction fails.
collateralInputs :: L.Lens' Tx [TxInput]
collateralInputs = L.lens g s where
    g = txCollateralInputs
    s tx i = tx { txCollateralInputs = i }

-- | The outputs of a transaction.
outputs :: L.Lens' Tx [TxOut]
outputs = L.lens g s where
    g = txOutputs
    s tx o = tx { txOutputs = o }

returnCollateral :: L.Lens' Tx (Maybe TxOut)
returnCollateral = L.lens g s where
    g = txReturnCollateral
    s tx o = tx { txReturnCollateral = o }

totalCollateral :: L.Lens' Tx (Maybe Value)
totalCollateral = L.lens g s where
    g = txTotalCollateral
    s tx o = tx { txTotalCollateral = o }

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

mintScripts :: L.Lens' Tx (Map MintingPolicyHash Redeemer)
mintScripts = L.lens g s where
    g = txMintingScripts
    s tx fs = tx { txMintingScripts = fs }

scriptWitnesses :: L.Lens' Tx (Map ScriptHash (Versioned Script))
scriptWitnesses = L.lens g s where
    g = txScripts
    s tx fs = tx { txScripts = fs }

datumWitnesses :: L.Lens' Tx (Map DatumHash Datum)
datumWitnesses = L.lens g s where
    g = txData
    s tx dat = tx { txData = dat }

-- | The inputs of a transaction.
metadata :: L.Lens' Tx (Maybe BuiltinByteString)
metadata = L.lens g s where
    g = txMetadata
    s tx i = tx { txMetadata = i }

lookupSignature :: PubKey -> Tx -> Maybe Signature
lookupSignature s Tx{txSignatures} = Map.lookup s txSignatures

lookupDatum :: Tx -> DatumHash -> Maybe Datum
lookupDatum Tx{txData} h = Map.lookup h txData

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

outValue' :: L.Lens' TxOut (C.TxOutValue C.BabbageEra)
outValue' = L.lens
  (\(TxOut (C.TxOut _aie tov _tod _rs)) -> tov)
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
    i = map txInputRef txInputs
    ri = map txInputRef txReferenceInputs

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

type ReferenceScript = C.ReferenceScript C.BabbageEra

txOutReferenceScript :: TxOut -> ReferenceScript
txOutReferenceScript (TxOut (C.TxOut _aie _tov _tod rs)) = rs

outReferenceScript :: L.Lens' TxOut ReferenceScript
outReferenceScript = L.lens
  txOutReferenceScript
  (\(TxOut (C.TxOut aie tov tod _)) rs -> TxOut (C.TxOut aie tov tod rs))

lookupScript :: Map ScriptHash (Versioned Script) -> ScriptHash -> Maybe (Versioned Script)
lookupScript txScripts hash  = Map.lookup hash txScripts

lookupValidator :: Map ScriptHash (Versioned Script) -> ValidatorHash -> Maybe (Versioned Validator)
lookupValidator txScripts = (fmap . fmap) Validator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (ValidatorHash b) = ScriptHash b

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> [TxOutRef]
spentOutputs = map txInputRef . txInputs

-- | The transaction output references referenced by a transaction.
referencedOutputs :: Tx -> [TxOutRef]
referencedOutputs = map txInputRef . txReferenceInputs

lookupMintingPolicy :: Map ScriptHash (Versioned Script) -> MintingPolicyHash -> Maybe (Versioned MintingPolicy)
lookupMintingPolicy txScripts = (fmap . fmap) MintingPolicy . lookupScript txScripts . toScriptHash
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

deriving instance OpenApi.ToSchema Tx
deriving instance OpenApi.ToSchema TxInputType
deriving instance OpenApi.ToSchema TxInput
deriving instance OpenApi.ToSchema Withdrawal
deriving instance OpenApi.ToSchema Certificate

lookupStakeValidator :: Map ScriptHash (Versioned Script) -> StakeValidatorHash -> Maybe (Versioned StakeValidator)
lookupStakeValidator txScripts = (fmap . fmap) StakeValidator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (StakeValidatorHash b) = ScriptHash b

-- | Translate TxInput to old Plutus.V1.Ledger.Api TxIn taking script and datum witnesses from Tx.
fillTxInputWitnesses :: Tx -> TxInput -> TxIn
fillTxInputWitnesses tx (TxInput outRef _inType) = case _inType of
    TxConsumePublicKeyAddress -> TxIn outRef (Just ConsumePublicKeyAddress)
    TxConsumeSimpleScriptAddress -> TxIn outRef (Just ConsumeSimpleScriptAddress)
    TxScriptAddress redeemer (Left vlh) dh -> TxIn outRef $ do
        datum <- Map.lookup dh (txData tx)
        validator <- lookupValidator (txScripts tx) vlh
        Just $ ScriptAddress (Left validator) redeemer datum
    TxScriptAddress redeemer (Right ref) dh -> TxIn outRef $ do
        datum <- Map.lookup dh (txData tx)
        Just $ ScriptAddress (Right ref) redeemer datum

pubKeyTxInput :: TxOutRef -> TxInput
pubKeyTxInput outRef = TxInput outRef TxConsumePublicKeyAddress

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly. Doesn't alter txMint.
addMintingPolicy :: Versioned MintingPolicy -> Redeemer -> Tx -> Tx
addMintingPolicy vvl rd tx@Tx{txMintingScripts, txScripts} = tx
    {txMintingScripts = Map.insert mph rd txMintingScripts,
     txScripts = Map.insert (ScriptHash b) (fmap getMintingPolicy vvl) txScripts}
    where
        mph@(MintingPolicyHash b) = mintingPolicyHash vvl

-- | Add validator together with the redeemer and datum into txInputs, txData and txScripts accordingly.
addScriptTxInput :: TxOutRef -> Versioned Validator -> Redeemer -> Datum -> Tx -> Tx
addScriptTxInput outRef vl rd dt tx@Tx{txInputs, txScripts, txData} = tx
    {txInputs = TxInput outRef (TxScriptAddress rd (Left vlHash) dtHash) : txInputs,
     txScripts = Map.insert (ScriptHash b) (fmap getValidator vl) txScripts,
     txData = Map.insert dtHash dt txData}
    where
        dtHash = datumHash dt
        vlHash@(ValidatorHash b) = validatorHash vl

-- | Add script reference together with the redeemer and datum into txInputs and txData accordingly.
addReferenceTxInput :: TxOutRef -> Versioned TxOutRef -> Redeemer -> Datum -> Tx -> Tx
addReferenceTxInput outRef vref rd dt tx@Tx{txInputs, txData} = tx
    {txInputs = TxInput outRef (TxScriptAddress rd (Right vref) dtHash) : txInputs,
     txData = Map.insert dtHash dt txData}
    where
        dtHash = datumHash dt

txRedeemers :: Tx -> Map ScriptPurpose Redeemer
txRedeemers = (Map.mapKeys Spending . txSpendingRedeemers)
    <> (Map.mapKeys (Minting . mpsSymbol) . txMintingRedeemers)
    <> (Map.mapKeys (Rewarding . StakingHash)  . txRewardingRedeemers)
    <> (Map.mapKeys Certifying . txCertifyingRedeemers)

txSpendingRedeemers :: Tx -> Map TxOutRef Redeemer
txSpendingRedeemers Tx{txInputs} = flip execState Map.empty $ traverse_ extract txInputs where
    extract TxInput{txInputType=TxScriptAddress redeemer _ _, txInputRef} =
        modify' $ Map.insert txInputRef redeemer
    extract _ = return ()

txMintingRedeemers :: Tx -> Map MintingPolicyHash Redeemer
txMintingRedeemers Tx{txMintingScripts} = txMintingScripts

txRewardingRedeemers :: Tx -> Map Credential Redeemer
txRewardingRedeemers Tx{txWithdrawals} = flip execState Map.empty $ traverse_ f txWithdrawals where
    f (Withdrawal cred _ (Just rd)) = modify' $ Map.insert cred rd
    f _                             = return ()

txCertifyingRedeemers :: Tx -> Map DCert Redeemer
txCertifyingRedeemers Tx{txCertificates} = flip execState Map.empty $ traverse_ f txCertificates where
    f (Certificate dcert (Just rd)) = modify' $ Map.insert dcert rd
    f _                             = return ()

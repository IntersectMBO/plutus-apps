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
    , LedgerPlutusVersion(..)
    ) where

import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, encode)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Slot
import Ledger.Tx.Orphans ()
import Ledger.Tx.Orphans.V2 ()
import Plutus.ApiCommon (LedgerPlutusVersion (PlutusV1, PlutusV2))
import Plutus.V1.Ledger.Api (TxOutRef)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Scripts (DatumHash, Redeemer, ValidatorHash)
import Plutus.V1.Ledger.Tx hiding (TxIn (..), TxInType (..), inRef, inScripts, inType, pubKeyTxIn, pubKeyTxIns,
                            scriptTxIn, scriptTxIns)
import Plutus.V1.Ledger.Value as V
import PlutusTx.Lattice
import Prettyprinter (Pretty (..), hang, vsep, (<+>))


deriving instance Show LedgerPlutusVersion
deriving instance Generic LedgerPlutusVersion
deriving instance NFData LedgerPlutusVersion
deriving instance Serialise LedgerPlutusVersion
deriving instance ToJSON LedgerPlutusVersion
deriving instance FromJSON LedgerPlutusVersion

-- | The type of a transaction input. Contains redeemer if consumes a script.
data TxInputType =
      TxConsumeScriptAddress !LedgerPlutusVersion !Redeemer !ValidatorHash !DatumHash -- ^ A transaction input that consumes a script address with the given validator, redeemer, and datum.
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
                    TxConsumeScriptAddress redeemer _ _ ->
                        pretty redeemer
                    _ -> mempty
        in hang 2 $ vsep ["-" <+> pretty txInputRef, rest]

-- | The 'TxOutRef' spent by a transaction input.
inRef :: Lens' TxInput TxOutRef
inRef = lens txInputRef s where
    s txi r = txi { txInputRef = r }

-- | The type of a transaction input.
inType :: Lens' TxInput TxInputType
inType = lens txInputType s where
    s txi t = txi { txInputType = t }

-- | Validator, redeemer, and data scripts of a transaction input that spends a
--   "pay to script" output.
inScripts :: TxIn -> Maybe (LedgerPlutusVersion, Validator, Redeemer, Datum)
inScripts TxIn{ txInType = t } = case t of
    Just (ConsumeScriptAddress l v r d) -> Just (l, v, r, d)
    _                                   -> Nothing

-- | A transaction input that spends a "pay to public key" output, given the witness.
pubKeyTxIn :: TxOutRef -> TxIn
pubKeyTxIn r = TxIn r (Just ConsumePublicKeyAddress)

-- | A transaction input that spends a "pay to script" output, given witnesses.
scriptTxIn :: TxOutRef -> LedgerPlutusVersion -> Validator -> Redeemer -> Datum -> TxIn
scriptTxIn ref l v r d = TxIn ref . Just $ ConsumeScriptAddress l v r d

-- | Filter to get only the pubkey inputs.
pubKeyTxInputs :: Fold [TxInput] TxInput
pubKeyTxInputs = folding (filter (\TxInput{ txInputType = t } -> t == TxConsumePublicKeyAddress))

-- | Filter to get only the script inputs.
scriptTxInputs :: Fold [TxInput] TxInput
scriptTxInputs = (\x -> folding x) . filter $ \case
    TxInput{ txInputType = TxConsumeScriptAddress{} } -> True
    _                                                 -> False


-- | A Babbage-era transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs          :: [TxInput],
    -- ^ The inputs to this transaction.
    txReferenceInputs :: [TxInput],
    -- ^ The reference inputs to this transaction.
    txCollateral      :: [TxInput],
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs         :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint            :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee             :: !Value,
    -- ^ The fee for this transaction.
    txValidRange      :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintingScripts  :: Map MintingPolicyHash Redeemer,
    -- ^ The scripts that must be run to check minting conditions matched with their redeemers.
    txWithdrawals     :: [Withdrawal],
    -- ^ Withdrawals, contains redeemers.
    txCertificates    :: [Certificate],
    -- ^ Certificates, contains redeemers.
    txSignatures      :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txScripts         :: Map.Map ScriptHash Script,
    -- ^ Scripts for all script credentials mentioned in this tx.
    txData            :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    txMetadata        :: Maybe BuiltinByteString
    -- ^ Metadata
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
        txMintingScripts = txMintingScripts tx1 <> txMintingScripts tx2,
        txSignatures = txSignatures tx1 <> txSignatures tx2,
        txData = txData tx1 <> txData tx2,
        txScripts = txScripts tx1 <> txScripts tx2,
        txWithdrawals = txWithdrawals tx1 <> txWithdrawals tx2,
        txCertificates = txCertificates tx1 <> txCertificates tx2,
        txMetadata = txMetadata tx1 <> txMetadata tx2
        }

instance Monoid Tx where
    mempty = Tx mempty mempty mempty mempty mempty mempty top mempty mempty mempty mempty mempty mempty mempty

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The inputs of a transaction.
inputs :: Lens' Tx [TxInput]
inputs = lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The reference inputs of a transaction.
referenceInputs :: Lens' Tx [TxInput]
referenceInputs = lens g s where
    g = txReferenceInputs
    s tx i = tx { txReferenceInputs = i }

-- | The collateral inputs of a transaction for paying fees when validating the transaction fails.
collateralInputs :: Lens' Tx [TxInput]
collateralInputs = lens g s where
    g = txCollateral
    s tx i = tx { txCollateral = i }

-- | The outputs of a transaction.
outputs :: Lens' Tx [TxOut]
outputs = lens g s where
    g = txOutputs
    s tx o = tx { txOutputs = o }

-- | The validity range of a transaction.
validRange :: Lens' Tx SlotRange
validRange = lens g s where
    g = txValidRange
    s tx o = tx { txValidRange = o }

signatures :: Lens' Tx (Map PubKey Signature)
signatures = lens g s where
    g = txSignatures
    s tx sig = tx { txSignatures = sig }

fee :: Lens' Tx Value
fee = lens g s where
    g = txFee
    s tx v = tx { txFee = v }

mint :: Lens' Tx Value
mint = lens g s where
    g = txMint
    s tx v = tx { txMint = v }

mintScripts :: Lens' Tx (Map MintingPolicyHash Redeemer)
mintScripts = lens g s where
    g = txMintingScripts
    s tx fs = tx { txMintingScripts = fs }

scriptWitnesses :: Lens' Tx (Map ScriptHash Script)
scriptWitnesses = lens g s where
    g = txScripts
    s tx fs = tx { txScripts = fs }

datumWitnesses :: Lens' Tx (Map DatumHash Datum)
datumWitnesses = lens g s where
    g = txData
    s tx dat = tx { txData = dat }

-- | The inputs of a transaction.
metadata :: Lens' Tx (Maybe BuiltinByteString)
metadata = lens g s where
    g = txMetadata
    s tx i = tx { txMetadata = i }

lookupSignature :: PubKey -> Tx -> Maybe Signature
lookupSignature s Tx{txSignatures} = Map.lookup s txSignatures

lookupDatum :: Tx -> DatumHash -> Maybe Datum
lookupDatum Tx{txData} h = Map.lookup h txData

lookupRedeemer :: Tx -> RedeemerPtr -> Maybe Redeemer
lookupRedeemer tx p = Map.lookup p (txRedeemers tx)

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

lookupScript :: Map ScriptHash Script -> ScriptHash -> Maybe Script
lookupScript txScripts hash  = Map.lookup hash txScripts

lookupValidator :: Map ScriptHash Script -> ValidatorHash -> Maybe Validator
lookupValidator txScripts = fmap Validator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (ValidatorHash b) = ScriptHash b

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> [TxOutRef]
spentOutputs = map txInputRef . txInputs

-- | The transaction output references referenced by a transaction.
referencedOutputs :: Tx -> [TxOutRef]
referencedOutputs = map txInRef . txReferenceInputs

lookupMintingPolicy :: Map ScriptHash Script -> MintingPolicyHash -> Maybe MintingPolicy
lookupMintingPolicy txScripts = fmap MintingPolicy . lookupScript txScripts . toScriptHash
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

lookupStakeValidator :: Map ScriptHash Script -> StakeValidatorHash -> Maybe StakeValidator
lookupStakeValidator txScripts = fmap StakeValidator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (StakeValidatorHash b) = ScriptHash b

-- | Translate TxInput to TxIn taking script and datum witnesses from Tx.
fillTxInputWitnesses :: Tx -> TxInput -> TxIn
fillTxInputWitnesses tx (TxInput outRef _inType) = case _inType of
    TxConsumePublicKeyAddress -> TxIn outRef (Just ConsumePublicKeyAddress)
    TxConsumeSimpleScriptAddress -> TxIn outRef (Just ConsumeSimpleScriptAddress)
    TxConsumeScriptAddress redeemer vlh dh -> TxIn outRef $ do
        datum <- Map.lookup dh (txData tx)
        validator <- lookupValidator (txScripts tx) vlh
        Just $ ConsumeScriptAddress validator redeemer datum

pubKeyTxInput :: TxOutRef -> TxInput
pubKeyTxInput outRef = TxInput outRef TxConsumePublicKeyAddress

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly.
addMintingPolicy :: MintingPolicy -> Redeemer -> Tx -> Tx
addMintingPolicy vl@(MintingPolicy script) rd tx@Tx{txMintingScripts, txScripts} = tx
    {txMintingScripts = Map.insert mph rd txMintingScripts,
     txScripts = Map.insert (ScriptHash b) script txScripts}
    where
        mph@(MintingPolicyHash b) = mintingPolicyHash vl

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly.
addScriptTxInput :: TxOutRef -> LedgerPlutusVersion -> Validator -> Redeemer -> Datum -> Tx -> Tx
addScriptTxInput outRef version vl@(Validator script) rd dt tx@Tx{txInputs, txScripts, txData} = tx
    {txInputs = TxInput outRef (TxConsumeScriptAddress version rd vlHash dtHash) : txInputs,
     txScripts = Map.insert (ScriptHash b) script txScripts,
     txData = Map.insert dtHash dt txData}
    where
        dtHash = datumHash dt
        vlHash@(ValidatorHash b) = validatorHash vl

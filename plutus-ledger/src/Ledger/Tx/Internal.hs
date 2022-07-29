{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Ledger.Tx.Internal where

import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, encode)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.Slot
import Ledger.Tx.Orphans ()
import Ledger.Value (geq)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Tx
import PlutusTx.Lattice

-- | A Babbage era transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs          :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txReferenceInputs :: Set.Set TxIn,
    -- ^ The reference inputs to this transaction.
    txCollateral      :: Set.Set TxIn,
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
    txSignatures      :: Map.Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txRedeemers       :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    txData            :: Map.Map DatumHash Datum
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
inputs :: Lens' Tx (Set.Set TxIn)
inputs = lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The reference inputs of a transaction.
referenceInputs :: Lens' Tx (Set.Set TxIn)
referenceInputs = lens g s where
    g = txReferenceInputs
    s tx i = tx { txReferenceInputs = i }

-- | The collateral inputs of a transaction for paying fees when validating the transaction fails.
collateralInputs :: Lens' Tx (Set.Set TxIn)
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

signatures :: Lens' Tx (Map.Map PubKey Signature)
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

mintScripts :: Lens' Tx (Map.Map MintingPolicyHash MintingPolicy)
mintScripts = lens g s where
    g = txMintScripts
    s tx fs = tx { txMintScripts = fs }

redeemers :: Lens' Tx Redeemers
redeemers = lens g s where
    g = txRedeemers
    s tx reds = tx { txRedeemers = reds }

datumWitnesses :: Lens' Tx (Map.Map DatumHash Datum)
datumWitnesses = lens g s where
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
      nonNegative i = geq i mempty

-- | A babbage era transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs          :: Set.Set TxOutRef,
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedReferenceInputs :: Set.Set TxOutRef,
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
    i = Set.map txInRef txInputs
    ri = Set.map txInRef txReferenceInputs

-- | A 'TxOut' along with the 'Tx' it comes from, which may have additional information e.g.
-- the full data script that goes with the 'TxOut'.
data TxOutTx = TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)

txOutTxDatum :: TxOutTx -> Maybe Datum
txOutTxDatum (TxOutTx tx txOut) =
  case txOutDatum txOut of
    NoOutputDatum      -> Nothing
    OutputDatum da     -> Just da
    OutputDatumHash dh -> lookupDatum tx dh

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> Set.Set TxOutRef
spentOutputs = Set.map txInRef . txInputs

-- | The transaction output references referenced by a transaction.
referencedOutputs :: Tx -> Set.Set TxOutRef
referencedOutputs = Set.map txInRef . txReferenceInputs

-- | Update a map of unspent transaction outputs and signatures
--   for a failed transaction using its collateral inputs.
updateUtxoCollateral :: Tx -> Map.Map TxOutRef TxOut -> Map.Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.map txInRef . txCollateral $ tx)


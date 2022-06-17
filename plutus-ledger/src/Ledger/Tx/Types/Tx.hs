{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ledger.Tx.Types.Tx (
    Tx(..),
    inputs,
    collateralInputs,
    outputs,
    validRange,
    signatures,
    fee,
    mint,
    mintScripts,
    scriptWitnesses,
    datumWitnesses,
    metadata,
    pubKeyTxInputs,
    scriptTxInputs,
) where

import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, encode)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Slot
import Ledger.Tx.Orphans ()
import Ledger.Tx.Types.Certificate
import Ledger.Tx.Types.TxInput
import Ledger.Tx.Types.Withdrawal
import Plutus.V1.Ledger.Api (BuiltinByteString, TxOut)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value as V
import PlutusTx.Lattice

-- | A transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs         :: [TxInput],
    -- ^ The inputs to this transaction.
    txCollateral     :: [TxInput],
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs        :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint           :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee            :: !Value,
    -- ^ The fee for this transaction.
    txValidRange     :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintingScripts :: Map MintingPolicyHash Redeemer,
    -- ^ The scripts that must be run to check minting conditions matched with their redeemers.
    txWithdrawals    :: [Withdrawal],
    -- ^ Withdrawals, contains redeemers.
    txCertificates   :: [Certificate],
    -- ^ Certificates, contains redeemers.
    txSignatures     :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txScripts        :: Map.Map ScriptHash Script,
    -- ^ Scripts for all script credentials mentioned in this tx.
    txData           :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    txMetadata       :: Maybe BuiltinByteString
    -- ^ Metadata
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise, NFData)


instance Semigroup Tx where
    tx1 <> tx2 = Tx {
        txInputs = txInputs tx1 <> txInputs tx2,
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
    mempty = Tx mempty mempty mempty mempty mempty top mempty mempty mempty mempty mempty mempty mempty

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The inputs of a transaction.
inputs :: Lens' Tx [TxInput]
inputs = lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

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

-- | Filter to get only the pubkey inputs.
pubKeyTxInputs :: Fold [TxInput] TxInput
pubKeyTxInputs = folding (filter (\TxInput{ txInputType = t } -> t == TxConsumePublicKeyAddress))

-- | Filter to get only the script inputs.
scriptTxInputs :: Fold [TxInput] TxInput
scriptTxInputs = (\x -> folding x) . filter $ \case
    TxInput{ txInputType = TxConsumeScriptAddress{} } -> True
    _                                                 -> False


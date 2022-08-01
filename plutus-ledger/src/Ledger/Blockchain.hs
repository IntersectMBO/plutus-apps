{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Ledger.Blockchain (
    OnChainTx(..),
    _Valid,
    _Invalid,
    Block,
    BlockId(..),
    Blockchain,
    Context(..),
    eitherTx,
    consumableInputs,
    outputsProduced,
    transaction,
    out,
    value,
    unspentOutputsTx,
    spentOutputs,
    unspentOutputs,
    datumTxo,
    updateUtxo,
    txOutPubKey,
    pubKeyTxo,
    validValuesTx
    ) where

import Codec.Serialise (Serialise)
import Control.Lens (makePrisms)
import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (First (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import GHC.Generics (Generic)
import Ledger.Tx (CardanoTx, TxId, TxIn, TxOut, TxOutRef (..), getCardanoTxCollateralInputs, getCardanoTxId,
                  getCardanoTxInputs, getCardanoTxOutputs, spentOutputs, txOutDatum, txOutPubKey, txOutValue,
                  unspentOutputsTx, updateUtxo, updateUtxoCollateral, validValuesTx)
import Prettyprinter (Pretty (..), (<+>))

import Data.Either (fromRight)
import Data.OpenApi qualified as OpenApi
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value (Value)

-- | Block identifier (usually a hash)
newtype BlockId = BlockId { getBlockId :: BS.ByteString }
    deriving stock (Eq, Ord, Generic)

instance Show BlockId where
    show = Text.unpack . JSON.encodeByteString . getBlockId

instance ToJSON BlockId where
    toJSON = JSON.String . JSON.encodeByteString . getBlockId

instance FromJSON BlockId where
    parseJSON v = BlockId <$> JSON.decodeByteString v

instance OpenApi.ToSchema BlockId where
    declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @String)

instance Pretty BlockId where
    pretty (BlockId blockId) =
        "BlockId "
     <> pretty (fromRight (JSON.encodeByteString blockId) $ decodeUtf8' blockId)

-- | A transaction on the blockchain.
-- Invalid transactions are still put on the chain to be able to collect fees.
data OnChainTx = Invalid CardanoTx | Valid CardanoTx
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty OnChainTx where
    pretty = \case
        Invalid tx -> "Invalid:" <+> pretty tx
        Valid   tx -> "Valid:"   <+> pretty tx

-- | A block on the blockchain. This is just a list of transactions
-- following on from the chain so far.
type Block = [OnChainTx]
-- | A blockchain, which is just a list of blocks, starting with the newest.
type Blockchain = [Block]

eitherTx :: (CardanoTx -> r) -> (CardanoTx -> r) -> OnChainTx -> r
eitherTx ifInvalid _ (Invalid tx) = ifInvalid tx
eitherTx _ ifValid (Valid tx)     = ifValid tx

consumableInputs :: OnChainTx -> [TxIn]
consumableInputs = eitherTx getCardanoTxCollateralInputs getCardanoTxInputs

-- | Outputs added to the UTXO set by the 'OnChainTx'
outputsProduced :: OnChainTx -> [TxOut]
outputsProduced = eitherTx (const []) getCardanoTxOutputs

-- | Lookup a transaction in a 'Blockchain' by its id.
transaction :: Blockchain -> TxId -> Maybe OnChainTx
transaction bc tid = getFirst . foldMap (foldMap p) $ bc where
    p tx = if tid == eitherTx getCardanoTxId getCardanoTxId tx then First (Just tx) else mempty

-- | Determine the unspent output that an input refers to
out :: Blockchain -> TxOutRef -> Maybe TxOut
out bc o = do
    Valid t <- transaction bc (txOutRefId o)
    let i = txOutRefIdx o
    if fromIntegral (length (getCardanoTxOutputs t)) <= i
        then Nothing
        else Just $ getCardanoTxOutputs t !! fromIntegral i

-- | Determine the unspent value that a transaction output refers to.
value :: Blockchain -> TxOutRef -> Maybe Value
value bc o = txOutValue <$> out bc o

-- | Determine the data script that a transaction output refers to.
datumTxo :: Blockchain -> TxOutRef -> Maybe DatumHash
datumTxo bc o = txOutDatum =<< out bc o

-- | Determine the public key that locks a transaction output, if there is one.
pubKeyTxo :: Blockchain -> TxOutRef -> Maybe PubKeyHash
pubKeyTxo bc o = out bc o >>= txOutPubKey

-- | The unspent transaction outputs of the ledger as a whole.
unspentOutputs :: Blockchain -> Map TxOutRef TxOut
unspentOutputs = foldr (eitherTx updateUtxoCollateral updateUtxo) Map.empty . join

makePrisms ''OnChainTx

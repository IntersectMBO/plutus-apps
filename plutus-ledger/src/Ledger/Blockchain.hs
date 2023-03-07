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
    unOnChain,
    onChainTxIsValid,
    consumableInputs,
    outputsProduced,
    transaction,
    out,
    value,
    unspentOutputs,
    datumTxo,
    updateUtxo,
    txOutPubKey,
    pubKeyTxo
    ) where

import Codec.Serialise (Serialise)
import Control.Lens (makePrisms)
import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (First (..))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))

import Cardano.Api qualified as C
import Ledger.Tx (CardanoTx, TxId, TxIn, TxOut, TxOutRef (..), getCardanoTxCollateralInputs, getCardanoTxId,
                  getCardanoTxInputs, getCardanoTxProducedOutputs, getCardanoTxProducedReturnCollateral, txOutDatumHash,
                  txOutPubKey, txOutValue, updateUtxo, updateUtxoCollateral)
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts

-- | Block identifier (usually a hash)
newtype BlockId = BlockId { getBlockId :: BS.ByteString }
    deriving stock (Eq, Ord, Generic)

instance Show BlockId where
    show = Text.unpack . JSON.encodeByteString . getBlockId

instance ToJSON BlockId where
    toJSON = JSON.String . JSON.encodeByteString . getBlockId

instance FromJSON BlockId where
    parseJSON v = BlockId <$> JSON.decodeByteString v

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

unOnChain :: OnChainTx -> CardanoTx
unOnChain = eitherTx id id

onChainTxIsValid :: OnChainTx -> Bool
onChainTxIsValid = eitherTx (const False) (const True)

-- | Outputs consumed from the UTXO set by the 'OnChainTx'
consumableInputs :: OnChainTx -> [TxIn]
consumableInputs = eitherTx getCardanoTxCollateralInputs getCardanoTxInputs

-- | Outputs added to the UTXO set by the 'OnChainTx'
outputsProduced :: OnChainTx -> Map TxOutRef TxOut
outputsProduced = eitherTx getCardanoTxProducedReturnCollateral getCardanoTxProducedOutputs

-- | Lookup a transaction in a 'Blockchain' by its id.
transaction :: Blockchain -> TxId -> Maybe OnChainTx
transaction bc tid = getFirst . foldMap (foldMap p) $ bc where
    p tx = if tid == eitherTx getCardanoTxId getCardanoTxId tx then First (Just tx) else mempty

-- | Determine the unspent output that an input refers to
out :: Blockchain -> TxOutRef -> Maybe TxOut
out bc o = do
    tx <- transaction bc (txOutRefId o)
    Map.lookup o $ outputsProduced tx

-- | Determine the unspent value that a transaction output refers to.
value :: Blockchain -> TxOutRef -> Maybe C.Value
value bc o = txOutValue <$> out bc o

-- | Determine the data script that a transaction output refers to.
datumTxo :: Blockchain -> TxOutRef -> Maybe DatumHash
datumTxo bc o = txOutDatumHash =<< out bc o

-- | Determine the public key that locks a transaction output, if there is one.
pubKeyTxo :: Blockchain -> TxOutRef -> Maybe PubKeyHash
pubKeyTxo bc o = out bc o >>= txOutPubKey

-- | The unspent transaction outputs of the ledger as a whole.
unspentOutputs :: Blockchain -> Map TxOutRef TxOut
unspentOutputs = foldr (eitherTx updateUtxoCollateral updateUtxo) Map.empty . join

makePrisms ''OnChainTx

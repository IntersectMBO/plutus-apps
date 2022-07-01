{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}

module Plutus.Blockfrost.Queries (
    getTipBlockfrost
    , getDatumBlockfrost
    , getValidatorBlockfrost
    , getUnspentTxOutBlockfrost
    , getIsUtxoBlockfrost
    , getUtxoAtAddressBlockfrost
    ) where

import Control.Monad.Freer.Extras.Pagination (Page (..), PageQuery (..), PageSize (..))
import Data.Aeson (Value)
import Data.Functor ((<&>))

import Blockfrost.Client

-- ENDPOINTS

getTipBlockfrost :: MonadBlockfrost m => m Block
getTipBlockfrost = getLatestBlock

getDatumBlockfrost :: MonadBlockfrost m => DatumHash -> m Value
getDatumBlockfrost dHash = getScriptDatum dHash <&> _scriptDatumJsonValue

getValidatorBlockfrost :: MonadBlockfrost m => ScriptHash -> m ScriptCBOR
getValidatorBlockfrost = getScriptCBOR

getUnspentTxOutBlockfrost :: MonadBlockfrost m => (TxHash, Integer) -> m UtxoOutput
getUnspentTxOutBlockfrost (txHash, idx) = do
    utxos <- getTxUtxos txHash
    return $ _transactionUtxosOutputs utxos !! fromIntegral idx

getIsUtxoBlockfrost :: MonadBlockfrost m => (TxHash, Integer) -> m (Block, Bool)
getIsUtxoBlockfrost ref = do
    tip <- getTipBlockfrost
    isUtxo <- checkIsUtxo ref
    return (tip, isUtxo)

getUtxoAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m (Block, [AddressUtxo])
getUtxoAtAddressBlockfrost pq addr =  do
    tip <- getTipBlockfrost
    utxos <- getAddressUtxos' addr (paged 50 1) def
    return(tip, utxos)

-- UTIL FUNCTIONS

getAddressFromReference :: MonadBlockfrost m => (TxHash, Integer) -> m Address
getAddressFromReference (txHash, idx) = getTxUtxos txHash <&> (getAddress . _transactionUtxosOutputs)
  where
    getAddress :: [UtxoOutput] -> Address
    getAddress list = _utxoOutputAddress (list !! fromIntegral idx)

-- TODO: Support addresses with more than 100 utxos
checkIsUtxo :: MonadBlockfrost m => (TxHash, Integer) -> m Bool
checkIsUtxo ref@(txHash, idx) = getAddressFromReference ref >>= getAddressUtxos <&> any matchUtxo
  where
    matchUtxo :: AddressUtxo -> Bool
    matchUtxo AddressUtxo{..} = (txHash == _addressUtxoTxHash) && (idx == _addressUtxoOutputIndex)

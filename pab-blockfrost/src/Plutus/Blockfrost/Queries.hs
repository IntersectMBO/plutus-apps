{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Plutus.Blockfrost.Queries (
    getTipBlockfrost
    , getDatumBlockfrost
    , getValidatorBlockfrost
    , getTxOutBlockfrost
    , getUnspentTxOutBlockfrost
    , getIsUtxoBlockfrost
    , getUtxoAtAddressBlockfrost
    , getUnspentAtAddressBlockfrost
    , getDatumsAtAddressBlockfrost
    , getTxoAtAddressBlockfrost
    , getUtxoSetWithCurrency
    , getTxFromTxIdBlockfrost
    , getTxsFromTxIdsBlockfrost
    , defaultGetUtxo
    , defaultGetList
    , defaultIsUtxo
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (throwError)
import Control.Monad.Freer.Extras.Pagination (PageQuery (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Functor ((<&>))
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Blockfrost.Client

import Plutus.Blockfrost.Types


-- ENDPOINTS

getTipBlockfrost :: MonadBlockfrost m => m Block
getTipBlockfrost = getLatestBlock

getDatumBlockfrost :: MonadBlockfrost m => DatumHash -> m Value
getDatumBlockfrost dHash = getScriptDatum dHash <&> _scriptDatumJsonValue

getValidatorBlockfrost :: MonadBlockfrost m => ScriptHash -> m ScriptCBOR
getValidatorBlockfrost = getScriptCBOR

getTxOutBlockfrost :: (TxHash, Integer) -> BlockfrostClient UtxoOutput
getTxOutBlockfrost (tHash, idx) = do
    txos <- getTxUtxos tHash <&> _transactionUtxosOutputs
    case filterByIndex txos of
        []  -> throwError BlockfrostNotFound
        [x] -> pure x
        _   -> throwError $ BlockfrostError "Multiple UTxOs with the same index found!!!"
  where
    filterByIndex :: [UtxoOutput] -> [UtxoOutput]
    filterByIndex = filter ((==) idx . _utxoOutputOutputIndex)

getUnspentTxOutBlockfrost :: (TxHash, Integer) -> BlockfrostClient UtxoOutput
getUnspentTxOutBlockfrost ref = do
    txo <- getTxOutBlockfrost ref
    isUtxo <- checkIsUtxo ref
    if isUtxo then pure txo else throwError BlockfrostNotFound

getIsUtxoBlockfrost :: MonadBlockfrost m => (TxHash, Integer) -> m (Block, Bool)
getIsUtxoBlockfrost ref = do
    tip <- getTipBlockfrost
    isUtxo <- checkIsUtxo ref
    return (tip, isUtxo)

getUtxoAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m (Block, [AddressUtxo])
getUtxoAtAddressBlockfrost _ addr = do
    tip <- getTipBlockfrost
    utxos <- allPages (wrapperPaged getAddressUtxos' addr)
    return (tip, utxos)

getUnspentAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m [AddressUtxo]
getUnspentAtAddressBlockfrost _ addr = allPages (wrapperPaged getAddressUtxos' addr)

getDatumsAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m [Value]
getDatumsAtAddressBlockfrost p a = do
  -- get all txouts at address
  txos <- getTxoAtAddressBlockfrost p a
  let dhs = catMaybes $ _utxoInputDataHash <$> txos
  liftIO $ mapConcurrently getDatumBlockfrost dhs

getTxoAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m [UtxoInput]
getTxoAtAddressBlockfrost _ a = do
    addTxs <- allPages (wrapperPagedTx getAddressTransactions' a)
    txUtxos <- liftIO $ mapConcurrently (getTxUtxos . _addressTransactionTxHash) addTxs
    let txos = concatMap _transactionUtxosInputs txUtxos
    return $ filter ((==) a . _utxoInputAddress) txos

getUtxoSetWithCurrency :: MonadBlockfrost m => PageQuery a -> AssetId -> m (Block, [AddressUtxo])
getUtxoSetWithCurrency _ assetId = do
    tip <- getTipBlockfrost
    xs <- allPages (wrapperPaged getAssetAddresses' assetId)
    utxos <- liftIO $ mapConcurrently (flip getAddressUtxosAsset assetId . _assetAddressAddress) xs
    return (tip, concat utxos)

getTxFromTxIdBlockfrost :: MonadBlockfrost m => TxHash -> m TxResponse
getTxFromTxIdBlockfrost tHash = do
    specificTx <- getTx tHash
    txUtxos <- getTxUtxos tHash
    datumMap <- getAllTxDatums txUtxos
    redeemers <- getTxRedeemers tHash
    liftIO $ print $ "INPUTS: " ++ show (_transactionUtxosInputs txUtxos)
    let scriptHashes = map _transactionRedeemerScriptHash redeemers
    redeemersList <- liftIO $ mapConcurrently getRedeemersList redeemers
    scriptsList <- liftIO $ mapConcurrently (\sHash -> (unScriptHash sHash,) <$> getScriptCBOR sHash) scriptHashes
    return $ TxResponse { _txHash        = tHash
                        , _invalidBefore = _transactionInvalidBefore specificTx
                        , _invalidAfter  = _transactionInvalidHereafter specificTx
                        , _utxosInputs   = _transactionUtxosInputs txUtxos
                        , _utxosOutpus   = _transactionUtxosOutputs txUtxos
                        , _datumsMap     = datumMap
                        , _redeemersMap  = fromList redeemersList
                        , _scriptsMap    = fromList scriptsList
                        }
  where
    getRedeemersList :: TransactionRedeemer -> IO (Integer, (ValidationPurpose, ScriptDatum))
    getRedeemersList red = do
        let idx     = _transactionRedeemerTxIndex red
            purp = _transactionRedeemerPurpose red
            dh      = _transactionRedeemerDatumHash red
        dat <- getScriptDatum dh
        return (idx, (purp  , dat))

getTxsFromTxIdsBlockfrost :: MonadBlockfrost m => [TxHash] -> m [TxResponse]
getTxsFromTxIdsBlockfrost = liftIO . mapConcurrently getTxFromTxIdBlockfrost

-- UTIL FUNCTIONS

getAllTxDatums :: MonadBlockfrost m => TransactionUtxos -> m (Map Text ScriptDatum)
getAllTxDatums utxos = do
    let inps = map _utxoInputDataHash (_transactionUtxosInputs utxos)
        outs = map _utxoOutputDataHash (_transactionUtxosOutputs utxos)
        datumHashes = catMaybes (inps ++ outs)
    datumMap <- liftIO $ mapConcurrently (\dHash -> (unDatumHash dHash,) <$> getScriptDatum dHash) datumHashes
    return $ fromList datumMap

getAddressFromReference :: MonadBlockfrost m => (TxHash, Integer) -> m (Maybe Address)
getAddressFromReference (tHash, idx) = getTxUtxos tHash <&> (getAddress . _transactionUtxosOutputs)
  where
    getAddress :: [UtxoOutput] -> Maybe Address
    getAddress outs = case filter ((==) idx . _utxoOutputOutputIndex) outs of
        [out] -> Just $ _utxoOutputAddress out
        _     -> Nothing

checkIsUtxo :: MonadBlockfrost m => (TxHash, Integer) -> m Bool
checkIsUtxo ref@(tHash, idx) = getAddressFromReference ref >>= maybe (pure []) (allPages . wrapperPaged getAddressUtxos') <&> any matchUtxo
  where
    matchUtxo :: AddressUtxo -> Bool
    matchUtxo AddressUtxo{..} = (tHash == _addressUtxoTxHash) && (idx == _addressUtxoOutputIndex)

wrapperPaged :: (a -> Paged -> SortOrder -> m [b]) -> a -> Paged -> m [b]
wrapperPaged f a p = f a p def

wrapperPagedTx :: (a -> Paged -> SortOrder -> Maybe b -> Maybe b -> m [c]) -> a -> Paged -> m [c]
wrapperPagedTx f a p = f a p def Nothing Nothing

-- DEFAULT RESPONSES

defaultGetUtxo :: MonadBlockfrost m => m (Block, [AddressUtxo])
defaultGetUtxo = do
    tip <- getTipBlockfrost
    return (tip, [])

defaultGetList :: MonadBlockfrost m => m [a]
defaultGetList = return []

defaultIsUtxo :: MonadBlockfrost m => m (Block, Bool)
defaultIsUtxo = do
    tip <- getTipBlockfrost
    return(tip, False)

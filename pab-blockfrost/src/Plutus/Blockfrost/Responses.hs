{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.Blockfrost.Responses (
    processTip
    , processGetDatum
    , processGetValidator
    , processUnspentTxOut
    , processIsUtxo
    , processGetUtxos
    , processGetTxos
    , processUnspentTxOutSetAtAddress
    , processGetTxFromTxId
    , processGetTxsFromTxIds
    ) where

import Control.Monad.Freer.Extras.Pagination (Page (..), PageQuery (..))
import Data.Aeson qualified as JSON
import Data.Aeson.QQ
import Data.List (find)
import Data.Map as Map (Map, elems, fromList, keys, lookup, toList)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import Data.Text qualified as Text (drop)
import Text.Hex (decodeHex)

import Blockfrost.Client
import Cardano.Api hiding (Block, Script, ScriptDatum, ScriptHash, TxIn, TxOut)
import Cardano.Api.Shelley qualified as Shelley
import Ledger.Slot qualified as Ledger (Slot)
import Ledger.Tx (ChainIndexTxOut (..), RedeemerPtr (..), TxIn (..), TxOut (..), TxOutRef (..), pubKeyTxIn, scriptTxIn)
import Plutus.ChainIndex.Api (IsUtxoResponse (..), QueryResponse (..), TxosResponse (..), UtxosResponse (..))
import Plutus.ChainIndex.Types (BlockId (..), BlockNumber (..), ChainIndexTx (..), ChainIndexTxOutputs (..), Tip (..))
import Plutus.V1.Ledger.Address qualified as Ledger
import Plutus.V1.Ledger.Api (BuiltinByteString)
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Plutus.V1.Ledger.Scripts (Datum, MintingPolicy, Redeemer, StakeValidator, Validator (..), ValidatorHash (..))
import Plutus.V1.Ledger.Scripts qualified as Ledger (DatumHash, Script, ScriptHash (..))
import Plutus.V1.Ledger.Tx qualified
import Plutus.V1.Ledger.Value qualified as Ledger

import PlutusTx qualified

import Plutus.Blockfrost.Types
import Plutus.Blockfrost.Utils


class FromJSON a => PlutusValidator a where
  fromCBOR :: Text -> JSON.Result a

instance PlutusValidator Validator where
  fromCBOR t = JSON.fromJSON [aesonQQ|{"getValidator": #{t}}|]

instance PlutusValidator MintingPolicy where
  fromCBOR t = JSON.fromJSON [aesonQQ|{"getMintingPolicy": #{t}}|]

instance PlutusValidator StakeValidator where
  fromCBOR t = JSON.fromJSON [aesonQQ|{"getStakeValidator": #{t}}|]

instance PlutusValidator Ledger.Script where
  fromCBOR t = JSON.fromJSON [aesonQQ|#{t}|]

processGetDatum ::  PlutusTx.FromData a => Maybe JSON.Value -> IO (Maybe a)
processGetDatum sdt = case sdt of
    Nothing -> return Nothing
    Just res ->
        case Shelley.scriptDataFromJson Shelley.ScriptDataJsonDetailedSchema res of
            Right dec -> do
                case  decodeData dec of
                    Just x  -> return (Just x)
                    Nothing -> ioError (userError "Error in parser")
            Left err -> ioError (userError $ show err)
  where
    decodeData :: PlutusTx.FromData a => ScriptData -> Maybe a
    decodeData = PlutusTx.fromBuiltinData . PlutusTx.dataToBuiltinData . Shelley.toPlutusData

processTip :: Block -> IO Tip
processTip Block{..} = return $ Tip { tipSlot = slotNumber
                                    , tipBlockId = blockId
                                    , tipBlockNo = blockNo}
  where
    slotNumber :: Ledger.Slot
    slotNumber = fromIntegral $ unSlot $ fromJust _blockSlot

    blockNo :: BlockNumber
    blockNo = BlockNumber $ fromIntegral $ fromJust  _blockHeight

    blockId :: BlockId
    blockId =  BlockId $ fromJust $ decodeHex $ unBlockHash _blockHash

processGetValidator :: PlutusValidator a => Maybe ScriptCBOR -> IO (Maybe a)
processGetValidator Nothing = pure Nothing
processGetValidator (Just val) = buildResponse val
  where
    buildResponse :: PlutusValidator a => ScriptCBOR -> IO (Maybe a)
    buildResponse = maybe (pure Nothing) retFromCbor . _scriptCborCbor

    retFromCbor :: PlutusValidator a => Text -> IO (Maybe a)
    retFromCbor txt = case fromCBOR $ Text.drop 6 txt of
              JSON.Success a -> return $ Just a
              JSON.Error _   -> return Nothing

processUnspentTxOut :: Maybe UtxoOutput -> IO (Maybe ChainIndexTxOut)
processUnspentTxOut Nothing = pure Nothing
processUnspentTxOut (Just utxo) = buildResponse utxo
  where
    buildResponse :: UtxoOutput -> IO (Maybe ChainIndexTxOut)
    buildResponse utxoOut = case toPlutusAddress (_utxoOutputAddress utxoOut) of
              Left err   -> ioError (userError err)
              Right addr -> case Ledger.addressCredential addr of
                    PubKeyCredential _       -> return $ Just $ buildPublicKeyTxOut addr utxoOut
                    ScriptCredential valHash -> return $ Just $ buildScriptTxOut addr utxoOut valHash

    buildScriptTxOut :: Ledger.Address -> UtxoOutput -> ValidatorHash -> ChainIndexTxOut
    buildScriptTxOut addr utxoOut val = ScriptChainIndexTxOut { _ciTxOutAddress=addr
                                                              , _ciTxOutValue=utxoValue utxoOut
                                                              , _ciTxOutScriptDatum=(utxoDatumHash utxoOut, Nothing)
                                                              , _ciTxOutValidator=(val, Nothing)
                                                              }

    buildPublicKeyTxOut :: Ledger.Address -> UtxoOutput -> ChainIndexTxOut
    buildPublicKeyTxOut addr utxoOut = PublicKeyChainIndexTxOut { _ciTxOutAddress=addr
                                                                , _ciTxOutValue=utxoValue utxoOut
                                                                , _ciTxOutPublicKeyDatum=Nothing}

    utxoValue :: UtxoOutput -> Ledger.Value
    utxoValue = amountsToValue . _utxoOutputAmount

    utxoDatumHash :: UtxoOutput -> Ledger.DatumHash
    utxoDatumHash = textToDatumHash . unDatumHash . fromJust . _utxoOutputDataHash

processIsUtxo :: (Block, Bool) -> IO IsUtxoResponse
processIsUtxo (blockN, isUtxo) = do
    tip <- processTip blockN
    return $ IsUtxoResponse {currentTip=tip, isUtxo=isUtxo}

processGetUtxos :: PageQuery TxOutRef -> (Block, [AddressUtxo]) -> IO UtxosResponse
processGetUtxos pq (blockN, xs) = do
    tip <- processTip blockN
    return $ UtxosResponse {currentTip=tip, page=refPage}
  where
      refPage :: Page TxOutRef
      refPage = Page {currentPageQuery=pq
                  , nextPageQuery=Nothing
                  , pageItems=items
                  }

      items :: [TxOutRef]
      items = map utxoToRef xs

processGetTxos :: PageQuery TxOutRef -> [UtxoInput] -> IO TxosResponse
processGetTxos pq xs = return $ TxosResponse {paget=refPage}
  where
      refPage :: Page TxOutRef
      refPage = Page {currentPageQuery=pq
                  , nextPageQuery=Nothing
                  , pageItems=items
                  }

      items :: [TxOutRef]
      items = map txoToRef xs

processUnspentTxOutSetAtAddress ::
    PageQuery TxOutRef
    -> Credential
    -> [AddressUtxo]
    -> IO (QueryResponse [(TxOutRef, ChainIndexTxOut)])
processUnspentTxOutSetAtAddress _ cred xs =
  return $ QueryResponse {queryResult = items, nextQuery = Nothing}
  where
    items :: [(TxOutRef, ChainIndexTxOut)]
    items = map transform xs

    transform :: AddressUtxo -> (TxOutRef, ChainIndexTxOut)
    transform utxo = (utxoToRef utxo, buildResponse utxo)

    add :: Ledger.Address
    add = case cred of
      PubKeyCredential pkh     -> Ledger.pubKeyHashAddress pkh
      ScriptCredential valHash -> Ledger.scriptHashAddress valHash

    buildResponse :: AddressUtxo -> ChainIndexTxOut
    buildResponse utxo = case cred of
        PubKeyCredential _       -> buildPublicKeyTxOut add utxo
        ScriptCredential valHash -> buildScriptTxOut add utxo valHash

    buildScriptTxOut :: Ledger.Address -> AddressUtxo -> ValidatorHash -> ChainIndexTxOut
    buildScriptTxOut addr utxo val = ScriptChainIndexTxOut { _ciTxOutAddress=addr
                                                           , _ciTxOutValue=utxoValue utxo
                                                           , _ciTxOutScriptDatum=(utxoDatumHash utxo, Nothing)
                                                           , _ciTxOutValidator=(val, Nothing)
                                                           }

    buildPublicKeyTxOut :: Ledger.Address -> AddressUtxo -> ChainIndexTxOut
    buildPublicKeyTxOut addr utxo = PublicKeyChainIndexTxOut { _ciTxOutAddress=addr
                                                             , _ciTxOutValue=utxoValue utxo
                                                             , _ciTxOutPublicKeyDatum=Nothing
                                                             }

    utxoValue :: AddressUtxo -> Ledger.Value
    utxoValue = amountsToValue . _addressUtxoAmount

    utxoDatumHash :: AddressUtxo -> Ledger.DatumHash
    utxoDatumHash = textToDatumHash . fromJust . _addressUtxoDataHash

processGetTxFromTxId :: Maybe TxResponse -> IO (Maybe ChainIndexTx)
processGetTxFromTxId Nothing = pure Nothing
processGetTxFromTxId (Just TxResponse{..}) = do
    datums <- getAllDatumsMap _datumsMap
    redeemers <- getAllRedeemersMap _redeemersMap
    scripts <- getAllScriptsMap _scriptsMap
    txouts <- processTxOuts _utxosOutpus
    return $ Just ChainIndexTx { _citxTxId       = txHashToTxId _txHash
                               , _citxInputs     = processTxIn scripts redeemers datums _utxosInputs
                               , _citxOutputs    = txouts
                               , _citxValidRange = toPlutusSlotRange _invalidBefore _invalidAfter
                               , _citxData       = datums
                               , _citxRedeemers  = redeemers
                               , _citxScripts    = scripts
                               , _citxCardanoTx  = Nothing
                               }
  where
    processTxOuts :: [UtxoOutput] -> IO ChainIndexTxOutputs
    processTxOuts [] = return InvalidTx
    processTxOuts xs = ValidTx <$> mapM utxoOutputToTxOut xs

    utxoOutputToTxOut :: UtxoOutput -> IO TxOut
    utxoOutputToTxOut utxo = do
        addr <- either (ioError . userError) return (toPlutusAddress $ _utxoOutputAddress utxo)
        return $ TxOut { txOutAddress = addr
                       , txOutValue     = amountsToValue $ _utxoOutputAmount utxo
                       , txOutDatumHash = textToDatumHash . unDatumHash <$> _utxoOutputDataHash utxo
                       }

    getAllDatumsMap :: Map Text ScriptDatum -> IO (Map Ledger.DatumHash Datum)
    getAllDatumsMap datumMap = do
        let newKeys = map textToDatumHash $ keys datumMap
            newElems = map ((<$>) fromJust . processGetDatum . Just . _scriptDatumJsonValue) $ elems datumMap
        datElems <- sequence newElems
        return $ fromList $ zip newKeys datElems

    getAllRedeemersMap :: Map Integer (ValidationPurpose, ScriptDatum) -> IO Plutus.V1.Ledger.Tx.Redeemers
    getAllRedeemersMap datumMap = do
        let indexs = keys datumMap
            st     = map (toPlutusScriptTag . fst) (elems datumMap)
            redPtr = zipWith RedeemerPtr st indexs
            newElems = map ((<$>) fromJust . processGetDatum . Just . _scriptDatumJsonValue . snd) $ elems datumMap
        redElems <- sequence newElems
        return $ fromList $ zip redPtr redElems

    getAllScriptsMap :: Map Text ScriptCBOR -> IO (Map Ledger.ScriptHash Ledger.Script)
    getAllScriptsMap scriptsMap = do
        let newKeys = map textToScriptHash $ keys scriptsMap
            newElems = map ((<$>) fromJust . processGetValidator . Just) $ elems scriptsMap
        scriptElems <- sequence newElems
        return $ fromList $ zip newKeys scriptElems

    processTxIn ::
        Map Ledger.ScriptHash Ledger.Script
        -> Plutus.V1.Ledger.Tx.Redeemers
        -> Map Ledger.DatumHash Datum
        -> [UtxoInput]
        -> [TxIn]
    processTxIn scripts redeemers datums utxoIns = zipWith toPlutusTxIn utxoIns [0..]
      where
        toPlutusTxIn :: UtxoInput -> Integer -> TxIn
        toPlutusTxIn utxoIn idx = case addr utxoIn  of
                            ScriptCredential (ValidatorHash bbs)  -> scriptTxIn (txoToRef utxoIn) (val bbs) (red idx) (dat utxoIn)
                            PubKeyCredential _                    -> pubKeyTxIn $ txoToRef utxoIn

        addr :: UtxoInput -> Credential
        addr utxoIn = either (error "processTxIn: Error decoding address") Ledger.addressCredential (toPlutusAddress $ _utxoInputAddress utxoIn)

        red :: Integer -> Redeemer
        red idx = case find (\(RedeemerPtr _ i, _) -> idx == i) (Map.toList redeemers) of
              Nothing -> error $ "processTxIn: Can't find a redeemer that has the same index as this UtxoInput (" ++ show idx ++ ")"
              Just (_, redeemer) -> redeemer

        val :: BuiltinByteString -> Validator
        val bbs = Validator $ fromJust $ Map.lookup (Ledger.ScriptHash bbs) scripts

        dat :: UtxoInput -> Datum
        dat utxoIn = fromJust $ Map.lookup (textToDatumHash $ unDatumHash $ fromJust $ _utxoInputDataHash utxoIn) datums

processGetTxsFromTxIds :: [TxResponse] -> IO [ChainIndexTx]
processGetTxsFromTxIds txs = catMaybes <$> mapM (processGetTxFromTxId . Just) txs

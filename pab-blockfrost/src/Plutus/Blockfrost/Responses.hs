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
    , processDatumsAtAddress
    , processGetTxFromTxId
    , processGetTxsFromTxIds
    ) where

import Control.Monad.Extra (mapMaybeM)
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
import Ledger.Tx (DatumFromQuery (DatumUnknown), DecoratedTxOut (..), Language (PlutusV1), RedeemerPtr (..), TxIn (..),
                  TxOutRef (..), Versioned (Versioned, unversioned), mkPubkeyDecoratedTxOut, mkScriptDecoratedTxOut,
                  pubKeyTxIn, scriptTxIn)
import Plutus.ChainIndex.Api (IsUtxoResponse (..), QueryResponse (..), TxosResponse (..), UtxosResponse (..))
import Plutus.ChainIndex.Types (BlockId (..), BlockNumber (..), ChainIndexTx (..), ChainIndexTxOutputs (..), Tip (..))
import Plutus.V1.Ledger.Address qualified as Ledger
import Plutus.V1.Ledger.Api (BuiltinByteString, PubKeyHash)
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Plutus.V1.Ledger.Scripts (Datum, MintingPolicy, Redeemer, StakeValidator, Validator (..), ValidatorHash (..))
import Plutus.V1.Ledger.Scripts qualified as Ledger (DatumHash, Script, ScriptHash (..))
import Plutus.V1.Ledger.Tx qualified
import Plutus.V1.Ledger.Value qualified as Ledger

import PlutusTx qualified

import Control.Monad ((<=<))

import Plutus.Blockfrost.Types
import Plutus.Blockfrost.Utils
import Plutus.ChainIndex.Types qualified as CI
import Plutus.V2.Ledger.Api qualified as PV2


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

processGetValidator :: PlutusValidator a => Maybe ScriptCBOR -> IO (Maybe (Versioned a))
processGetValidator val = pure $ val >>= buildResponse
  where
    buildResponse :: PlutusValidator a => ScriptCBOR -> Maybe (Versioned a)
    buildResponse = retFromCbor <=< _scriptCborCbor

    retFromCbor :: PlutusValidator a => Text -> Maybe (Versioned a)
    retFromCbor txt = case fromCBOR $ Text.drop 6 txt of
              JSON.Success a -> Just (Versioned a PlutusV1)
              JSON.Error _   -> Nothing

processUnspentTxOut :: Maybe UtxoOutput -> IO (Maybe DecoratedTxOut)
processUnspentTxOut Nothing = pure Nothing
processUnspentTxOut (Just utxo) = buildResponse utxo
  where
    buildResponse :: UtxoOutput -> IO (Maybe DecoratedTxOut)
    buildResponse utxoOut = case toPlutusAddress (_utxoOutputAddress utxoOut) of
              Left err   -> ioError (userError err)
              Right addr -> case Ledger.addressCredential addr of
                    PubKeyCredential _ -> pure $ buildPublicKeyTxOut addr utxoOut
                    ScriptCredential _ -> pure $ buildScriptTxOut addr utxoOut

    buildScriptTxOut :: Ledger.Address -> UtxoOutput -> Maybe DecoratedTxOut
    buildScriptTxOut addr utxoOut = mkScriptDecoratedTxOut addr
                                                          (utxoValue utxoOut)
                                                          (utxoDatumHash utxoOut, DatumUnknown)
                                                          Nothing
                                                          Nothing

    buildPublicKeyTxOut :: Ledger.Address -> UtxoOutput -> Maybe DecoratedTxOut
    buildPublicKeyTxOut addr utxoOut = mkPubkeyDecoratedTxOut addr (utxoValue utxoOut) Nothing Nothing

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
    -> IO (QueryResponse [(TxOutRef, DecoratedTxOut)])
processUnspentTxOutSetAtAddress _ cred xs =
  return $ QueryResponse {queryResult = items, nextQuery = Nothing}
  where
    items :: [(TxOutRef, DecoratedTxOut)]
    items = map transform xs

    transform :: AddressUtxo -> (TxOutRef, DecoratedTxOut)
    transform utxo = (utxoToRef utxo, buildResponse utxo)

    buildResponse :: AddressUtxo -> DecoratedTxOut
    buildResponse utxo = case cred of
        PubKeyCredential pkh     -> buildPublicKeyTxOut pkh utxo
        ScriptCredential valHash -> buildScriptTxOut valHash utxo

    buildScriptTxOut :: ValidatorHash -> AddressUtxo -> DecoratedTxOut
    buildScriptTxOut valHash utxo = ScriptDecoratedTxOut { _decoratedTxOutValidatorHash=valHash
                                                        , _decoratedTxOutStakingCredential=Nothing
                                                        , _decoratedTxOutValue=utxoValue utxo
                                                        , _decoratedTxOutScriptDatum=(utxoDatumHash utxo, DatumUnknown)
                                                        , _decoratedTxOutValidator=Nothing
                                                        , _decoratedTxOutReferenceScript=Nothing
                                                        }

    buildPublicKeyTxOut :: PubKeyHash -> AddressUtxo -> DecoratedTxOut
    buildPublicKeyTxOut pkh utxo = PublicKeyDecoratedTxOut { _decoratedTxOutPubKeyHash=pkh
                                                          , _decoratedTxOutStakingCredential=Nothing
                                                           , _decoratedTxOutValue=utxoValue utxo
                                                           , _decoratedTxOutPubKeyDatum=Nothing
                                                           , _decoratedTxOutReferenceScript=Nothing
                                                           }

    utxoValue :: AddressUtxo -> Ledger.Value
    utxoValue = amountsToValue . _addressUtxoAmount

    utxoDatumHash :: AddressUtxo -> Ledger.DatumHash
    utxoDatumHash = textToDatumHash . fromJust . _addressUtxoDataHash


processDatumsAtAddress ::
  PlutusTx.FromData a
  => PageQuery TxOutRef
  -> Credential
  -> [JSON.Value]
  -> IO (QueryResponse [a])
processDatumsAtAddress _ _ xs = do
  items <- mapMaybeM (\d -> processGetDatum (Just d)) xs
  return $ QueryResponse {queryResult = items, nextQuery = Nothing}


processGetTxFromTxId :: Maybe TxResponse -> IO (Maybe ChainIndexTx)
processGetTxFromTxId Nothing = pure Nothing
processGetTxFromTxId (Just TxResponse{..}) = do
    datums <- getAllDatumsMap _datumsMap
    redeemers <- getAllRedeemersMap _redeemersMap
    scripts <- getAllScriptsMap _scriptsMap
    txouts <- processTxOuts _utxosOutpus
    return $ Just ChainIndexTx { _citxTxId       = txHashToTxId _txHash
                               , _citxInputs     = processTxIn (fmap unversioned scripts) redeemers datums _utxosInputs
                               , _citxOutputs    = txouts
                               , _citxValidRange = toPlutusSlotRange _invalidBefore _invalidAfter
                               , _citxData       = datums
                               , _citxRedeemers  = redeemers
                               , _citxScripts    = scripts
                               , _citxCardanoTx  = Nothing
                               }
  where
    processTxOuts :: [UtxoOutput] -> IO ChainIndexTxOutputs
    processTxOuts [] = pure $ InvalidTx Nothing
    processTxOuts xs = ValidTx <$> mapM utxoOutputToTxOut xs

    utxoOutputToTxOut :: UtxoOutput -> IO CI.ChainIndexTxOut
    utxoOutputToTxOut utxo = do
        addr <- either (ioError . userError) return (toPlutusAddress $ _utxoOutputAddress utxo)
        pure $ CI.ChainIndexTxOut
          { CI.citoAddress = addr
          , CI.citoValue     = amountsToValue $ _utxoOutputAmount utxo
          , CI.citoDatum = maybe PV2.NoOutputDatum PV2.OutputDatumHash (textToDatumHash . unDatumHash <$> _utxoOutputDataHash utxo)
          , CI.citoRefScript = CI.ReferenceScriptNone
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

    getAllScriptsMap :: Map Text ScriptCBOR -> IO (Map Ledger.ScriptHash (Versioned Ledger.Script))
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
                            ScriptCredential (ValidatorHash bbs)  -> scriptTxIn (txoToRef utxoIn) (Versioned (val bbs) PlutusV1) (red idx) (Just $ dat utxoIn)
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

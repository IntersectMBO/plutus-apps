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
    ) where

import Control.Monad.Freer.Extras.Pagination (PageQuery (..))
import Data.Aeson qualified as JSON
import Data.Aeson.QQ
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text (drop)

import Blockfrost.Client
import Cardano.Api hiding (Block)
import Cardano.Api.Shelley qualified as Shelley
import Ledger.Tx (ChainIndexTxOut (..), TxOutRef (..))
import Plutus.ChainIndex.Api (IsUtxoResponse, UtxosResponse)
import Plutus.ChainIndex.Types (Tip (..))
import Plutus.V1.Ledger.Address qualified as Ledger
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Plutus.V1.Ledger.Scripts (Datum, MintingPolicy, StakeValidator, Validator, ValidatorHash, unitDatum)
import Plutus.V1.Ledger.Scripts qualified as Ledger (DatumHash)
import Plutus.V1.Ledger.Value qualified as Ledger
import PlutusTx qualified

import Plutus.Blockfrost.Utils

class FromJSON a => PlutusValidator a where
  fromCBOR :: Text -> JSON.Result a

instance PlutusValidator Validator where
  fromCBOR t = JSON.fromJSON [aesonQQ|{"getValidator": #{t}}|]

instance PlutusValidator MintingPolicy where
  fromCBOR t = JSON.fromJSON [aesonQQ|{"getMintingPolicy": #{t}}|]

instance PlutusValidator StakeValidator where
  fromCBOR t = JSON.fromJSON [aesonQQ|{"getStakeValidator": #{t}}|]

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
processTip Block{..} = return ((fromSucceed $ JSON.fromJSON hcJSON) :: Tip)
  where
      slotNumber :: Slot
      slotNumber = fromJust _blockSlot

      blockNo :: Integer
      blockNo = fromJust _blockHeight

      blockId :: Text
      blockId = unBlockHash _blockHash

      hcJSON :: JSON.Value
      hcJSON = [aesonQQ|{
                "tag": "Tip",
                "tipBlockNo": #{blockNo},
                "tipBlockId": #{blockId},
                "tipSlot": {
                    "getSlot": #{slotNumber}
                }
                }
                |]

processGetValidator :: PlutusValidator a => Maybe ScriptCBOR -> IO (Maybe a)
processGetValidator = maybe (pure Nothing) buildResponse
  where
    buildResponse :: PlutusValidator a => ScriptCBOR -> IO (Maybe a)
    buildResponse = maybe (pure Nothing) retFromCbor . _scriptCborCbor

    retFromCbor :: PlutusValidator a => Text -> IO (Maybe a)
    retFromCbor = return . Just . fromSucceed . fromCBOR . Text.drop 6

processUnspentTxOut :: Integer -> Maybe [UtxoOutput] -> IO (Maybe ChainIndexTxOut)
processUnspentTxOut _ Nothing = pure Nothing
processUnspentTxOut idx (Just outs) =
  case filterByIndex of
    []  -> pure Nothing
    [x] -> buildResponse x
    _   -> ioError $ (userError "Multiple UTxOs with the same index found!!!")
  where
    filterByIndex :: [UtxoOutput]
    filterByIndex = filter ((==) idx . _utxoOutputOutputIndex) outs

    buildResponse :: UtxoOutput -> IO (Maybe ChainIndexTxOut)
    buildResponse utxo = case toPlutusAddress (_utxoOutputAddress utxo) of
              Left err   -> ioError (userError err)
              Right addr -> case Ledger.addressCredential addr of
                    PubKeyCredential _       -> return $ Just $ buildPublicKeyTxOut addr utxo
                    ScriptCredential valHash -> return $ Just $ buildScriptTxOut addr utxo valHash

    buildScriptTxOut :: Ledger.Address -> UtxoOutput -> ValidatorHash -> ChainIndexTxOut
    buildScriptTxOut addr utxo val = ScriptChainIndexTxOut { _ciTxOutAddress=addr
                                                           , _ciTxOutValidator=Left val
                                                           , _ciTxOutDatum=utxoDatumHash utxo
                                                           , _ciTxOutValue=utxoValue utxo
                                                           }

    buildPublicKeyTxOut :: Ledger.Address -> UtxoOutput -> ChainIndexTxOut
    buildPublicKeyTxOut addr utxo = PublicKeyChainIndexTxOut { _ciTxOutAddress=addr
                                                             , _ciTxOutValue=utxoValue utxo}

    utxoValue :: UtxoOutput -> Ledger.Value
    utxoValue = amountsToValue . _utxoOutputAmount

    utxoDatumHash :: UtxoOutput -> Either Ledger.DatumHash Datum
    utxoDatumHash = maybe (Right unitDatum) (Left . textToDatumHash . unDatumHash) . _utxoOutputDataHash

processIsUtxo :: (Block, Bool) -> IO IsUtxoResponse
processIsUtxo (blockN, isUtxo) = do
    tip <- processTip blockN
    return ((fromSucceed $ JSON.fromJSON $ hcJSON tip) :: IsUtxoResponse)
  where
    hcJSON :: Tip -> JSON.Value
    hcJSON tip = [aesonQQ|{
                "currentTip": #{tip},
                "isUtxo": #{isUtxo}
                }
                |]

processGetUtxos :: PageQuery TxOutRef -> (Block, [AddressUtxo]) ->  IO UtxosResponse
processGetUtxos pq (blockN, xs) = do
    tip <- processTip blockN
    return ((fromSucceed $ JSON.fromJSON $ hcJSON tip) :: UtxosResponse)
  where
      hcJSON :: Tip -> JSON.Value
      hcJSON tip = [aesonQQ| {
                  "currentTip": #{tip},
                  "page": {
                    "currentPageQuery" : #{pq},
                    "nextPageQuery": #{nextItem},
                    "pageItems": #{items}
                  }
                }
                 |]

      nextItem :: Maybe TxOutRef
      nextItem = Nothing

      items :: [TxOutRef]
      items = map transform xs

      transform :: AddressUtxo -> TxOutRef
      transform utxo = fromSucceed $ JSON.fromJSON $ txRefJSON utxo

      txRefJSON :: AddressUtxo -> JSON.Value
      txRefJSON utxo = [aesonQQ| {
                          "txOutRefId":{ "getTxId" : #{_addressUtxoTxHash utxo}},
                          "txOutRefIdx": #{_addressUtxoOutputIndex utxo}
                          }
                        |]

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
    ) where

import Data.Aeson qualified as JSON
import Data.Aeson.QQ
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text, unpack)
import Data.Text qualified as Text (drop)

import Blockfrost.Client
import Cardano.Api hiding (Block)
import Cardano.Api.Shelley qualified as Shelley
import Ledger.Tx (ChainIndexTxOut (..))
import Plutus.ChainIndex.Api (IsUtxoResponse)
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
                case PlutusTx.fromBuiltinData $ PlutusTx.dataToBuiltinData $ Shelley.toPlutusData dec of
                    Just x  -> print @String "Got the Datum hash" >> return (Just x)
                    Nothing -> print @String "Error in plutusTX" >> ioError (userError "Error in parser")
            Left err -> print ("getDatum(2): " ++ show err) >> ioError (userError $ show err)

processTip :: Block -> IO Tip
processTip Block{..} = return ((fromSucceed $ JSON.fromJSON hcJSON) :: Tip)
  where
      slot :: Slot
      slot = fromJust _blockSlot

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
                    "getSlot": #{slot}
                }
                }
                |]

processGetValidator :: PlutusValidator a => Maybe ScriptCBOR -> IO (Maybe a)
processGetValidator = maybe (pure Nothing) buildResponse
  where
    buildResponse :: PlutusValidator a => ScriptCBOR -> IO (Maybe a)
    buildResponse = maybe (pure Nothing) (return . Just . fromSucceed . fromCBOR . Text.drop 6) . _scriptCborCbor

processUnspentTxOut :: Maybe UtxoOutput -> IO (Maybe ChainIndexTxOut)
processUnspentTxOut = maybe (pure Nothing) buildResponse
  where
    buildResponse :: UtxoOutput -> IO (Maybe ChainIndexTxOut)
    buildResponse utxo = case toPlutusAddress (_utxoOutputAddress utxo) of
              Left err   -> ioError (userError err)
              Right addr -> case Ledger.addressCredential addr of
                        PubKeyCredential _       -> return $ Just $ buildPublicKeyTxOut addr utxo
                        ScriptCredential valHash -> return $ Just $ buildScriptTxOut addr utxo valHash

    buildScriptTxOut :: Ledger.Address -> UtxoOutput -> ValidatorHash -> ChainIndexTxOut
    buildScriptTxOut addr utxo val = ScriptChainIndexTxOut {_ciTxOutAddress=addr, _ciTxOutValidator=Left val, _ciTxOutDatum=utxoDatumHash utxo, _ciTxOutValue=utxoValue utxo}

    buildPublicKeyTxOut :: Ledger.Address -> UtxoOutput -> ChainIndexTxOut
    buildPublicKeyTxOut addr utxo = PublicKeyChainIndexTxOut { _ciTxOutAddress=addr, _ciTxOutValue=utxoValue utxo}

    utxoValue :: UtxoOutput -> Ledger.Value
    utxoValue = amountsToValue . _utxoOutputAmount

    utxoDatumHash :: UtxoOutput -> Either Ledger.DatumHash Datum
    utxoDatumHash = maybe (Right unitDatum) (Left . textToDatumHash . unDatumHash) . _utxoOutputDataHash

processIsUtxo :: (Block, Bool) -> IO IsUtxoResponse
processIsUtxo (block, isUtxo) = do
    tip <- processTip block
    return ((fromSucceed $ JSON.fromJSON $ hcJSON tip) :: IsUtxoResponse)
  where
    hcJSON :: Tip -> JSON.Value
    hcJSON tip = [aesonQQ|{
                "currentTip": #{tip},
                "isUtxo": #{isUtxo}
                }
                |]

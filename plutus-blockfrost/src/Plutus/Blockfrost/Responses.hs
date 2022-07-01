{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.Blockfrost.Responses (
    processTip
    , processGetDatum
    , processGetValidator
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
import Plutus.ChainIndex.Types (Tip (..))
import Plutus.V1.Ledger.Scripts (MintingPolicy, StakeValidator, Validator)
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

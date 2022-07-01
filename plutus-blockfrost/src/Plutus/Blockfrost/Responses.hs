{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.Blockfrost.Responses (
    processTip
    ) where

import Data.Aeson qualified as JSON
import Data.Aeson.QQ
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text, unpack)

import Blockfrost.Client
import Plutus.ChainIndex.Types (Tip (..))

import Plutus.Blockfrost.Utils

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

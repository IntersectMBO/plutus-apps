{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marconi.Index.TxConfirmationStatus
  ( -- * UtxoIndex
    TCSIndex
  , Event(..)
  , Depth(..)
  , open
  , Ix.insert
  , Ix.rewind
  ) where

import Cardano.Api (SlotNo (SlotNo))
import Control.Applicative ((<|>))
import Control.Lens.Operators ((^.))
import Data.Foldable (forM_)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid (Last (Last), Sum (Sum))
import Data.String (fromString)
import Database.SQLite.Simple (Only (Only), SQLData (SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField))
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Ledger (TxId)
import Plutus.ChainIndex.Types (BlockNumber (BlockNumber),
                                TxConfirmedState (TxConfirmedState, blockAdded, timesConfirmed, validity),
                                TxValidity (TxValid))

import Index.VSqlite (SqliteIndex)
import Index.VSqlite qualified as Ix

type Result = Maybe TxConfirmedState

data Event = Event
  { txId        :: TxId
  , blockNumber :: BlockNumber
  , slotNumber  :: SlotNo
  } deriving (Eq, Show, Generic)

type TCSIndex = SqliteIndex Event () TxId Result

newtype Depth = Depth Int

instance FromField TxId where
  fromField f = fromString <$> fromField f

instance ToField TxId where
  toField = SQLText . fromString . show

deriving newtype instance FromField BlockNumber

deriving newtype instance ToField BlockNumber

deriving newtype instance FromField SlotNo

deriving newtype instance ToField SlotNo

instance ToRow Event where
  toRow t = [ toField $ txId t
            , toField $ blockNumber t
            , toField $ slotNumber t
            ]

instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field

open
  :: FilePath
  -> Depth
  -> IO TCSIndex
open dbPath (Depth k) = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 4) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS tx_status (txId TEXT NOT NULL PRIMARY KEY, blockNo INT NOT NULL, slotNo INT NOT NULL)"
  pure ix

query
  :: TCSIndex
  -> TxId
  -> [Event]
  -> IO Result
query ix txId' events = (<|>) <$> searchInMemory
                            <*> searchOnDisk
  where
    searchInMemory :: IO Result
    searchInMemory = do
      buffered <- Ix.getBuffer $ ix ^. Ix.storage
      let event = find (\(_, e) -> txId e == txId')
                $ zip [1..] (events ++ buffered)
      pure $ event <&> \ (cs, Event _ bn _) ->
        TxConfirmedState { timesConfirmed = Sum    cs
                         , blockAdded     = Last $ Just bn
                         , validity       = Last $ Just TxValid
                         }

    searchOnDisk :: IO Result
    searchOnDisk = do
      txStatus :: [Event]
        <- SQL.query (ix ^. Ix.handle) "SELECT (txId, blockNo, slotNo) FROM tx_status WHERE txId = ?" (Only txId')
      if null txStatus
         then pure Nothing
         else let (Event _ bn _) = head txStatus
              in  pure . Just $
                TxConfirmedState { timesConfirmed = Sum 0
                                 , blockAdded     = Last $ Just bn
                                 , validity       = Last $ Just TxValid
                                 }

store :: TCSIndex -> IO ()
store ix = do
  events <- Ix.getEvents $ ix ^. Ix.storage
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let all' = buffer ++ events
      c    = ix ^. Ix.handle
  SQL.execute_ c "BEGIN"
  forM_ all' $
    SQL.execute c "INSERT INTO tx_status (txId, blockNumber, slotNo) VALUES (?, ?, ?)"
  SQL.execute_ c "COMMIT"

onInsert :: TCSIndex -> Event -> IO [()]
onInsert _ix _update = pure []

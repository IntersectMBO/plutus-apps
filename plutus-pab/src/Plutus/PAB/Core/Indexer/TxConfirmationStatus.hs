{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plutus.PAB.Core.Indexer.TxConfirmationStatus
  ( -- * TxConfirmationStatus
    TCSIndex
  , TxInfo(..)
  , Depth(..)
  , open
  , Ix.insert
  , Ix.rewind
  ) where

import Cardano.Api (SlotNo (SlotNo))
import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
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

import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix

type Result = Maybe TxConfirmedState

data TxInfo = TxInfo
  { txId        :: TxId
  , blockNumber :: BlockNumber
  , slotNumber  :: SlotNo
  } deriving (Eq, Show, Generic)

type TCSIndex = SqliteIndex Event () TxId Result
type Event = [TxInfo]

newtype Depth = Depth Int

instance FromField TxId where
  fromField f = fromString <$> fromField f

instance ToField TxId where
  toField = SQLText . fromString . show

deriving newtype instance FromField BlockNumber

deriving newtype instance ToField BlockNumber

deriving newtype instance FromField SlotNo

deriving newtype instance ToField SlotNo

instance ToRow TxInfo where
  toRow t = [ toField $ txId t
            , toField $ blockNumber t
            , toField $ slotNumber t
            ]

instance FromRow TxInfo where
  fromRow = TxInfo <$> field <*> field <*> field

open
  :: FilePath
  -> Depth
  -> IO TCSIndex
open dbPath (Depth k) = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
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
                $ zip [1..] (concat $ events ++ buffered)
      pure $ event <&> \ (cs, TxInfo _ bn _) ->
        TxConfirmedState { timesConfirmed = Sum    cs
                         , blockAdded     = Last $ Just bn
                         , validity       = Last $ Just TxValid
                         }

    searchOnDisk :: IO Result
    searchOnDisk = do
      txStatus :: [TxInfo]
        <- SQL.query (ix ^. Ix.handle) "SELECT txId, blockNo, slotNo FROM tx_status WHERE txId = ?" (Only txId')
      pure $ if null txStatus
              then Nothing
              else let (TxInfo _ bn _) = head txStatus
                   in Just $
                        TxConfirmedState { timesConfirmed = Sum 0
                                         , blockAdded     = Last $ Just bn
                                         , validity       = Last $ Just TxValid
                                         }

store :: TCSIndex -> IO ()
store ix = do
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let c   = ix ^. Ix.handle
  SQL.execute_ c "BEGIN"
  forM_ (concat buffer) $ \event -> do
    SQL.execute c "INSERT INTO tx_status (txId, blockNo, slotNo) VALUES (?, ?, ?)" event `catch` (\e -> putStrLn $ "SQL Exception: " <> show (txId event) <> " " <> show (e :: SomeException))
  SQL.execute_ c "COMMIT"

onInsert :: TCSIndex -> Event -> IO [()]
onInsert _ix _update = pure []

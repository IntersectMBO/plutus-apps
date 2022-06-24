{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marconi.Index.TxConfirmationStatus
  ( -- * UtxoIndex
    TCSIndex
  , Depth(..)
  , open
  , Ix.insert
  , Ix.rewind
  ) where

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
import Database.SQLite.Simple.ToField (ToField (toField))
import Ledger (TxId)
import Plutus.ChainIndex.Types (BlockNumber (BlockNumber),
                                TxConfirmedState (TxConfirmedState, blockAdded, timesConfirmed, validity),
                                TxValidity (TxValid))

import Index.VSqlite (SqliteIndex)
import Index.VSqlite qualified as Ix

type Result = Maybe TxConfirmedState
type Event  = [(TxId, BlockNumber)]

type TCSIndex = SqliteIndex Event () TxId Result

newtype Depth = Depth Int

instance FromField TxId where
  fromField f = fromString <$> fromField f

instance ToField TxId where
  toField = SQLText . fromString . show

deriving newtype instance FromField BlockNumber

deriving newtype instance ToField BlockNumber

open
  :: FilePath
  -> Depth
  -> IO TCSIndex
open dbPath (Depth k) = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 4) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS tx_status (txId TEXT NOT NULL PRIMARY KEY, blockNo INT NOT NULL)"
  pure ix

query
  :: TCSIndex
  -> TxId
  -> [Event]
  -> IO Result
query ix txId events = (<|>) <$> searchInMemory
                             <*> searchOnDisk
  where
    searchInMemory :: IO Result
    searchInMemory = do
      buffered <- Ix.getBuffer $ ix ^. Ix.storage
      let event = find ((== txId) . fst . snd)
                $ zip [1..] (concat $ events ++ buffered)
      pure $ event <&> \ (cs, (_, bn)) ->
        TxConfirmedState { timesConfirmed = Sum    cs
                         , blockAdded     = Last $ Just bn
                         , validity       = Last $ Just TxValid
                         }

    searchOnDisk :: IO Result
    searchOnDisk = do
      txStatus :: [(TxId, BlockNumber)]
        <- SQL.query (ix ^. Ix.handle) "SELECT (txId, blockNo) FROM tx_status WHERE txId = ?" (Only txId)
      if null txStatus
         then pure Nothing
         else let slotNo = snd $ head txStatus
              in  pure . Just $
                TxConfirmedState { timesConfirmed = Sum 0
                                 , blockAdded     = Last $ Just slotNo
                                 , validity       = Last $ Just TxValid
                                 }

store :: TCSIndex -> IO ()
store ix = do
  events <- Ix.getEvents $ ix ^. Ix.storage
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let all' = concat $ buffer ++ events
      c    = ix ^. Ix.handle
  SQL.execute_ c "BEGIN"
  forM_ all' $
    SQL.execute c "INSERT INTO tx_status (txId, blockNumber) VALUES (?, ?)"
  SQL.execute_ c "COMMIT"

onInsert :: TCSIndex -> Event -> IO [()]
onInsert _ix _update = pure []

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Index.Datum
  ( -- * DatumIndex
    DatumIndex
  , Event
  , Query
  , Result
  , Notification
  , open
  ) where

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative ((<|>))
import Control.Lens.Operators ((^.))
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (find)
import Data.Maybe (fromJust, listToMaybe)
import Data.String (fromString)
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLInteger, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.ToField (ToField (toField))

import Cardano.Api (SlotNo (SlotNo))
import Index.VSqlite (SqliteIndex)
import Index.VSqlite qualified as Ix
import Plutus.Script.Utils.V1.Scripts (Datum, DatumHash)

type Event        = [(SlotNo, (DatumHash, Datum))]
type Query        = DatumHash
type Result       = Maybe Datum
type Notification = ()

type DatumIndex = SqliteIndex Event Notification Query Result

instance FromField DatumHash where
  fromField f = fromString <$> fromField f

instance ToField DatumHash where
  toField = SQLText . fromString . show

instance FromField Datum where
  fromField f = fromField f >>=
    either (const $ returnError ConversionFailed f "Cannot deserialise datum.")
           pure
    . deserialiseOrFail

instance ToField Datum where
  toField = SQLBlob . toStrict . serialise

instance FromField SlotNo where
  fromField f = SlotNo <$> fromField f

instance ToField SlotNo where
  toField (SlotNo s) = SQLInteger $ fromIntegral s

open
  :: FilePath
  -> Int
  -> IO DatumIndex
open dbPath k = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS kv_datumhsh_datum (datumHash TEXT PRIMARY KEY, datum BLOB, slotNo INT)"
  pure ix

query :: DatumIndex -> Query -> [Event] -> IO Result
query ix hsh es = (memoryResult <|>) <$> sqliteResult
  where
    -- TODO: Consider buffered events
    memoryResult :: Result
    memoryResult = snd . snd <$> find ((== hsh) . fst . snd) (concat es)
    sqliteResult :: IO Result
    sqliteResult = do
      result <- SQL.query (ix ^. Ix.handle) "SELECT datum from kv_datumhsh_datum WHERE datumHash = ?" (Only hsh)
      pure $ head <$> listToMaybe result

store :: DatumIndex -> IO ()
store ix = do
  let c = ix ^. Ix.handle
  SQL.execute_ c "BEGIN"
  Ix.getBuffer (ix ^. Ix.storage) >>=
    mapM_ (SQL.execute c "INSERT INTO kv_datumhsh_datum (slotNo, datumHash, datum) VALUES (?, ?,?) ON CONFLICT(datumHash) DO UPDATE SET slotNo = ?") . map unpack . concat
  SQL.execute_ c "COMMIT"
  where
    unpack :: (SlotNo, (DatumHash, Datum)) -> (SlotNo, DatumHash, Datum, SlotNo)
    unpack (s, (h, d)) = (s, h, d, s)

onInsert :: DatumIndex -> Event -> IO [Notification]
onInsert _ _ = pure []

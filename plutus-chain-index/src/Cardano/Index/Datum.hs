{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Index.Datum
  ( -- * DatumIndex
    DatumIndex
  , open
  , Ix.insert
  ) where

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative ((<|>))
import Control.Lens.Operators ((^.))
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (find)
import Data.Maybe (fromJust, listToMaybe)
import Data.String (fromString)
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.ToField (ToField (toField))

import Index.VSqlite (SqliteIndex)
import Index.VSqlite qualified as Ix
import Plutus.Script.Utils.V1.Scripts (Datum, DatumHash)

type Event        = [(DatumHash, Datum)]
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

open
  :: FilePath
  -> Int
  -> IO DatumIndex
open dbPath k = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS kv_datumhsh_datum (datumHash TEXT PRIMARY KEY, datum BLOB)"
  pure ix

query :: DatumIndex -> Query -> [Event] -> IO Result
query ix hsh es = (memoryResult <|>) <$> sqliteResult
  where
    memoryResult :: Result
    memoryResult = snd <$> find ((== hsh) . fst) (concat es)
    sqliteResult :: IO Result
    sqliteResult = do
      result <- SQL.query (ix ^. Ix.handle) "SELECT datum from kv_datumhsh_datum WHERE datumHash = ?" (Only hsh)
      pure $ head <$> listToMaybe result

store :: DatumIndex -> IO ()
store ix = do
  let c = ix ^. Ix.handle
  SQL.execute_ c "BEGIN"
  Ix.getBuffer (ix ^. Ix.storage) >>=
    mapM_ (SQL.execute c "INSERT INTO kv_datumhash_datum (datumHash, datum) VALUES (?, ?)") . concat
  SQL.execute_ c "COMMIT"

onInsert :: DatumIndex -> Event -> IO [Notification]
onInsert _ _ = pure []

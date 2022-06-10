{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marconi.Index.Utxo
  ( -- * UtxoIndex
    UtxoIndex
  , Depth(..)
  , open
  , Ix.insert
  , Ix.rewind
  , UtxoUpdate(..)
  , inputs
  , outputs
  , slotNo
  , address
  , reference
  ) where

import Cardano.Api (SlotNo)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Lens.Operators ((&), (^.))
import Control.Lens.TH (makeLenses)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (foldl', forM_, toList)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Ledger (Address, TxId, TxOut, TxOutRef (TxOutRef, txOutRefId, txOutRefIdx))
import Ledger qualified as Ledger

import Index.VSqlite (SqliteIndex)
import Index.VSqlite qualified as Ix

data UtxoUpdate = UtxoUpdate
  { _inputs  :: !(Set TxOutRef)
  , _outputs :: ![(TxOut, TxOutRef)]
  , _slotNo  :: !SlotNo
  } deriving (Show)

$(makeLenses ''UtxoUpdate)

type Result = Maybe [TxOutRef]

type UtxoIndex = SqliteIndex UtxoUpdate () Address Result

newtype Depth = Depth Int

instance FromField Address where
  fromField f = fromField f >>=
    either (const $ returnError ConversionFailed f "Cannot deserialise address.")
           pure
    . deserialiseOrFail

instance ToField Address where
  toField = SQLBlob . toStrict . serialise

instance FromField TxId where
  fromField f = fromString <$> fromField f

instance ToField TxId where
  toField = SQLText . fromString . show

data UtxoRow = UtxoRow
  { _address   :: !Address
  , _reference :: !TxOutRef
  } deriving (Generic)

$(makeLenses ''UtxoRow)

instance FromRow UtxoRow where
  fromRow = UtxoRow <$> field <*> (TxOutRef <$> field <*> field)

instance ToRow UtxoRow where
  toRow u =  (toField $ u ^. address) : (toRow $ u ^. reference)

instance FromRow TxOutRef where
  fromRow = TxOutRef <$> field <*> field

instance ToRow TxOutRef where
  toRow r = [ toField $ txOutRefId  r
            , toField $ txOutRefIdx r
            ]

open
  :: FilePath
  -> Depth
  -> IO UtxoIndex
open dbPath (Depth k) = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 4) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS utxos (address TEXT NOT NULL, txId TEXT NOT NULL, inputIx INT NOT NULL)"
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS spent (txId TEXT NOT NULL, inputIx INT NOT NULL)"
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS utxo_address ON utxos (address)"
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS utxo_refs ON utxos (txId, inputIx)"
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS spent_refs ON spent (txId, inputIx)"
  pure ix

query
  :: UtxoIndex
  -> Address
  -> [UtxoUpdate]
  -> IO Result
query ix addr updates = do
  -- SELECT all utxos that have not been spent.
  storedUtxos <- SQL.query (ix ^. Ix.handle) "SELECT address, txId, inputIx FROM utxos LEFT JOIN spent ON utxos.txId = spent.txId AND utxos.inputIx = spent.inputIx WHERE utxos.txId IS NULL AND utxos.address = ?" (Only addr)
  let memoryUtxos  = concatMap (filter (onlyAt addr) . toRows) updates
      spentOutputs = foldl' Set.union Set.empty $ map _inputs updates
  pure . Just $ storedUtxos ++ memoryUtxos
              -- Remove utxos that have been spent (from memory db).
              & filter (\u -> not (_reference u `Set.member` spentOutputs))
              & map _reference

store :: UtxoIndex -> IO ()
store ix = do
  events <- Ix.getEvents $ ix ^. Ix.storage
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let all'  = buffer ++ events
      utxos = concatMap toRows all'
      spent = concatMap (toList . _inputs) all'
      c     = ix ^. Ix.handle

  SQL.execute_ c "BEGIN"
  forM_ utxos $
    SQL.execute c "INSERT INTO utxos (address, txId, inputIx) VALUES (?, ?, ?)"
  forM_ spent $
    SQL.execute c "INSERT INTO spent (txId, inputIx) VALUES (?, ?)"

  SQL.execute_ c "DELETE FROM utxos WHERE utxos.rowid IN (SELECT utxos.rowid FROM utxos LEFT JOIN spent on utxos.txId = spent.txId AND utxos.inputIx = spent.inputIx WHERE spent.txId IS NOT NULL)"
  SQL.execute_ c "COMMIT"

onInsert :: UtxoIndex -> UtxoUpdate -> IO [()]
onInsert _ix _update = pure []

toRows :: UtxoUpdate -> [UtxoRow]
toRows update = update ^. outputs
  & map (\(out, ref)  -> UtxoRow { _address   = Ledger.txOutAddress out
                                 , _reference = ref
                                 })

onlyAt :: Address -> UtxoRow -> Bool
onlyAt address' row = address' == (row ^. address)

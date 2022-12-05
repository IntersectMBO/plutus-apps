{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-
-- | Back-end support for Utxo Indexer

-- This module will create the SQL tables:
+ table: unspentTransactions
|---------+------+-------+-----------+-------+-------+------------------+--------------+------+-------------|
| Address | TxId | TxIdx | DatumHash | Datum | Value | InlineScriptHash | InlineScript | Slot | BlockNumber |
|---------+------+-------+-----------+-------+-------+------------------+--------------+------+-------------|

+ table: spent
  |------+------|
  | txId | txIx |
  |------+------|

-}
module Marconi.Index.Utxo
    ( eventAtAddress
    , Utxo (..)
    , utxoAddress
    , utxoEventSlotNo
    , UtxoEvent (..)
    , utxoEventUtxos
    , UtxoRow (..)
    , utxoRowUtxo
    , C.BlockNo (..)
    , C.SlotNo (..)
    , Depth (..)
    , Result
    , toRows
    , addressFilteredRows
    , toAddr
    , UtxoIndex
    , open
    , query
    , queryPlusVolatile
    ) where

import Cardano.Binary (fromCBOR, toCBOR)
import Codec.Serialise (Serialise (encode), deserialiseOrFail, serialise)
import Codec.Serialise.Class (Serialise (decode))
import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket_)
import Control.Lens (filtered, folded, traversed)
import Control.Lens.Operators ((%~), (&), (^.), (^..))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.List (union)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLInteger, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import System.Random.MWC (createSystemRandom, uniformR)
import Text.ParserCombinators.Parsec (parse)
import Text.RawString.QQ (r)

import Cardano.Api qualified as C
import Marconi.Types (CurrentEra)
import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix

data Utxo                       = Utxo
    { _utxoAddress          :: !C.AddressAny
    , _utxoTxId             :: !C.TxId
    , _utxoTxIx             :: !C.TxIx
    , _utxoDatum            :: Maybe C.ScriptData
    , _utxoDatumHash        :: Maybe (C.Hash C.ScriptData)
    , _utxoValue            :: C.Value
    , _utxoInlineScript     :: Maybe ByteString
    , _utxoInlineScriptHash :: Maybe (C.ScriptHash)
    } deriving (Show, Generic)

$(makeLenses ''Utxo)

instance Eq Utxo where
    u1 == u2 = (_utxoTxId u1) == (_utxoTxId u2)

instance Ord Utxo where
    compare u1 u2 = compare (_utxoTxId u1) (_utxoTxId u2)

data UtxoEvent          = UtxoEvent
    { _utxoEventUtxos   :: [Utxo]
    , _utxoEventInputs  :: !(Set.Set C.TxIn)
    , _utxoEventSlotNo  :: !C.SlotNo
    , _utxoEventBlockNo :: !C.BlockNo
    } deriving (Show, Eq)

$(makeLenses ''UtxoEvent)

data UtxoRow            = UtxoRow
    { _utxoRowUtxo    :: Utxo
    , _utxoRowSlotNo  :: !C.SlotNo
    , _utxoRowBlockNo :: !C.BlockNo
    } deriving (Show, Eq, Ord, Generic)

$(makeLenses ''UtxoRow)

type Result = Maybe [UtxoRow]

type Notification = ()

type UtxoIndex
    = SqliteIndex
      UtxoEvent
      Notification
      C.AddressAny
      Result

newtype Depth = Depth Int

instance FromRow C.TxIn where
    fromRow = C.TxIn <$> field <*> field
instance ToRow C.TxIn where
    toRow (C.TxIn txid txix) = toRow (txid, txix)
instance ToRow UtxoRow where
    toRow u = toRow (
        (u ^. utxoRowUtxo . utxoAddress)
        , (u ^. utxoRowUtxo . utxoTxId)
        , (u ^. utxoRowUtxo . utxoTxIx)
        , (u ^. utxoRowUtxo . utxoDatum)
        , (u ^. utxoRowUtxo . utxoDatumHash)
        , (u ^. utxoRowUtxo . utxoValue)
        , (u ^. utxoRowUtxo . utxoInlineScript)
        , (u ^. utxoRowUtxo . utxoInlineScriptHash)
        , (u ^. utxoRowSlotNo)
        , (u ^. utxoRowBlockNo))

instance FromRow UtxoRow where
    fromRow
        = UtxoRow
        <$> (Utxo
             <$> field
             <*> field
             <*> field
             <*> field
             <*> field
             <*> field
             <*> field
             <*> field )
        <*> field
        <*> field

instance FromField C.AddressAny where
  fromField f = fromField f >>=
      maybe (returnError ConversionFailed f "Cannot deserialise address.")
          pure . C.deserialiseFromRawBytes C.AsAddressAny

instance ToField C.AddressAny where
  toField = SQLBlob . C.serialiseToRawBytes

instance FromField C.TxId where
  fromField f = fromField f >>=
      maybe (returnError ConversionFailed f "Cannot deserialise TxId.")
          pure . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

instance ToField C.TxId where
  toField = SQLBlob . C.serialiseToRawBytes

instance FromField C.TxIx where
  fromField = fmap C.TxIx . fromField

instance ToField C.TxIx where
  toField (C.TxIx i) = SQLInteger $ fromIntegral i

instance FromField (C.Hash C.ScriptData) where
    fromField f = fromField f >>=
        maybe (returnError ConversionFailed f "Cannot deserialise scriptDataHash.")
          pure . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

instance ToField (C.Hash C.ScriptData) where
  toField = SQLBlob . C.serialiseToRawBytes

instance Serialise C.ScriptData where
  encode = toCBOR
  decode = fromCBOR

instance FromField C.ScriptData where
  fromField f = fromField f >>=
    either (const $ returnError ConversionFailed f "Cannot deserialise scriptdata.")
    pure . deserialiseOrFail

instance ToField C.ScriptData where
  toField = SQLBlob . toStrict . serialise

instance ToField C.Value where
    toField  = SQLText . C.renderValue

instance FromField C.Value where
    fromField f = fromField f >>=
        either (const $ returnError ConversionFailed f "Cannot deserialise value.") pure
        . (parse C.parseValue "")

instance ToField C.ScriptHash where
    toField = SQLBlob . C.serialiseToRawBytes

instance FromField C.ScriptHash where
    fromField f = fromField f >>=
        maybe (returnError ConversionFailed f "Cannot deserialise scriptDataHash.")
          pure . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)


instance FromField C.SlotNo where
  fromField f = C.SlotNo <$> fromField f

instance ToField C.SlotNo where
  toField (C.SlotNo s) = SQLInteger $ fromIntegral s

instance FromField C.BlockNo where
  fromField f = C.BlockNo <$> fromField f

instance ToField C.BlockNo where
  toField (C.BlockNo s) = SQLInteger $ fromIntegral s

open
  :: FilePath  -- ^ sqlite file path
  -> Depth
  -> IO UtxoIndex
open dbPath (Depth k) = do
  -- The second parameter ((k + 1) * 2) specifies the amount of events that are buffered.
  -- The larger the number, the more RAM the indexer uses. However, we get improved SQL
  -- queries due to batching more events together.
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let conn = ix ^. Ix.handle
  SQL.execute_ conn "DROP TABLE IF EXISTS unspent_transactions"
  SQL.execute_ conn "DROP TABLE IF EXISTS spent"
  SQL.execute_ conn [r|CREATE TABLE IF NOT EXISTS unspent_transactions
                            (address TEXT NOT NULL
                            ,txId TEXT NOT NULL
                            , txIx INT NOT NULL
                            , datum BLOB
                            , datumHash BLOB
                            , value TEXT
                            , inLineScript BLOB
                            , inLineScriptHash BLOB
                            , slotNo INT
                            , blockNo INT)|]
  SQL.execute_ conn [r|CREATE TABLE IF NOT EXISTS spent
                            (txId TEXT NOT NULL
                            , txIx INT NOT NULL)|]
  SQL.execute_ conn [r|CREATE INDEX IF NOT EXISTS
                      unspent_transaction_address
                      ON unspent_transactions (address)|]
  SQL.execute_ conn [r|CREATE UNIQUE INDEX IF NOT EXISTS
                      unspent_transaction_txid ON
                      unspent_transactions (txId)|]
  pure ix

eventAtAddress :: C.AddressAny -> UtxoEvent -> [UtxoEvent]
eventAtAddress addr event =
    let
        utxosAtAddress :: [Utxo]
        utxosAtAddress = event ^. utxoEventUtxos ^.. folded . filtered (\u -> (u ^. utxoAddress ) == addr)
    in
        if null utxosAtAddress  then []
        else [event { _utxoEventUtxos = utxosAtAddress }]

findByAddress :: C.AddressAny -> [UtxoEvent] -> [UtxoEvent]
findByAddress addr = concatMap (eventAtAddress addr)

-- | remove spent transactions
rmSpentUtxos :: UtxoEvent -> UtxoEvent
rmSpentUtxos event =
    event & utxoEventUtxos %~ (f (event ^. utxoEventInputs) )
    where
        f :: (Set.Set C.TxIn) -> [Utxo] -> [Utxo]
        f txIns utxos = filter (not . isUtxoSpent txIns) utxos
        isUtxoSpent :: (Set.Set C.TxIn) -> Utxo -> Bool
        isUtxoSpent txIns u = ( C.TxIn (u ^. utxoTxId)(u ^. utxoTxIx)) `Set.member` txIns

-- | convert utoEvents to utxoRows
toRows :: UtxoEvent -> [UtxoRow]
toRows event =  event ^. utxoEventUtxos & traversed %~ f
    where
        f :: Utxo -> UtxoRow
        f  u = UtxoRow u (event ^. utxoEventSlotNo ) (event ^. utxoEventBlockNo)

-- | only store rows in the address list.
addressFilteredRows :: C.AddressAny -> [UtxoEvent] -> [UtxoRow]
addressFilteredRows addr = (concatMap toRows ) . findByAddress addr

-- | Query the data stored in the indexer
-- Quries SQL + buffered data, where buffered data is the data that will be batched to SQL
query
  :: UtxoIndex                  -- ^ in-memory indexer
  -> C.AddressAny               -- ^ Address to filter for
  -> [UtxoEvent]                -- ^ volatile events that may be rollbacked
  -> IO Result                  -- ^ search results
query ix addr volatiles = do
  diskStored <-
      SQL.query
        (ix ^. Ix.handle)
        [r|SELECT u.address
            , u.txId
            , u.txIx
            , u.datum
            , u.datumHash
            , u.value
            , u.inLineScriptHash
            , u.inLineScriptHash
            , u.slotNo, u.blockNo
       FROM unspent_transactions u
       LEFT JOIN spent s ON u.txId = s.txId
            AND u.txIx = s.txIx
       WHERE u.address = ?|]
        -- "SELECT u.address, u.txId, u.txIx, u.datum, u.datumHash, u.value, u.inLineScriptHash, u.inLineScriptHash, u.slotNo, u.blockNo FROM unspent_transactions u LEFT JOIN spent s ON u.txId = s.txId AND u.txIx = s.txIx WHERE u.address = ?"
       (Only addr) :: IO[UtxoRow]
  buffered <- Ix.getBuffer $ ix ^. Ix.storage :: IO [UtxoEvent]
  let events = volatiles ++ buffered
  pure . Just $
      ( concatMap toRows . fmap rmSpentUtxos . (findByAddress addr) $ events)
      `union`
      diskStored

-- | Query the data stored in the indexer as a whole from:
    -- + volatile  : in-memory, datat that may rollback
    -- + diskStore : on-disk
    -- + buffered  : in-memeoy, data that will flush to storage
queryPlusVolatile
  :: UtxoIndex                  -- ^ in-memory indexer
  -> C.AddressAny               -- ^ Address to filter for
  -> IO Result                  -- ^ search results
queryPlusVolatile ix addr =
  Ix.getEvents (ix ^. Ix.storage)  >>= query ix addr

onInsert :: UtxoIndex -> UtxoEvent -> IO [Notification]
onInsert  _ _ =  pure []

store :: UtxoIndex -> IO ()
store ix = do
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let rows =  (concatMap toRows) $  buffer
      spent = concatMap (Set.toList . _utxoEventInputs) buffer
      conn = ix ^. Ix.handle
  bracket_
      (SQL.execute_ conn "BEGIN")
      (SQL.execute_ conn "COMMIT")
      ( concurrently_
            (unless (null rows)
                (SQL.executeMany conn
                "INSERT OR REPLACE INTO unspent_transactions (address, txId, txIx, datum, datumHash, value, inlineScript, inlineScriptHash, slotNo, blockNo) VALUES (?,?,?,?,?,?,?,?,?,?)"
                rows))
            (unless (null spent)
                (SQL.executeMany conn
                "INSERT OR REPLACE INTO spent (txId, txIx) VALUES (?, ?)"
                spent))
      )
  -- We want to perform vacuum about once every 100 * buffer ((k + 1) * 2)
  rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
  when (rndCheck == 42) $ do
    SQL.execute_ conn "DELETE FROM unspent_transactions WHERE unspent_transactions.rowid IN (SELECT unspent_transactions.rowid FROM unspent_transactions LEFT JOIN spent on unspent_transactions.txId = spent.txId AND unspent_transactions.txIx = spent.txIx WHERE spent.txId IS NOT NULL)"
    SQL.execute_ conn "VACUUM"

-- convert to AddressAny from address in any valid cardano era
toAddr :: C.AddressInEra CurrentEra -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

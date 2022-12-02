{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-
 Address
| TxId
| TxIdx
| DatumHash
| Datum
| Value
| InlineScriptHash
| InlineScript
| Slot
| BlockNumber
| transactionIndexWithinTheBlock (-}
module Marconi.Index.Utxos where
    -- ( eventAtAddress
    -- , Utxo (..)
    -- , UtxoEvent (..)
    -- , UtxoRow (..)
    -- , C.BlockNo (..)
    -- , C.SlotNo (..)
    -- , Depth (..)
    -- , Result
    -- , toRows
    -- , addressFilteredRows
    -- , toAddr
    -- , UtxoIndex
    -- , HasUtxo (..)
    -- , HasUtxoRow (..)
    -- , HasUtxoEvent (..)
    -- , open
    --                       ) whe re

import Cardano.Api qualified as C
import Control.Concurrent.Async (concurrently_)
import Control.Lens (filtered, folded, traversed)
import Control.Lens.Operators ((%~), (&), (^.), (^..))
import Control.Lens.TH (makeLenses)

import Cardano.Binary (fromCBOR, toCBOR)
import Codec.Serialise (Serialise (encode), deserialiseOrFail, serialise)
import Codec.Serialise.Class (Serialise (decode))
import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson qualified
import Data.ByteString.Lazy (toStrict)
import Data.List (union)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text (pack)
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLInteger, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Marconi.Types (CurrentEra)
import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix
import Text.ParserCombinators.Parsec (parse)

data Utxo               = Utxo
    { _utxoAddress   :: !C.AddressAny
    , _utxoTxId      :: !C.TxId
    , _utxoTxIx      :: !C.TxIx
    , _utxoDatum     :: Maybe C.ScriptData
    , _utxoDatumHash :: Maybe (C.Hash C.ScriptData)
    , _utxoValue     :: C.Value
    -- , _inlineScriptHash    :: Maybe (C.Hash (C.ScriptDatum C.WitCtxTxIn))
    -- , _inlineScript        :: Maybe (C.ScriptDatum C.WitCtxTxIn)
    } deriving (Show, Generic)

$(makeLenses ''Utxo)

instance ToJSON C.AddressAny where
    toJSON = Data.Aeson.String . C.serialiseAddress

instance ToJSON C.ScriptData where
    toJSON = Data.Aeson.String . pack . show

instance ToJSON Utxo


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

instance ToJSON C.BlockNo

instance ToJSON UtxoRow

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
             <*> field )
        <*> field
        <*> field

instance ToField C.Value where
    toField  = SQLText . C.renderValue
    -- toField  = SQLText . pack . show

instance FromField C.Value where
    fromField f = fromField f >>=
        either (const $ returnError ConversionFailed f "Cannot deserialise value.") pure
        . (parse C.parseValue "")

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

instance FromField C.SlotNo where
  fromField f = C.SlotNo <$> fromField f

instance ToField C.SlotNo where
  toField (C.SlotNo s) = SQLInteger $ fromIntegral s

instance FromField C.BlockNo where
  fromField f = C.BlockNo <$> fromField f

instance ToField C.BlockNo where
  toField (C.BlockNo s) = SQLInteger $ fromIntegral s


open
  :: FilePath
  -> Depth
  -> IO UtxoIndex
open dbPath (Depth k) = do
  -- The second parameter ((k + 1) * 2) specifies the amount of events that are buffered.
  -- The larger the number, the more RAM the indexer uses. However, we get improved SQL
  -- queries due to batching more events together.
-- TODO this required for k > 0
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let conn = ix ^. Ix.handle
  SQL.execute_ conn "DROP TABLE IF EXISTS unspent_transactions"
  SQL.execute_ conn "DROP TABLE IF EXISTS spent"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS unspent_transactions (address TEXT NOT NULL, txId TEXT NOT NULL, txIx INT NOT NULL, datum TEXT, datumHash TEXT, value TEXT, slotNo INT, blockNo INT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS spent (txId TEXT NOT NULL, txIx INT NOT NULL)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS unspent_transaction_address ON unspent_transactions (address)"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS unspent_transaction_txid ON unspent_transactions (txId)"
  pure ix

eventAtAddress :: C.AddressAny -> UtxoEvent -> [UtxoEvent]
eventAtAddress addr event =
    let
        utxosAtAddress :: [Utxo]
        utxosAtAddress = event ^. utxoEventUtxos ^.. folded . filtered (\u -> (u ^. utxoAddress ) == addr)
    in
        if null utxosAtAddress  then []
        else [event { _utxoEventUtxos = utxosAtAddress }]

addressFilteredEvents :: C.AddressAny -> [UtxoEvent] -> [UtxoEvent]
addressFilteredEvents addr = concatMap (eventAtAddress addr)

toRows :: UtxoEvent -> [UtxoRow]
toRows event =  event ^. utxoEventUtxos & traversed %~ f
    where
        f :: Utxo -> UtxoRow
        f  u = UtxoRow u (event ^. utxoEventSlotNo ) (event ^. utxoEventBlockNo)

-- | Query the data stored in the indexer as a whole from:
    -- + hotStore  : in-memory
    -- + coldStore : SQL DB
    -- + buffered  : data that can still change (through rollbacks)
--
addressFilteredRows :: C.AddressAny -> [UtxoEvent] -> [UtxoRow]
addressFilteredRows addr = (concatMap toRows ) . addressFilteredEvents addr

query
  :: UtxoIndex
  -> C.AddressAny
  -> [UtxoEvent]                    -- ^ inflight events
  -> IO Result
query ix addr events = do
  fromColdStore <-
      SQL.query
        (ix ^. Ix.handle)
        "SELECT u.address, u.txId, u.txIx, u.datum, u.datumHash, u.value, u.slotNo, u.blockNo FROM unspent_transactions u LEFT JOIN spent s ON u.txId = s.txId AND u.txIx = s.txIx WHERE u.address = ?"
        (Only addr) :: IO[UtxoRow]

  -- putStrLn $ show fromColdStore <> "fromColdstore"
  hotStore <- Ix.getEvents $ ix ^. Ix.storage :: IO [UtxoEvent]
  -- putStrLn $ show hotStore <> " fromhotstore"
  pure . Just $
      (addressFilteredRows addr events)
      `union`
      (addressFilteredRows addr hotStore)
      `union`
      fromColdStore
      -- TODO
      -- & filter (\u -> not (_reference u `Set.member` spentOutputs))
      -- & map _reference

onInsert :: UtxoIndex -> UtxoEvent -> IO [Notification]
onInsert  _ _ =  pure []

store :: UtxoIndex -> IO ()
store ix = do
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  putStrLn "storing row"
  let rows =  (concatMap toRows) $  buffer
      spent = concatMap (Set.toList . _utxoEventInputs) buffer
      conn = ix ^. Ix.handle
  bracket_
      (SQL.execute_ conn "BEGIN")
      (SQL.execute_ conn "COMMIT")
      ( concurrently_ (
            unless (null rows)
                (SQL.executeMany conn
                "INSERT OR REPLACE INTO unspent_transactions (address, txId, txIx, datum, datumHash, value, slotNo, blockNo) VALUES (?,?,?,?,?,?,?,?)"
                rows) >> putStrLn "inserted utxo")
            (unless (null spent)
                (SQL.executeMany conn
                "INSERT OR REPLACE INTO spent (txId, txIx) VALUES (?, ?)"
                spent) >> putStrLn "inserted spent")
      )

  -- We want to perform vacuum about once every 100 * buffer ((k + 1) * 2)
  -- rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
  -- when (rndCheck == 42) $ do
  --   SQL.execute_ conn "DELETE FROM unspent_transactions WHERE unspent_transactions.rowid IN (SELECT unspent_transactions.rowid FROM unspent_transactions LEFT JOIN spent on unspent_transactions.txId = spent.txId AND unspent_transactions.txIx = spent.txIx WHERE spent.txId IS NOT NULL)"
  --   SQL.execute_ conn "VACUUM"

toAddr :: C.AddressInEra CurrentEra -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
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

import Cardano.Api qualified as C
import Control.Lens (filtered, folded, traversed)
import Control.Lens.Operators ((%~), (&), (^.), (^..))
import Control.Lens.TH (makeLenses)

import Cardano.Binary (fromCBOR, toCBOR)
import Codec.Serialise (Serialise (encode), deserialiseOrFail, serialise)
import Codec.Serialise.Class (Serialise (decode))
import Control.Monad (when)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (catMaybes, fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLInteger, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import Marconi.Types (CurrentEra)
import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix
import System.Random.MWC (createSystemRandom, uniformR)
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
    } deriving Show

instance Eq Utxo where
    u1 == u2 = (_utxoTxId u1) == (_utxoTxId u2)

instance Ord Utxo where
    compare u1 u2 = compare (_utxoTxId u1) (_utxoTxId u2)

$(makeLenses ''Utxo)

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
    } deriving (Show, Eq, Ord)

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

isAtAddress :: C.AddressAny -> Utxo -> Bool
isAtAddress address' utx = (utx ^. utxoAddress) == address'

onlyAt :: C.AddressAny -> UtxoEvent -> Maybe UtxoEvent
onlyAt address' event =
    let
        uts =  event ^. utxoEventUtxos ^.. folded . filtered (isAtAddress address')
    in
        if null uts then
            Nothing
        else Just event {_utxoEventUtxos = uts}

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
addressFilteredRows :: C.AddressAny -> [UtxoEvent] -> Set.Set UtxoRow
addressFilteredRows addr = Set.fromList . concatMap toRows . catMaybes . fmap (onlyAt addr)

query
  :: UtxoIndex
  -> C.AddressAny
  -> [UtxoEvent]                    -- ^ inflight events
  -> IO Result
query ix address' events = do
  fromColdStore <-
      SQL.query
        (ix ^. Ix.handle)
        "SELECT u.address, u.txId, u.txIx, u.datum, u.datumHash, u.value, u.slotNo, u.blockNo FROM unspent_transactions u LEFT JOIN spent s ON u.txId = s.txId AND u.txIx = s.txIx WHERE u.address = ?"
        (Only address')
  buffered <- Ix.getBuffer $ ix ^. Ix.storage
  pure . Just . Set.toList
      $  (Set.fromList fromColdStore
          `Set.union` (addressFilteredRows address' buffered)
          `Set.union` (addressFilteredRows address' events) )

onInsert :: UtxoIndex -> UtxoEvent -> IO [Notification]
onInsert _ _ =  pure []

store :: UtxoIndex -> IO ()
store ix = do
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let asTuple :: UtxoRow -> (C.AddressAny, C.TxId, C.TxIx, Maybe C.ScriptData, Maybe (C.Hash C.ScriptData), C.Value , C.SlotNo, C.BlockNo)
      asTuple u =
          ( (u ^. utxoRowUtxo . utxoAddress)
          , (u ^. utxoRowUtxo . utxoTxId)
          , (u ^. utxoRowUtxo . utxoTxIx)
          , (u ^. utxoRowUtxo . utxoDatum)
          , (u ^. utxoRowUtxo . utxoDatumHash)
          , (u ^. utxoRowUtxo . utxoValue)
          , (u ^. utxoRowSlotNo)
          , (u ^. utxoRowBlockNo))
      rows = (fmap asTuple) . (concatMap toRows) $  buffer
      spent = concatMap (Set.toList . _utxoEventInputs) buffer
      conn = ix ^. Ix.handle
  SQL.execute_ conn "BEGIN"
  SQL.executeMany conn
      "INSERT OR REPLACE INTO unspent_transactions (address, txId, txIx, datum, datumHash, value, slotNo, blockNo) VALUES (?,?,?,?,?,?,?,?)"
      rows
  SQL.executeMany conn
      "INSERT OR REPLACE INTO spent (txId, txIx) VALUES (?, ?)"
      spent
  SQL.execute_ conn "COMMIT"

  -- We want to perform vacuum about once every 100 * buffer ((k + 1) * 2)
  rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
  when (rndCheck == 42) $ do
    SQL.execute_ conn "DELETE FROM unspent_transactions WHERE unspent_transactions.rowid IN (SELECT unspent_transactions.rowid FROM unspent_transactions LEFT JOIN spent on unspent_transactions.txId = spent.txId AND unspent_transactions.txIx = spent.txIx WHERE spent.txId IS NOT NULL)"
    SQL.execute_ conn "VACUUM"

toAddr :: C.AddressInEra CurrentEra -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
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
module Marconi.Index.Utxos
    ( UnspentTransaction (..)
    , UnspentTransactionEvent (..)
                           ) where

import Cardano.Api qualified as C
import Control.Lens (filtered, folded, traversed)
import Control.Lens.Operators ((%~), (&), (^.), (^..))
import Control.Lens.TH (makeLenses)

import Cardano.Binary (fromCBOR, toCBOR)
import Codec.Serialise (Serialise (encode), deserialiseOrFail, serialise)
import Codec.Serialise.Class (Serialise (decode))
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (catMaybes, fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Database.SQLite.Simple (Only (Only), SQLData (SQLBlob, SQLInteger, SQLText))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix
import Text.ParserCombinators.Parsec (parse)

data UnspentTransaction       = UnspentTransaction
    { _address   :: !C.AddressAny
    , _txId      :: !C.TxId
    , _txIx      :: !C.TxIx
    , _datum     :: Maybe C.ScriptData
    , _datumHash :: Maybe (C.Hash C.ScriptData)
    , _value     :: C.Value
    -- , _inlineScriptHash    :: Maybe (C.Hash (C.ScriptDatum C.WitCtxTxIn))
    -- , _inlineScript        :: Maybe (C.ScriptDatum C.WitCtxTxIn)
    } deriving Show

$(makeLenses ''UnspentTransaction)

data UnspentTransactionEvent  = UnspentTransactionEvent
    { _unspentTransactions :: [UnspentTransaction]
    , _uSlotNo             :: !C.SlotNo
    , _uBlockNo            :: !C.BlockNo
    } deriving Show

$(makeLenses ''UnspentTransactionEvent)

data UnspentTransactionRow   = UnspentTransactionRow
    { _unspentTransaction :: UnspentTransaction
    , _slotNo             :: !C.SlotNo
    , _blockNo            :: !C.BlockNo
    } deriving Show

$(makeLenses ''UnspentTransactionRow)

type Result = Maybe [UnspentTransactionRow]

type Notification = ()

type UnspentTransactionIndex = SqliteIndex UnspentTransactionEvent Notification C.AddressAny Result

newtype Depth = Depth Int

instance FromRow UnspentTransactionRow where
    fromRow
        = UnspentTransactionRow
        <$> (UnspentTransaction
             <$> field
             <*> field
             <*> field
             <*> field
             <*> field
             <*> field )
        <*> field
        <*> field

instance ToField C.Value where
    toField  = SQLText . pack . show . C.valueToList

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
  -> IO UnspentTransactionIndex
open dbPath (Depth k) = do
  -- The second parameter ((k + 1) * 2) specifies the amount of events that are buffered.
  -- The larger the number, the more RAM the indexer uses. However, we get improved SQL
  -- queries due to batching more events together.
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS unspent_transactions (address TEXT NOT NULL, txId TEXT NOT NULL, txIx INT NOT NULL, datum TEXT, datumHash TEXT, value TEXT, slotNo INT, blockNo INT)"
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS unspent_transaction_address ON unspent_transactions (address)"
  SQL.execute_ c "CREATE UNIQUE INDEX IF NOT EXISTS unspent_transaction_txid ON unspent_transactions (txId)"
  pure ix

isAtAddress :: C.AddressAny -> UnspentTransaction -> Bool
isAtAddress address' utx = (utx ^. address) == address'

onlyAt :: C.AddressAny -> UnspentTransactionEvent -> Maybe UnspentTransactionEvent
onlyAt address' event =
    let
        uts =  event ^. unspentTransactions ^.. folded . filtered (isAtAddress address')
    in
        if null uts then
            Nothing
        else Just event {_unspentTransactions = uts}

toRows :: UnspentTransactionEvent -> [UnspentTransactionRow]
toRows event =  event ^. unspentTransactions & traversed %~ f
    where
        f :: UnspentTransaction -> UnspentTransactionRow
        f  u = UnspentTransactionRow u (event ^. uSlotNo ) (event ^. uBlockNo)

-- | Query the data stored in the indexer as a whole from:
    -- + hotStore  : in-memory
    -- + coldStore : SQL DB
    -- + buffered  : data that can still change (through rollbacks)
--
toFilteredRows :: C.AddressAny -> [UnspentTransactionEvent] -> [UnspentTransactionRow]
toFilteredRows address' = concatMap toRows . catMaybes . fmap  (onlyAt address')

query
  :: UnspentTransactionIndex
  -> C.AddressAny
  -> [UnspentTransactionEvent]                    -- ^ inflight events
  -> IO Result
query ix address' events = do
  fromColdStore <- SQL.query (ix ^. Ix.handle) "" (Only address')
  buffered <- Ix.getBuffer $ ix ^. Ix.storage
  pure . Just
      $ fromColdStore
      <> toFilteredRows address' buffered
      <> toFilteredRows address' events
onInsert :: UnspentTransactionIndex -> UnspentTransactionEvent -> IO [Notification]
onInsert _ _ = pure []

store :: UnspentTransactionIndex -> IO ()
store ix = do
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let asTuple :: UnspentTransactionRow -> (C.AddressAny, C.TxId, C.TxIx, Maybe C.ScriptData, Maybe (C.Hash C.ScriptData), C.Value , C.SlotNo, C.BlockNo)
      asTuple u =
          ( (u ^. unspentTransaction . address)
          , (u ^. unspentTransaction . txId)
          , (u ^. unspentTransaction . txIx)
          , (u ^. unspentTransaction . datum)
          , (u ^. unspentTransaction . datumHash)
          , (u ^. unspentTransaction . value )
          , (u ^. slotNo)
          , (u ^. blockNo)  )
      rows = (fmap asTuple) . (concatMap toRows) $  buffer
  SQL.executeMany (ix ^. Ix.handle)
      "INSERT OR REPLACE INTO unspent_transaction (address, txId, txIx, datum, datumHash, value, slotNo, blockNo) VALUES (?,?,?,?,?,?,?,?)"
      rows

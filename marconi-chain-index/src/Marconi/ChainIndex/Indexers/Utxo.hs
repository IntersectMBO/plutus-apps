{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Module for indexing the Utxos in the Cardano blockchain

-- + This module will create the SQL tables:
--
-- + table: unspent_transactions
--
-- @
--      |---------+------+-------+-----------+-------+-------+------------------+--------------+------|
--      | Address | TxId | TxIx  | DatumHash | Datum | Value | InlineScriptHash | InlineScript | Slot |
--      |---------+------+-------+-----------+-------+-------+------------------+--------------+------|
-- @
--
-- + table: spent
-- @
--      |------+------|--------+-----------|
--      | txId | txIx | slotNo | blockHash |
--      |------+------|--------+-----------|
-- @
-- To create these tables, we extract all transactions outputs from each transactions fetched with
-- the chain-sync protocol of the local node.

module Marconi.ChainIndex.Indexers.Utxo where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket_)
import Control.Lens.Combinators (imap)
import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.Either (fromRight)
import Data.Foldable (fold, foldl', toList)
import Data.Functor ((<&>))
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple (Only (Only))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import System.Random.MWC (createSystemRandom, uniformR)
import Text.RawString.QQ (r)

import Cardano.Api ()
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (TargetAddresses, TxOut, pattern CurrentEra)

import Marconi.Core.Storable (Buffered (getStoredEvents, persistToStorage), HasPoint,
                              QueryInterval (QEverything, QInterval), Queryable (queryStorage),
                              Resumable (resumeFromStorage), Rewindable (rewindStorage), StorableEvent, StorableMonad,
                              StorablePoint, StorableQuery, StorableResult, emptyState)
import Marconi.Core.Storable qualified as Storable

type UtxoIndexer = Storable.State UtxoHandle

data UtxoHandle = UtxoHandle
  { hdlConnection :: !SQL.Connection  -- ^ SQLite connection
  , hdlDpeth      :: !Int             -- ^ depth before flushing to disk storage
  , toVacuume     :: !Bool            -- ^ weather to perform SQLite vacuum to release space
  }

newtype instance StorableQuery UtxoHandle =
  UtxoAddress C.AddressAny deriving (Show, Eq, Ord)

type QueryableAddresses = NonEmpty (StorableQuery UtxoHandle)

type instance StorableMonad UtxoHandle = IO

type instance StorablePoint UtxoHandle = C.ChainPoint

newtype Depth = Depth Int

data Utxo = Utxo
  { _address          :: !C.AddressAny
  , _txId             :: !C.TxId
  , _txIx             :: !C.TxIx
  , _datum            :: !(Maybe C.ScriptData)
  , _datumHash        :: !(Maybe (C.Hash C.ScriptData))
  , _value            :: !C.Value
  , _inlineScript     :: !(Maybe C.ScriptInAnyLang)
  , _inlineScriptHash :: !(Maybe C.ScriptHash)
  } deriving (Show, Eq, Generic)

$(makeLenses ''Utxo)

instance Ord Utxo where
  compare (Utxo addr txid txix _ _ _ _ _) (Utxo addr' txid' txix' _ _ _ _ _) =
     compare (addr, C.TxIn txid txix) (addr', C.TxIn txid' txix')

instance FromJSON Utxo where
    parseJSON (Object v) =
        Utxo
            <$> v .: "address"
            <*> v .: "txId"
            <*> v .: "txIx"
            <*> v .: "datum"
            <*> v .: "datumHash"
            <*> v .: "value"
            <*> v .: "inlineScript"
            <*> v .: "inlineScriptHash"
    parseJSON _ = mempty

instance ToJSON Utxo where
  toJSON (Utxo addr txid txix dtum dtumHash val scrpt scrptHash) = object
    [ "address"           .= addr
    , "txId"              .= txid
    , "txIx"              .= txix
    , "datum"             .= dtum
    , "datumHash"         .= dtumHash
    , "value"             .= val
    -- Uses ToJSON instance of cardano-api which serialises using the 'C.HasTextEnvelope' typeclass.
    , "inlineScript"      .= scrpt
    , "inlineScriptHash"  .= scrptHash
    ]

data UtxoRow = UtxoRow
  { _urUtxo      :: !Utxo
  , _urSlotNo    :: !C.SlotNo
  , _urBlockHash :: !(C.Hash C.BlockHeader)
  } deriving (Show, Eq, Ord, Generic)

$(makeLenses ''UtxoRow)

instance FromJSON UtxoRow where
    parseJSON (Object v) =
        UtxoRow
            <$> v .: "utxo"
            <*> v .: "slotNo"
            <*> v .: "blockHeaderHash"
    parseJSON _ = mempty

instance ToJSON UtxoRow where
  toJSON (UtxoRow u s h) = object
    [ "utxo" .= u
    , "slotNo" .= s
    , "blockHeaderHash" .= h
    ]

newtype instance StorableResult UtxoHandle =
    UtxoResult { getUtxoResult :: [UtxoRow] } deriving Show

data instance StorableEvent UtxoHandle = UtxoEvent
  { ueUtxos       :: !(Set Utxo)
  , ueInputs      :: !(Set C.TxIn)
  , ueChainPoint  :: !C.ChainPoint
  } deriving (Eq, Ord, Show, Generic)

eventIsBefore :: C.ChainPoint -> StorableEvent UtxoHandle -> Bool
eventIsBefore (C.ChainPoint slot' _) (UtxoEvent _ _ (C.ChainPoint slot _)) =  slot <= slot'
eventIsBefore _ _                                                          = False

-- | mappend, combine Unspent utxoEvents:
instance Semigroup (StorableEvent UtxoHandle) where
  (UtxoEvent us is cp) <> (UtxoEvent us' is' cp') =
    UtxoEvent utxos txins (max cp cp')
    where
      toTxIn :: Utxo -> C.TxIn
      toTxIn u = C.TxIn (u ^.txId) (u ^. txIx)
      txins = Set.union is is'
      utxos
        = foldl' (\a c -> if toTxIn c `Set.notMember` txins then Set.insert c a; else a) Set.empty
        $ Set.union us us'

instance Monoid (StorableEvent UtxoHandle) where
  mempty = UtxoEvent mempty mempty C.ChainPointAtGenesis

-- | The effect of a transaction (or a number of them) on the tx output set.
data TxOutBalance =
  TxOutBalance
    { _tobUnspent :: !(Set C.TxIn)
    -- ^ Outputs newly added by the transaction(s)
    , _tobSpent   :: !(Set C.TxIn)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving stock (Eq, Show, Generic)

instance Semigroup TxOutBalance where
    tobL <> tobR =
        TxOutBalance
            { _tobUnspent = _tobUnspent tobR
                            <> (_tobUnspent tobL `Set.difference` _tobSpent tobR)
            , _tobSpent   = _tobSpent tobL
                            <>   (_tobSpent tobR `Set.difference` _tobUnspent tobL)

            }

instance Monoid TxOutBalance where
    mappend = (<>)
    mempty = TxOutBalance mempty mempty


data Spent = Spent
  { _sTxId      :: !C.TxId
  , _sTxIx      :: !C.TxIx
  , _sSlotNo    :: !C.SlotNo
  , _sBlockHash :: !(C.Hash C.BlockHeader)
  } deriving (Show, Eq)

$(makeLenses ''Spent)

instance Ord Spent where
  compare s s' =
    compare (s ^. sTxId, s ^. sTxIx) (s' ^. sTxId, s' ^. sTxIx)

instance HasPoint (StorableEvent UtxoHandle) C.ChainPoint where
  getPoint (UtxoEvent _ _ cp) = cp

------------------
-- sql mappings --
------------------

instance ToRow UtxoRow where
  toRow u = toRow
    ( toField (u ^. urUtxo . address)
    , toField (u ^. urUtxo . txId)
    , toField (u ^. urUtxo . txIx)
    , toField (u ^. urUtxo . datum)
    , toField (u ^. urUtxo . datumHash)
    , toField (u ^. urUtxo . value)
    , toField (u ^. urUtxo . inlineScript)
    , toField (u ^. urUtxo . inlineScriptHash)
    , toField (u ^. urSlotNo)
    , toField (u ^. urBlockHash)
    )

instance FromRow UtxoRow where
  fromRow = UtxoRow
      <$> (Utxo <$> field
           <*> field <*> field
           <*> field <*> field <*> field <*> field <*> field)
      <*> field <*> field

instance FromRow Spent where
  fromRow = Spent <$> field <*> field <*> field <*> field

instance ToRow Spent where
  toRow s =
    [ toField (s ^. sTxId)
    , toField (s ^. sTxIx)
    , toField (s ^. sSlotNo)
    , toField (s ^. sBlockHash)
    ]

-- | Open a connection to DB, and create resources
-- The parameter ((k + 1) * 2) specifies the amount of events that are buffered.
-- The larger the number, the more RAM the indexer uses. However, we get improved SQL
-- queries due to batching more events together.
open
  :: FilePath   -- ^ sqlite file path
  -> Depth      -- ^ The Depth parameter k, the larger K, the more RAM the indexer uses
  -> Bool       -- ^ whether to perform vacuum
  -> IO UtxoIndexer
open dbPath (Depth k) isToVacuume = do
  c <- SQL.open dbPath

  SQL.execute_ c "PRAGMA journal_mode=WAL"

  SQL.execute_ c [r|CREATE TABLE IF NOT EXISTS unspent_transactions
                      ( address TEXT NOT NULL
                      , txId TEXT NOT NULL
                      , txIx INT NOT NULL
                      , datum BLOB
                      , datumHash BLOB
                      , value BLOB
                      , inlineScript BLOB
                      , inlineScriptHash BLOB
                      , slotNo INT NOT NULL
                      , blockHash BLOB NOT NULL
                      , UNIQUE (txId, txIx))|]

  SQL.execute_ c [r|CREATE TABLE IF NOT EXISTS spent
                      ( txId TEXT NOT NULL
                      , txIx INT NOT NULL
                      , slotNo INT NOT NULL
                      , blockHash BLOB NOT NULL
                      , UNIQUE (txId, txIx))|]

  SQL.execute_ c [r|CREATE INDEX IF NOT EXISTS
                      spent_slotNo ON spent (slotNo)|]

  SQL.execute_ c [r|CREATE INDEX IF NOT EXISTS
                      unspent_transaction_address ON unspent_transactions (address)|]
  emptyState k (UtxoHandle c k isToVacuume)

getSpentFrom :: StorableEvent UtxoHandle -> [Spent]
getSpentFrom (UtxoEvent _ txIns cp) = case cp of
  C.ChainPointAtGenesis -> [] -- There are no Spent in the Genesis block
  (C.ChainPoint sn bh)  ->  fmap (\(C.TxIn txid txix) -> Spent txid txix sn bh) . Set.toList $ txIns

-- | Store UtxoEvents
-- Events are stored in memory and flushed to SQL, disk, when memory buffer has reached capacity
instance Buffered UtxoHandle where
  persistToStorage
    :: Foldable f
    => f (StorableEvent UtxoHandle) -- ^ ues to store
    -> UtxoHandle -- ^ handler for storing events
    -> StorableMonad UtxoHandle UtxoHandle
  persistToStorage events h = do
    let rows = concatMap eventToRows events
        spents = concatMap getSpentFrom events
        c = hdlConnection h
    bracket_
        (SQL.execute_ c "BEGIN")
        (SQL.execute_ c "COMMIT")
        (concurrently_
         (unless
          (null rows)
          (SQL.executeMany c
            [r|INSERT INTO unspent_transactions (
                 address,
                 txId,
                 txIx,
                 datum,
                 datumHash,
                 value,
                 inlineScript,
                 inlineScriptHash,
                 slotNo,
                 blockHash
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|] rows))
         (unless
          (null spents)
          (SQL.executeMany c
           [r|INSERT INTO spent (
                txId,
                txIx, slotNo, blockHash
              ) VALUES
              (?, ?, ?, ?)|] spents)))
    -- We want to perform vacuum about once every 100
    when (toVacuume h) $ do
      rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
      when (rndCheck == 42) $
        SQL.execute_ c [r|DELETE FROM
                            unspent_transactions
                          WHERE
                            unspent_transactions.rowid IN (
                              SELECT
                                unspent_transactions.rowid
                              FROM
                                unspent_transactions
                                JOIN spent ON unspent_transactions.txId = spent.txId
                                AND unspent_transactions.txIx = spent.txIx
                            )|] >> SQL.execute_ c "VACUUM" -- remove Spent and release space, see https://www.sqlite.org/lang_vacuum.html
    pure h

  getStoredEvents :: UtxoHandle -> StorableMonad UtxoHandle [StorableEvent UtxoHandle]
  getStoredEvents (UtxoHandle c sz _) = do
    sns <- SQL.query c
        [r|SELECT
              slotNo
           FROM
              unspent_transactions
           GROUP BY
              slotNo
           ORDER BY
              slotNo DESC
           LIMIT ?|] (SQL.Only sz) :: IO [[Integer]]

    -- Take the slot number of the sz'th slot
    let sn = if null sns
                then 0
                else head . last $ take sz sns

    rows :: [UtxoRow] <- SQL.query c
        [r|SELECT
              u.address,
              u.txId,
              u.txIx,
              u.datum,
              u.datumHash,
              u.value,
              u.inlineScript,
              u.inlineScriptHash,
              u.slotNo,
              u.blockHash
           FROM
              unspent_transactions u
           WHERE
              u.slotNo >= ?
           GROUP by
              u.slotNo
           ORDER BY
              u.slotNo ASC|] (SQL.Only (sn :: Integer))

    rowsToEvents (getTxIns c) rows

-- | Retrieve TxIns at a slotNo
-- This function is used to reconstruct the original UtxoEvent
getTxIns :: SQL.Connection -> C.SlotNo -> IO (Set C.TxIn)
getTxIns c sn = SQL.query c
    "SELECT txInTxId, txInTxIx FROM spent WHERE slotNo =?" (SQL.Only (sn :: C.SlotNo))
    <&> Set.fromList

-- | Convert UtxoRows to UtxoEvents
rowsToEvents
  :: (C.SlotNo -> IO (Set C.TxIn))  -- ^ function to fetch TxIn
  -> [UtxoRow]                      -- ^ rows to convert back to event
  -> IO [StorableEvent UtxoHandle]  -- ^ utxo events
rowsToEvents _ [] = pure []
rowsToEvents  fetchTxIn rows
  = traverse reduce eventsMap
  <&> sortBy (\(UtxoEvent _ _ cp) (UtxoEvent _ _ cp') -> compare cp' cp )
  where
    mkEvent :: UtxoRow -> StorableEvent UtxoHandle
    mkEvent row = UtxoEvent
       (Set.singleton $ row ^. urUtxo)
       Set.empty
       (C.ChainPoint (row ^. urSlotNo) (row ^. urBlockHash))

    newEventWithSpentOnly :: Set C.TxIn -> C.ChainPoint -> StorableEvent UtxoHandle
    newEventWithSpentOnly = UtxoEvent Set.empty

    reduce :: (C.ChainPoint, [StorableEvent UtxoHandle]) -> IO (StorableEvent UtxoHandle)
    reduce ( C.ChainPointAtGenesis, _) = pure $ UtxoEvent Set.empty Set.empty C.ChainPointAtGenesis
    reduce (cp@(C.ChainPoint sn _), es) = do
                     tins <- fetchTxIn sn
                     let newE = newEventWithSpentOnly tins cp
                     pure. fold $ newE:es

    eventsMap :: [(C.ChainPoint, [StorableEvent UtxoHandle])]
    eventsMap
            = fmap  (\x -> (ueChainPoint . head $ x, x) )
            . groupBy (\el er -> ueChainPoint el == ueChainPoint er)
            . fmap mkEvent
            $ rows

-- | merge in-memory events with SQL retreived UtxoRows
-- Notes, a peroperty of this merge is to remove all spent utxos from the resulting [UtxoRow]
mergeInMemoryAndSql
  :: Foldable f
  => f (StorableEvent UtxoHandle)
  -> [UtxoRow]
  -> [UtxoRow]
mergeInMemoryAndSql events = filter (\u -> C.TxIn (u ^. urUtxo . txId)(u ^. urUtxo . txIx) `notElem` txins)
  where
    txins :: Set C.TxIn
    txins = foldl' (\a c -> ueInputs c `Set.union` a) Set.empty events

-- | convert utxoEvent to utxoRow
-- Note: No `unspent` computeation is performed
eventToRows :: StorableEvent UtxoHandle -> [UtxoRow]
eventToRows (UtxoEvent _ _ C.ChainPointAtGenesis) = []  -- we dont save anyting at genesis.  TODO verify
eventToRows (UtxoEvent utxos _ (C.ChainPoint sn bhsh)) =
  fmap (\u -> UtxoRow
           { _urUtxo = u
           , _urSlotNo = sn
           , _urBlockHash = bhsh
           }
       ) . Set.toList $ utxos

-- | Filter for events at the given address
eventsAtAddress
  :: Foldable f
  => StorableQuery UtxoHandle       -- ^ Address query
  -> f (StorableEvent UtxoHandle)   -- ^ Utxo event
  -> [StorableEvent UtxoHandle]     -- ^ Utxo event at thegiven address
eventsAtAddress (UtxoAddress addr) = concatMap splitEventAtAddress
  where
    splitEventAtAddress :: StorableEvent UtxoHandle -> [StorableEvent UtxoHandle]
    splitEventAtAddress event =
      let
        utxosAtAddress :: Set Utxo
        utxosAtAddress = Set.filter (\u -> (u ^. address) == addr) . ueUtxos $ event
      in
        ([event {ueUtxos = utxosAtAddress} | not (null utxosAtAddress)])

-- | only store rows in the address list.
addressFilteredRows
  :: Foldable f
  => StorableQuery UtxoHandle       -- ^ query
  -> f (StorableEvent UtxoHandle)   -- ^ Utxo Event
  -> [UtxoRow]                      -- ^ Rows at the query
addressFilteredRows addr = concatMap eventToRows . eventsAtAddress addr . toList

-- | Query the data stored in the indexer
-- Quries SQL + buffered data, where buffered data is the data that will be batched to SQL
instance Queryable UtxoHandle where
  queryStorage
    :: Foldable f
    => QueryInterval C.ChainPoint
    -> f (StorableEvent UtxoHandle)
    -> UtxoHandle
    -> StorableQuery UtxoHandle
    -> IO (StorableResult UtxoHandle)
  queryStorage qi es (UtxoHandle c _ _) q@(UtxoAddress addr) = do
    fromSQL :: [UtxoRow] <- case qi of -- get the unspent transaction from DB
      QEverything -> SQL.query c
          [r|SELECT
                u.address,
                u.txId,
                u.txIx,
                u.datum,
                u.datumHash,
                u.value,
                u.inlineScript,
                u.inlineScriptHash,
                u.slotNo,
                u.blockHash
             FROM
                unspent_transactions u
                LEFT JOIN spent s ON u.txId = s.txId
                AND u.txIx = s.txIx
             WHERE
                u.address = ?
                AND s.txId IS NULL
                AND s.txIx IS NULL
             ORDER BY
                u.slotNo ASC|] (Only (C.serialiseToRawBytes addr))
      QInterval _ (C.ChainPoint sn _) -> SQL.query c
          [r|SELECT
                u.address,
                u.txId,
                u.txIx,
                u.datum,
                u.datumHash,
                u.value,
                u.inlineScript,
                u.inlineScriptHash,
                u.slotNo,
                u.blockHash
             FROM
                unspent_transactions u
             LEFT JOIN spent s ON u.txId = s.txId
             AND u.txIx = s.txIx
             WHERE
                u.slotNo <= ?
                AND  u.address = ?
                AND s.txId IS NOT NULL
                AND s.txIx IS NOT NULL
             ORDER BY
                u.slotNo ASC|] (sn, C.serialiseToRawBytes addr)
      QInterval _ C.ChainPointAtGenesis -> pure []
    let eventAtQuery :: StorableEvent UtxoHandle = queryBuffer qi q es -- query in-memory and conver to row
    pure
      $ UtxoResult
      $ mergeInMemoryAndSql
          es
          (fromSQL <> eventToRows eventAtQuery)

-- | Query memory buffer
queryBuffer
  :: Foldable f
  => QueryInterval C.ChainPoint
  -> StorableQuery UtxoHandle             -- ^ Query
  -> f (StorableEvent UtxoHandle)         -- ^ Utxo events
  -> StorableEvent UtxoHandle
queryBuffer QEverything q      = fold . eventsAtAddress q
queryBuffer (QInterval _ cp) q = fold . filter (eventIsBefore cp) . eventsAtAddress q

instance Rewindable UtxoHandle where
  rewindStorage :: C.ChainPoint -> UtxoHandle -> IO (Maybe UtxoHandle)
  rewindStorage (C.ChainPoint sn _) h@(UtxoHandle c _ _) = do
    SQL.execute c "DELETE FROM unspent_transactions WHERE slotNo > ?" (SQL.Only sn)
    SQL.execute c "DELETE FROM spent WHERE slotNo > ?" (SQL.Only sn)
    pure $ Just h
  rewindStorage C.ChainPointAtGenesis _ = pure Nothing

-- For resuming we need to provide a list of points where we can resume from.
instance Resumable UtxoHandle where
  resumeFromStorage (UtxoHandle c _ _) = do
    chainPoints <- fmap (uncurry C.ChainPoint) <$>
            SQL.query c
                [r|SELECT slotNo, blockHash
                   FROM unspent_transactions
                   ORDER BY slotNo DESC|] ()
    -- The ordering here matters. The node will try to find the first point in the
    -- ledger, then move to the next and so on, so we will send the latest point
    -- first.
    pure $ chainPoints ++ [C.ChainPointAtGenesis]

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

-- | Extract UtxoEvents from Cardano Block
getUtxoEventsFromBlock
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> C.Block era
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock maybeTargetAddresses (C.Block (C.BlockHeader slotNo hsh _) txs) =
  getUtxoEvents maybeTargetAddresses txs (C.ChainPoint slotNo hsh)

-- | Extract UtxoEvents from Cardano Transactions
getUtxoEvents
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> [C.Tx era]
  -> C.ChainPoint
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents maybeTargetAddresses txs cp =
  let utxoMap :: Map C.TxIn Utxo
      utxoMap = foldMap (getUtxos maybeTargetAddresses) txs
      (TxOutBalance utxos spentTxOuts) = foldMap txOutBalanceFromTx txs
      resolvedUtxos :: Set Utxo
      resolvedUtxos
        = Set.fromList
        $ mapMaybe (`Data.Map.lookup` utxoMap)
        $ Set.toList utxos
  in
    UtxoEvent resolvedUtxos spentTxOuts cp

-- | does the transaction contain a targetAddress
isAddressInTarget :: Maybe TargetAddresses -> C.AddressAny -> Bool
isAddressInTarget Nothing _ = True -- all addresses are target addresses
isAddressInTarget (Just targetAddresses) addr =
    case addr  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

getUtxos :: (C.IsCardanoEra era) => Maybe TargetAddresses -> C.Tx era -> Map C.TxIn Utxo
getUtxos maybeTargetAddresses (C.Tx txBody@(C.TxBody C.TxBodyContent {C.txOuts}) _) =
  fromRight Data.Map.empty (getUtxos' txOuts)
  where
    getUtxos' :: C.IsCardanoEra era => [C.TxOut C.CtxTx era] -> Either C.EraCastError (Map C.TxIn Utxo)
    getUtxos'
      = fmap (Data.Map.fromList . concatMap Data.Map.toList . imap txoutToUtxo)
      . traverse (C.eraCast CurrentEra)

    txoutToUtxo :: Int -> TxOut -> Map C.TxIn Utxo
    txoutToUtxo ix (C.TxOut addr value' datum' refScript) =
      if isAddressInTarget maybeTargetAddresses addrAny then
        Data.Map.singleton txin Utxo
        { _txId = txid
        , _txIx = txix
        , _address = addrAny
        , _value = C.txOutValueToValue value'
        , _datum = __datum
        , _datumHash = __datumHash
        , _inlineScript = __inlineScript
        , _inlineScriptHash = __inlineScriptHash
        }
      else
        Data.Map.empty
      where
        addrAny = toAddr addr
        (__datum, __datumHash) = getScriptDataAndHash datum'
        (__inlineScript, __inlineScriptHash) = getRefScriptAndHash refScript
        txin@(C.TxIn txid txix) = C.TxIn (C.getTxId txBody)(C.TxIx $ fromIntegral ix)

-- | get the inlineScript and inlineScriptHash
--
getRefScriptAndHash
  :: C.ReferenceScript era
  -> (Maybe C.ScriptInAnyLang, Maybe C.ScriptHash)
getRefScriptAndHash refScript = case refScript of
  C.ReferenceScriptNone -> (Nothing, Nothing)
  C.ReferenceScript _ s@(C.ScriptInAnyLang(C.SimpleScriptLanguage C.SimpleScriptV1) script) ->
      ( Just  s
      , Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV2) script)->
    ( Just s
    , Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script)->
    ( Just s
    , Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script)->
    ( Just s
    , Just . C.hashScript $ script)

-- | Get the datum hash and datum or a transaction output.
getScriptDataAndHash
  :: C.TxOutDatum C.CtxTx era
  -> (Maybe C.ScriptData, Maybe (C.Hash C.ScriptData))
getScriptDataAndHash C.TxOutDatumNone         = (Nothing, Nothing)
getScriptDataAndHash (C.TxOutDatumHash _ h)   = (Nothing, Just h)
getScriptDataAndHash (C.TxOutDatumInTx _ d)   = (Just d, (Just . C.hashScriptData) d)
getScriptDataAndHash (C.TxOutDatumInline _ d) = (Just d, (Just . C.hashScriptData) d)

-- | remove spent transactions
rmSpent :: Set C.TxIn -> [Utxo] -> [Utxo]
rmSpent txins = filter (not . isUtxoSpent txins)
  where
    isUtxoSpent :: Set C.TxIn -> Utxo -> Bool
    isUtxoSpent txIns u =
        C.TxIn (u ^. txId)(u ^. txIx) `Set.member` txIns

getInputs :: C.Tx era -> Set C.TxIn
getInputs (C.Tx (C.TxBody C.TxBodyContent
                 { C.txIns
                 , C.txScriptValidity
                 , C.txInsCollateral
                 }) _) =
  let
    inputs = case txScriptValidityToScriptValidity txScriptValidity of
      C.ScriptValid -> fst <$> txIns
      C.ScriptInvalid -> case txInsCollateral of
        C.TxInsCollateralNone     -> []
        C.TxInsCollateral _ txins -> txins
  in
    Set.fromList inputs

-- | Duplicated from cardano-api (not exposed in cardano-api)
-- This function should be removed when marconi will depend on a cardano-api version that has accepted this PR:
-- https://github.com/input-output-hk/cardano-node/pull/4569
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone                = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity

-- | does the transaction contain a targetAddress
isAddressInTarget' :: TargetAddresses -> Utxo -> Bool
isAddressInTarget' targetAddresses utxo =
    case utxo ^. address  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

mkQueryableAddresses :: TargetAddresses -> QueryableAddresses
mkQueryableAddresses = fmap (UtxoAddress . C.toAddressAny)

txOutBalanceFromTxs :: [C.Tx era] -> TxOutBalance
txOutBalanceFromTxs = foldMap txOutBalanceFromTx

txOutBalanceFromTx :: C.Tx era -> TxOutBalance
txOutBalanceFromTx (C.Tx txBody@(C.TxBody txBodyContent) _) =
    let
        txInputs = Set.fromList $ fst <$> C.txIns txBodyContent
        utxoRefs = Set.fromList
                 $ fmap (\(txix, _) -> C.TxIn (C.getTxId txBody) (C.TxIx txix))
                 $ zip [0..]
                 $ C.txOuts txBodyContent
     in TxOutBalance utxoRefs txInputs

convertTxOutToUtxo :: C.TxId -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo
convertTxOutToUtxo txid txix (C.TxOut (C.AddressInEra _ addr) val txOutDatum refScript) =
    let (scriptDataHash, scriptData) =
            case txOutDatum of
              C.TxOutDatumNone       -> (Nothing, Nothing)
              C.TxOutDatumHash _ dh  -> (Just dh, Nothing)
              C.TxOutDatumInTx _ d   -> (Just $ C.hashScriptData d, Just d)
              C.TxOutDatumInline _ d -> (Just $ C.hashScriptData d, Just d)
        (scriptHash, script) =
            case refScript of
              C.ReferenceScriptNone -> (Nothing, Nothing)
              C.ReferenceScript _ scriptInAnyLang@(C.ScriptInAnyLang _ s) ->
                  (Just $ C.hashScript s, Just scriptInAnyLang)
     in Utxo
            { _address = C.toAddressAny addr
            , _txId = txid
            , _txIx = txix
            , _datum = scriptData
            , _datumHash = scriptDataHash
            , _value = C.txOutValueToValue val
            , _inlineScript = script
            , _inlineScriptHash = scriptHash
            }

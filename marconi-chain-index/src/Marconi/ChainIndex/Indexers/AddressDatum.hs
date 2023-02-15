{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

-- | Module for indexing the datums for all addresses in the Cardano blockchain.

-- This module will create the SQL tables:

-- + table: address_datums
--
-- @
--    |---------+------------+---------+------------|
--    | address | datum_hash | slot_no | block_hash |
--    |---------+------------+---------+------------|
-- @
--
-- + table: datumhash_datum
--
-- @
--    |------------+-------|
--    | datum_hash | datum |
--    |------------+-------|
-- @

-- To create these tables, we extract all transactions outputs from each transactions fetched with
-- the chain-sync protocol of the local node.

-- Here is a synopsis of the indexing algorithm.

-- Each transaction output contains an address along with an optional datum hash or an optional inline
-- datum (actual datum).
-- In the inline datum scenario, we simply create an entry in the `address_datums` table with the hash
-- of the datum, and add an entry in the `datumhash_datum` table.
-- In the datum hash scenario, we create an entry in the `address_datums` table, but not in the
-- `datumhash_datum` table, as we don't know the actual value of the datum and we can't infer it from
-- the hash.
-- In that last scenario, we can resolve in the datum hash in one of three ways (which we then simply
-- add an entry in the `datumhash_datum` table):
--
--     * a different transaction output has an inline datum with that same hash
--
--     * a datum with that same hash has been found in the in transaction body
--
--     * a datum with that same hash was included in the witnesses for a Plutus spending script
--     which was included in the transaction body
--
module Marconi.ChainIndex.Indexers.AddressDatum
  ( -- * AddressDatumIndex
    AddressDatumIndex
  , AddressDatumHandle
  , StorableEvent(..)
  , StorableQuery(..)
  , StorableResult(..)
  , toAddressDatumIndexEvent
  , AddressDatumQuery
  , AddressDatumResult
  , AddressDatumDepth (..)
  , open
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.TxWitness qualified as Ledger
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Data.Foldable (Foldable (foldl'), fold, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Storable (Buffered (persistToStorage), HasPoint (getPoint), QueryInterval (QEverything, QInterval),
                              Queryable (queryStorage), Resumable, Rewindable (rewindStorage), StorableEvent,
                              StorableMonad, StorablePoint, StorableQuery, StorableResult, emptyState,
                              filterWithQueryInterval)
import Marconi.Core.Storable qualified as Storable
import Text.RawString.QQ (r)

-- | Define the `handler` data type, meant as a wrapper for the connection type (in this case the
-- SQLite connection). In this indexer, we also add the number of events that we want to return from
-- the on-disk buffer.
data AddressDatumHandle = AddressDatumHandle
    { addressDatumHandleConnection :: SQL.Connection
    , _addressDatumHandleDiskStore :: Int
    }

type instance StorableMonad AddressDatumHandle = IO

-- | 'StorableEvent AddressDatumHandle is the type of events. Events are the data atoms that the
-- indexer consumes.
-- They depend on the `handle` because they need to eventually be persisted in the database, so the
-- database has to be able to accomodate them.

-- We store the datum hashes of each address that we processed.
-- Then we keep a separate 'Map' which stores the actual datum given a datum hash.
-- Note that we don't always have the actual datum for a given hash.
data instance StorableEvent AddressDatumHandle =
    AddressDatumIndexEvent
        (Map C.AddressAny (Set (C.Hash C.ScriptData)))
        (Map (C.Hash C.ScriptData) C.ScriptData)
        !C.ChainPoint
    deriving (Eq, Show)

instance Semigroup (StorableEvent AddressDatumHandle) where
    AddressDatumIndexEvent ad1 d1 c1 <> AddressDatumIndexEvent ad2 d2 c2 =
        AddressDatumIndexEvent
            (Map.unionWith (<>) ad1 ad2)
            (Map.union d1 d2)
            (max c1 c2)

instance Monoid (StorableEvent AddressDatumHandle) where
    mempty = AddressDatumIndexEvent Map.empty Map.empty C.ChainPointAtGenesis
    mappend = (<>)

type instance StorablePoint AddressDatumHandle = C.ChainPoint

instance HasPoint (StorableEvent AddressDatumHandle) C.ChainPoint where
  getPoint (AddressDatumIndexEvent _ _ s) = s

data instance StorableQuery AddressDatumHandle =
    AllAddressesQuery
    | AddressDatumQuery C.AddressAny

data instance StorableResult AddressDatumHandle =
    AllAddressesResult (Set C.AddressAny)
  | AddressDatumResult (Set C.ScriptData)
    deriving (Eq, Show)

type AddressDatumQuery = StorableQuery AddressDatumHandle
type AddressDatumResult = StorableResult AddressDatumHandle

type AddressDatumIndex = Storable.State AddressDatumHandle

newtype AddressDatumDepth = AddressDatumDepth Int

-- * SQLite

data AddressDatumHashRow = AddressDatumHashRow
  { addressDatumRowAddress   :: !C.AddressAny
  , addressDatumRowDatumHash :: !(C.Hash C.ScriptData)
  , addressDatumRowSlot      :: !C.SlotNo
  , addressDatumRowBlockHash :: !(C.Hash C.BlockHeader)
  } deriving (Show, Generic)

instance SQL.ToRow AddressDatumHashRow where
  toRow (AddressDatumHashRow addr d slotNo blockHash) =
      [ SQL.toField addr
      , SQL.toField d
      , SQL.toField slotNo
      , SQL.toField blockHash
      ]

deriving anyclass instance SQL.FromRow AddressDatumHashRow

data DatumRow = DatumRow
    { datumRowDatumHash :: C.Hash C.ScriptData
    , datumRowDatum     :: C.ScriptData
    } deriving (Show, Generic)

instance SQL.ToRow DatumRow where
  toRow (DatumRow dh d) = [SQL.toField dh, SQL.toField d]

deriving anyclass instance SQL.FromRow DatumRow

toAddressDatumIndexEvent
    :: Maybe (C.Address C.ShelleyAddr -> Bool)
    -> [C.Tx era]
    -> C.ChainPoint
    -> StorableEvent AddressDatumHandle
toAddressDatumIndexEvent addressFilter txs chainPoint = do
    let datumsPerAddr = getDatumsPerAddressFromTxs
        filterFun =
            case addressFilter of
              Nothing -> id
              Just f -> Map.filterWithKey $ \k _ ->
                  case k of
                      -- Target addresses filter are only shelley addresses. Therefore, as we
                      -- encounter Byron addresses with datum, we don't filter them. However, that
                      -- is highly improbable as Byron addresses are almost never used anymore.
                      C.AddressByron _      -> True
                      C.AddressShelley addr -> f addr
        filteredDatumsPerAddr = filterFun datumsPerAddr
        datumMap = Map.fromList
                 $ mapMaybe (\(dh, d) -> fmap (dh,) d)
                 $ concatMap (\datums -> Map.toList datums)
                 $ Map.elems filteredDatumsPerAddr
     in AddressDatumIndexEvent
            (fmap Map.keysSet filteredDatumsPerAddr)
            (Map.union datumMap getPlutusWitDatumsFromTxs)
            chainPoint
 where
    getDatumsPerAddressFromTxs :: Map C.AddressAny (Map (C.Hash C.ScriptData) (Maybe C.ScriptData))
    getDatumsPerAddressFromTxs =
         Map.filter (not . Map.null)
            $ Map.fromListWith (Map.unionWith (<|>))
            $ concatMap getDatumsPerAddressFromTx txs

    getDatumsPerAddressFromTx
        :: C.Tx era
        -> [(C.AddressAny, Map (C.Hash C.ScriptData) (Maybe C.ScriptData))]
    getDatumsPerAddressFromTx (C.Tx (C.TxBody C.TxBodyContent { C.txOuts }) _) =
         fmap
            (\(C.TxOut (C.AddressInEra _ addr) _ dat _) ->
                ( C.toAddressAny addr
                , maybe Map.empty (uncurry Map.singleton) $ getScriptDataFromTxOutDatum dat
                ))
            txOuts

    getScriptDataFromTxOutDatum
        :: C.TxOutDatum C.CtxTx era
        -> Maybe (C.Hash C.ScriptData, Maybe C.ScriptData)
    getScriptDataFromTxOutDatum (C.TxOutDatumHash _ dh)  = Just (dh, Nothing)
    getScriptDataFromTxOutDatum (C.TxOutDatumInTx _ d)   = Just (C.hashScriptData d, Just d)
    getScriptDataFromTxOutDatum (C.TxOutDatumInline _ d) = Just (C.hashScriptData d, Just d)
    getScriptDataFromTxOutDatum _                        = Nothing

    getPlutusWitDatumsFromTxs :: Map (C.Hash C.ScriptData) C.ScriptData
    getPlutusWitDatumsFromTxs =
        foldr (\acc x -> Map.union acc x) Map.empty
        $ fmap (\(C.Tx txBody _) -> getPlutusWitDatumsFromTxBody txBody) txs

    getPlutusWitDatumsFromTxBody :: C.TxBody era -> Map (C.Hash C.ScriptData) C.ScriptData
    getPlutusWitDatumsFromTxBody (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Ledger.TxDats' datum) _) _ _) =
        -- TODO I'm recomputing the ScriptHash hash, because cardano-api doesn't provide the correct
        -- functions to convert 'Ledger.DataHash' to 'C.Hash C.ScriptData'. This should go away once
        -- we fully switch to `cardano-ledger` types.
        Map.fromList
            $ fmap (\(_, alonzoDat) -> let d = C.fromAlonzoData alonzoDat in (C.hashScriptData d, d))
            $ Map.toList datum
    getPlutusWitDatumsFromTxBody (C.TxBody _) = Map.empty

instance Buffered AddressDatumHandle where
  persistToStorage
    :: Foldable f
    => f (StorableEvent AddressDatumHandle)
    -> AddressDatumHandle
    -> IO AddressDatumHandle
  persistToStorage es h = do
    let addressDatumHashRows = foldl' (\ea e -> ea ++ toAddressDatumHashRow e) [] es
        datumRows = foldl' (\ea e -> ea ++ toDatumRow e) [] es
        c    = addressDatumHandleConnection h
    SQL.execute_ c "BEGIN"
    forM_ addressDatumHashRows $
      SQL.execute c
          [r|INSERT INTO address_datums
              ( address
              , datum_hash
              , slot_no
              , block_hash
              )
             VALUES (?, ?, ?, ?)|]
    forM_ datumRows $
      -- We ignore inserts that introduce duplicate datum hashes in the table
      SQL.execute c
        [r|INSERT OR IGNORE INTO datumhash_datum
            ( datum_hash
            , datum
            )
           VALUES (?, ?)|]
    SQL.execute_ c "COMMIT"
    pure h
    where
      toAddressDatumHashRow :: StorableEvent AddressDatumHandle -> [AddressDatumHashRow]
      toAddressDatumHashRow (AddressDatumIndexEvent _ _ C.ChainPointAtGenesis) = []
      toAddressDatumHashRow (AddressDatumIndexEvent addressDatumHashMap _ (C.ChainPoint sl bh)) = do
          (addr, dhs) <- Map.toList addressDatumHashMap
          dh <- Set.toList dhs
          pure $ AddressDatumHashRow addr dh sl bh
      toDatumRow :: StorableEvent AddressDatumHandle -> [DatumRow]
      toDatumRow (AddressDatumIndexEvent _ datumMap _) =
          fmap (uncurry DatumRow) $ Map.toList datumMap

  getStoredEvents
    :: AddressDatumHandle
    -> IO [StorableEvent AddressDatumHandle]
  getStoredEvents (AddressDatumHandle c n) = do
      sns :: [[Integer]] <-
          SQL.query c
            [r|SELECT slot_no
               FROM address_datums
               GROUP BY slot_no
               ORDER BY slot_no
               DESC LIMIT ?|]
            (SQL.Only n)
      -- Take the slot number of the sz'th slot
      let sn = if null sns
                  then 0
                  else head . last $ take n sns
      res <-
          SQL.query c
            [r|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
               FROM address_datums
               LEFT JOIN datumhash_datum
               ON datumhash_datum.datum_hash = address_datums.datum_hash
               WHERE slot_no >= ?
               ORDER BY slot_no DESC, address, datumhash_datum.datum_hash|]
            (SQL.Only (sn :: Integer))
      pure $ asEvents res

-- | This function recomposes the in-memory format from the database records.
asEvents
  :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo, C.Hash C.BlockHeader)]
  -- ^ Should be sorted by C.SlotNo in ascending order.
  -> [StorableEvent AddressDatumHandle]
asEvents events =
    fmap toEvent $ NonEmpty.groupWith (\(_, _, _, s, _) -> s) events
 where
     toEvent
         :: NonEmpty ( C.AddressAny
                     , C.Hash C.ScriptData
                     , Maybe C.ScriptData
                     , C.SlotNo
                     , C.Hash C.BlockHeader
                     )
         -> StorableEvent AddressDatumHandle
     toEvent es =
         let (_, _, _, slot, blockHash) = NonEmpty.head es
          in AddressDatumIndexEvent (toAddressDatums es) (toDatumMap es) (C.ChainPoint slot blockHash)

     toAddressDatums
         :: NonEmpty ( C.AddressAny
                     , C.Hash C.ScriptData
                     , Maybe C.ScriptData
                     , C.SlotNo
                     , C.Hash C.BlockHeader
                     )
         -> Map C.AddressAny (Set (C.Hash C.ScriptData))
     toAddressDatums es =
         Map.fromListWith (<>)
            $ NonEmpty.toList
            $ fmap (\(addr, dh, _, _, _) -> (addr, Set.singleton dh)) es

     toDatumMap
         :: NonEmpty ( C.AddressAny
                     , C.Hash C.ScriptData
                     , Maybe C.ScriptData
                     , C.SlotNo
                     , C.Hash C.BlockHeader
                     )
         -> Map (C.Hash C.ScriptData) C.ScriptData
     toDatumMap es =
         Map.fromList
            $ catMaybes
            $ NonEmpty.toList
            $ fmap (\(_, dh, d, _, _) -> fmap (dh,) d) es

instance Queryable AddressDatumHandle where
  queryStorage
    :: Foldable f
    => QueryInterval C.ChainPoint
    -> f (StorableEvent AddressDatumHandle)
    -> AddressDatumHandle
    -> StorableQuery AddressDatumHandle
    -> IO (StorableResult AddressDatumHandle)
  queryStorage qi es (AddressDatumHandle c _) AllAddressesQuery = do
    persistedData :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo, C.Hash C.BlockHeader)] <-
      case qi of
        QEverything ->
            SQL.query c
                [r|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
                   FROM address_datums
                   LEFT JOIN datumhash_datum
                   ON datumhash_datum.datum_hash = address_datums.datum_hash
                   ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
                ()
        QInterval (C.ChainPoint _ _) (C.ChainPoint e _) -> do
            SQL.query c
                [r|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
                   FROM address_datums
                   LEFT JOIN datumhash_datum
                   ON datumhash_datum.datum_hash = address_datums.datum_hash
                   WHERE slot_no <= ?
                   ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
                (SQL.Only e)
        QInterval C.ChainPointAtGenesis (C.ChainPoint e _) ->
            SQL.query c
                [r|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
                   FROM address_datums
                   LEFT JOIN datumhash_datum
                   ON datumhash_datum.datum_hash = address_datums.datum_hash
                   WHERE slot_no <= ?
                   ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
                (SQL.Only e)
        QInterval _ C.ChainPointAtGenesis -> pure []
    let addressDatumIndexEvents = filterWithQueryInterval qi (asEvents persistedData ++ toList es)
    pure $ AllAddressesResult
         $ Set.fromList
         $ concatMap (\(AddressDatumIndexEvent addrMap _ _) -> Map.keys addrMap) addressDatumIndexEvents

  queryStorage qi es (AddressDatumHandle c _) (AddressDatumQuery q) = do
    persistedData :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo, C.Hash C.BlockHeader)] <-
      case qi of
        QEverything ->
            SQL.query c
                [r|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
                   FROM address_datums
                   LEFT JOIN datumhash_datum
                   ON datumhash_datum.datum_hash = address_datums.datum_hash
                   WHERE address = ?
                   ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
                (SQL.Only q)
        QInterval _ (C.ChainPoint _ _) ->
            -- TODO When intervals are more clearer
            -- SQL.query c
            --     [r|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
            --        FROM address_datums
            --        LEFT JOIN datumhash_datum
            --        ON datumhash_datum.datum_hash = address_datums.datum_hash
            --        WHERE address = ? AND slot_no <= ?
            --        ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
            --     (q, e)
            pure []
        QInterval _ C.ChainPointAtGenesis -> pure []

    -- IMPORTANT: Ordering is quite important here, as the `filterWithQueryInterval`
    -- function assumes events are ordered from oldest (the head) to most recent.
    let addressDatumIndexEvents = filterWithQueryInterval qi (asEvents persistedData ++ toList es)
    let (AddressDatumIndexEvent addressDatumMap datumMap cp) = fold addressDatumIndexEvents

    -- Datum hashes that are linked to an address, but do not have a corresponding datum value
    -- associated with it.
    let unresolvedDatumHashes =
            Set.toList $ fold (Map.elems addressDatumMap) `Set.difference` Map.keysSet datumMap
    datums <- forM unresolvedDatumHashes $ \dh -> do
        (datum :: Maybe DatumRow) <- listToMaybe <$> SQL.query c
          "SELECT datum_hash, datum FROM datumhash_datum WHERE datum_hash = ?" (SQL.Only dh)
        pure $ fmap (\(DatumRow _ d) -> (dh, d)) datum
    let resolvedDatumHashes = Map.fromList $ catMaybes datums

    pure $ AddressDatumResult
         $ storableEventToResult
            q
            (AddressDatumIndexEvent addressDatumMap (datumMap <> resolvedDatumHashes) cp)

    where
      storableEventToResult
          :: C.AddressAny
          -> StorableEvent AddressDatumHandle
          -> Set C.ScriptData
      storableEventToResult targetAddr (AddressDatumIndexEvent addressDatumMap datumMap _chainPoint) =
          Set.map snd
            $ Set.filter ((==) targetAddr . fst)
            $ foldMap (\(addr, datumHashes) -> Set.map (addr,) $ resolveMapKeys datumHashes datumMap)
            $ Map.toList addressDatumMap

      resolveMapKeys
          :: (Ord k, Ord v)
          => Set k
          -> Map k v
          -> Set v
      resolveMapKeys keys m =
          -- TODO Not efficient to convert back n forth between Set
          Set.fromList $ mapMaybe (\k -> Map.lookup k m) $ Set.toList keys

instance Rewindable AddressDatumHandle where
    rewindStorage
        :: C.ChainPoint
        -> AddressDatumHandle
        -> IO (Maybe AddressDatumHandle )
    rewindStorage C.ChainPointAtGenesis h@(AddressDatumHandle c _) = do
         SQL.execute_ c "DELETE FROM address_datums"
         pure $ Just h
    rewindStorage (C.ChainPoint sn _) h@(AddressDatumHandle c _) = do
         SQL.execute c "DELETE FROM address_datums WHERE slot_no > ?" (SQL.Only sn)
         pure $ Just h

instance Resumable AddressDatumHandle where
    resumeFromStorage
        :: AddressDatumHandle
        -> IO [C.ChainPoint]
    resumeFromStorage h = do
        es <- Storable.getStoredEvents h
        pure $ fmap (\(AddressDatumIndexEvent _ _ chainPoint) -> chainPoint) es
            ++ [C.ChainPointAtGenesis]

open
  :: FilePath
  -> AddressDatumDepth
  -> IO AddressDatumIndex
open dbPath (AddressDatumDepth k) = do
    c <- SQL.open dbPath
    SQL.execute_ c "PRAGMA journal_mode=WAL"
    SQL.execute_ c
        [r|CREATE TABLE IF NOT EXISTS address_datums
            ( address TEXT NOT NULL
            , datum_hash BLOB NOT NULL
            , slot_no INT NOT NULL
            , block_hash BLOB NOT NULL
            )|]
    SQL.execute_ c
        [r|CREATE TABLE IF NOT EXISTS datumhash_datum
            ( datum_hash BLOB PRIMARY KEY
            , datum BLOB
            )|]
    SQL.execute_ c
        [r|CREATE INDEX IF NOT EXISTS address_datums_index
           ON address_datums (address)|]

    emptyState k (AddressDatumHandle c k)

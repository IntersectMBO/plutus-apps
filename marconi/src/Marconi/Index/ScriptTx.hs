{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marconi.Index.ScriptTx where

import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)

import Cardano.Api (BlockHeader, ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, SlotNo (SlotNo))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
-- TODO Remove the following dependencies (and also cardano-ledger-*
-- package dependencies in cabal file) when fromShelleyBasedScript is
-- exported from cardano-node PR:
-- https://github.com/input-output-hk/cardano-node/pull/4386
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Core qualified
import Cardano.Ledger.Crypto qualified as LedgerCrypto
import Cardano.Ledger.Keys qualified as LedgerShelley
import Cardano.Ledger.Shelley.Scripts qualified as LedgerShelley
import Cardano.Ledger.ShelleyMA.Timelocks qualified as Timelock
import Marconi.Types ()
import RewindableIndex.Storable (Buffered (getStoredEvents, persistToStorage), HasPoint (getPoint),
                                 QueryInterval (QEverything, QInterval), Queryable (queryStorage),
                                 Resumable (resumeFromStorage), Rewindable (rewindStorage), StorableEvent,
                                 StorableMonad, StorablePoint, StorableQuery, StorableResult, emptyState,
                                 filterWithQueryInterval)
import RewindableIndex.Storable qualified as Storable

{- The first thing that we need to define for a new indexer is the `handler` data
   type, meant as a wrapper for the connection type (in this case the SQLite
   connection).

   However this is a very good place to add some more configurations
   that the indexer may require (for example performance tuning settings). In our
   case we add the number of events that we want to return from the on-disk buffer -}

data ScriptTxHandle = ScriptTxHandle
  { hdlConnection :: SQL.Connection
  , hdlDepth      :: Int
  }

{- The next step is to define the data types that make up the indexer. There are
   5 of these and they depend on the handle that we previously defined. We make use
   of this semantic dependency by using type and data families that connect these
   types to the `handle` that was previously defined.

   If you want to consider semantics, you can think of the `handle` type as identifying
   both the database connection type and the database structure. Thinking of it this
   way makes the reason for the dependency clearer.

   The first type we introduce is the monad in which the database (and by extension,
   the indexer) runs. -}

type instance StorableMonad ScriptTxHandle = IO

{- The next type we introduce is the type of events. Events are the data atoms that
   the indexer consumes. They depend on the `handle` because they need to eventually
   be persisted in the database, so the database has to be able to accomodate them.

   The original implementation used two spearate data structures for storing data
   in memory vs. on-disk. It has the advantage of a better usage of memory and the
   disadvantage of complicating the implementation quite a bit. I am leaving it as-is
   for now, as this is more of a tutorial implementation and complicating things
   may have some educational value. -}

data instance StorableEvent ScriptTxHandle = ScriptTxEvent
  { txScripts  :: [(TxCbor, [StorableQuery ScriptTxHandle])]
  , chainPoint :: !ChainPoint
  } deriving (Show)

{- The resume and query functionality requires a way to specify points on the chain
   from which we want to resume, or points up to which we want to query. Next we
   define the types of these points. -}

type instance StorablePoint ScriptTxHandle = ChainPoint

-- We also need to know at which slot number an event was produced.

instance HasPoint (StorableEvent ScriptTxHandle) ChainPoint where
  getPoint (ScriptTxEvent _ cp) = cp

{- Next we begin to defined the types required for running queries. Both request and
   response types will depend naturally on the structure of the database, which is
   identified by our `handle`. First, lets define the type for queries (or requests). -}

newtype instance StorableQuery ScriptTxHandle = ScriptTxAddress Shelley.ScriptHash
  deriving (Show, Eq)

-- Now, we need one more type for the query results.

newtype instance StorableResult ScriptTxHandle = ScriptTxResult [TxCbor]

-- Next, we define types required for the interaction with SQLite and the cardano
-- blocks.

newtype Depth = Depth Int

newtype TxCbor = TxCbor BS.ByteString
  deriving (Eq, Show)
  deriving newtype (SQL.ToField, SQL.FromField)

type ScriptTxIndexer = Storable.State ScriptTxHandle

-- * SQLite
data ScriptTxRow = ScriptTxRow
  { scriptAddress :: !(StorableQuery ScriptTxHandle)
  , txCbor        :: !TxCbor
  , txSlot        :: !SlotNo
  , blockHash     :: !(Hash BlockHeader)
  } deriving (Generic)

instance SQL.ToField (StorableQuery ScriptTxHandle) where
  toField (ScriptTxAddress hash)  = SQL.SQLBlob . Shelley.serialiseToRawBytes $ hash
instance SQL.FromField (StorableQuery ScriptTxHandle) where
  fromField f = SQL.fromField f >>=
      \b -> maybe cantDeserialise (return . ScriptTxAddress) $ Shelley.deserialiseFromRawBytes Shelley.AsScriptHash b
    where
      cantDeserialise = SQL.returnError SQL.ConversionFailed f "Cannot deserialise address."
instance SQL.ToField SlotNo where
  toField (SlotNo n) =  SQL.toField (fromIntegral n :: Int)
instance SQL.FromField SlotNo where
  fromField f = SlotNo <$> SQL.fromField f

instance SQL.ToRow ScriptTxRow where
  toRow o = [ SQL.toField $ scriptAddress o
            , SQL.toField $ txCbor o
            , SQL.toField $ txSlot o
            , SQL.toField $ blockHash o ]

deriving instance SQL.FromRow ScriptTxRow

instance SQL.ToField (Hash BlockHeader) where
  toField f = SQL.toField $ C.serialiseToRawBytes f

instance SQL.FromField (Hash BlockHeader) where
   fromField f =
      SQL.fromField f <&>
        fromMaybe (error "Cannot deserialise block hash") .
          C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

-- * Indexer

type Query  = StorableQuery  ScriptTxHandle
type Result = StorableResult ScriptTxHandle

toUpdate
  :: forall era . C.IsCardanoEra era
  => [C.Tx era]
  -> ChainPoint
  -> StorableEvent ScriptTxHandle
toUpdate txs = ScriptTxEvent txScripts'
  where
    txScripts' = map (\tx -> (TxCbor $ C.serialiseToCBOR tx, getTxScripts tx)) txs

getTxBodyScripts :: forall era . C.TxBody era -> [StorableQuery ScriptTxHandle]
getTxBodyScripts body = let
    hashesMaybe :: [Maybe C.ScriptHash]
    hashesMaybe = case body of
      Shelley.ShelleyTxBody shelleyBasedEra _ scripts _ _ _ ->
        flip map scripts $ \script ->
          case fromShelleyBasedScript shelleyBasedEra script of
            Shelley.ScriptInEra _ script' -> Just $ C.hashScript script'
      _ -> [] -- Byron transactions have no scripts
    hashes = catMaybes hashesMaybe :: [Shelley.ScriptHash]
  in map ScriptTxAddress hashes

getTxScripts :: forall era . C.Tx era -> [StorableQuery ScriptTxHandle]
getTxScripts (C.Tx txBody _ws) = getTxBodyScripts txBody

{- Now that all connected data types have been defined, we go on to implement some
   of the type classes required for information storage and retrieval. -}

instance Buffered ScriptTxHandle where
  {- The data is buffered in memory. When the memory buffer is filled, we need to store
     it on disk. -}
  persistToStorage
    :: Foldable f
    => f (StorableEvent ScriptTxHandle)
    -> ScriptTxHandle
    -> IO ScriptTxHandle
  persistToStorage es h = do
    let rows = foldl' (\ea e -> ea ++ flatten e) [] es
        c    = hdlConnection h
    SQL.executeMany c
      "INSERT INTO script_transactions (scriptAddress, txCbor, slotNo, blockHash) VALUES (?, ?, ?, ?)" rows
    pure h

    where
      flatten :: StorableEvent ScriptTxHandle -> [ScriptTxRow]
      flatten (ScriptTxEvent txs (ChainPoint sn hsh)) = do
        (tx, scriptAddrs) <- txs
        addr <- scriptAddrs
        pure $ ScriptTxRow { scriptAddress = addr
                           , txCbor        = tx
                           , txSlot        = sn
                           , blockHash     = hsh
                           }
      flatten _ = error "There should be no scripts in the genesis block."

  {- We want to potentially store data in two formats. The first one is similar (if
     not identical) to the format of data stored in memory; it should contain information
     that allows knowing at which point the data was generated.

     We use this first format to support rollbacks for disk data. The second format,
     which is not always necessary and does not have any predetermined structure,
     should be thought of as an aggregate of the previously produced events.

     For this indexer we don't really need an aggregate, so our "aggregate" has almost the same
     structure as the in-memory data. We pretend that there is an aggregate by
     segregating the data into two sections, by using the `hdlDiskStore` parameter. We
     take this approach because we don't want to return the entire database when this
     function is called, and we know that there is a point after which we will not
     see any rollbacks. -}

  getStoredEvents
    :: ScriptTxHandle
    -> IO [StorableEvent ScriptTxHandle]
  getStoredEvents (ScriptTxHandle c sz) = do
    sns :: [[Integer]] <-
      SQL.query c "SELECT slotNo FROM script_transactions GROUP BY slotNo ORDER BY slotNo DESC LIMIT ?" (SQL.Only sz)
    -- Take the slot number of the sz'th slot
    let sn = if null sns
                then 0
                else head . last $ take sz sns
    es <- SQL.query c "SELECT scriptAddress, txCbor, slotNo, blockHash FROM script_transactions WHERE slotNo >= ? ORDER BY slotNo DESC, txCbor, scriptAddress" (SQL.Only (sn :: Integer))
    pure $ asEvents es

-- This function recomposes the in-memory format from the database records. This
-- function expectes it's first argument to be ordered by slotNo and txCbor for the
-- proper grouping of records.
--
-- TODO: There should be an easier lensy way of doing this.
asEvents
  :: [ScriptTxRow]
  -> [StorableEvent ScriptTxHandle]
asEvents [] = []
asEvents rs@(ScriptTxRow _ _ sn hsh : _) =
   let (xs, ys) = span (\(ScriptTxRow _ _ sn' hsh') -> sn == sn' && hsh == hsh') rs
    in mkEvent xs : asEvents ys
  where
    mkEvent :: [ScriptTxRow] -> StorableEvent ScriptTxHandle
    mkEvent rs'@(ScriptTxRow _ _ sn' hsh' : _) =
       ScriptTxEvent { chainPoint = ChainPoint sn' hsh'
                     , txScripts = agScripts rs'
                     }
    mkEvent _ = error "We should always be called with a non-empty list"
    agScripts :: [ScriptTxRow] -> [(TxCbor, [StorableQuery ScriptTxHandle])]
    agScripts [] = []
    agScripts rs'@(ScriptTxRow _ tx _ _ : _) =
       let (xs, ys) = span (\(ScriptTxRow _ tx' _ _) -> tx == tx') rs'
        in (tx, map scriptAddress xs) : agScripts ys

instance Queryable ScriptTxHandle where
  queryStorage
    :: Foldable f
    => QueryInterval ChainPoint
    -> f (StorableEvent ScriptTxHandle)
    -> ScriptTxHandle
    -> StorableQuery ScriptTxHandle
    -> IO (StorableResult ScriptTxHandle)
  queryStorage qi es (ScriptTxHandle c _) q = do
    persisted :: [ScriptTxRow] <-
      case qi of
        QEverything -> SQL.query c
          "SELECT scriptAddress, txCbor, slotNo, blockHash FROM script_transactions WHERE scriptAddress = ? ORDER BY slotNo ASC, txCbor, scriptAddress" (SQL.Only q)
        QInterval _ (ChainPoint sn _) -> SQL.query c
          "SELECT scriptAddress, txCbor, slotNo, blockHash FROM script_transactions WHERE slotNo <= ? AND scriptAddress = ? ORDER BY slotNo ASC, txCbor, scriptAddress" (sn, q)
        QInterval _ ChainPointAtGenesis -> pure []
    -- Note that ordering is quite important here, as the `filterWithQueryInterval`
    -- function assumes events are ordered from oldest (the head) to most recent.
    let updates = filterWithQueryInterval qi (asEvents persisted ++ toList es)
    pure . ScriptTxResult $ filterByScriptAddress q updates

    where
      filterByScriptAddress :: StorableQuery ScriptTxHandle -> [StorableEvent ScriptTxHandle] -> [TxCbor]
      filterByScriptAddress addr updates = do
         ScriptTxEvent update _slotNo <- updates
         map fst $ filter (\(_, addrs) -> addr `elem` addrs) update

instance Rewindable ScriptTxHandle where
  rewindStorage
    :: ChainPoint
    -> ScriptTxHandle
    -> IO (Maybe ScriptTxHandle)
  rewindStorage (ChainPoint sn   _) h@(ScriptTxHandle c _) = do
     SQL.execute c "DELETE FROM script_transactions WHERE slotNo > ?" (SQL.Only sn)
     pure $ Just h
  rewindStorage ChainPointAtGenesis h@(ScriptTxHandle c _) = do
     SQL.execute_ c "DELETE FROM script_transactions"
     pure $ Just h


-- For resuming we need to provide a list of points where we can resume from.

instance Resumable ScriptTxHandle where
  resumeFromStorage h = do
    es <- Storable.getStoredEvents h
    -- The ordering here matters. The node will try to find the first point in the
    -- ledger, then move to the next and so on, so we will send the latest point
    -- first.
    pure $ map chainPoint es ++ [ChainPointAtGenesis]

open :: FilePath -> Depth -> IO ScriptTxIndexer
open dbPath (Depth k) = do
  c <- SQL.open dbPath
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS script_transactions (scriptAddress TEXT NOT NULL, txCbor BLOB NOT NULL, slotNo INT NOT NULL, blockHash BLOB NOT NULL)"
  -- Add this index for normal queries.
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS script_address ON script_transactions (scriptAddress)"
  -- Add this index for interval queries.
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS script_address_slot ON script_transactions (scriptAddress, slotNo)"
  -- This index helps with group by
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS script_grp ON script_transactions (slotNo)"
  emptyState k (ScriptTxHandle c k)

-- * Copy-paste
--
-- | TODO: Remove when the following function is exported from Cardano.Api.Script
-- PR: https://github.com/input-output-hk/cardano-node/pull/4386
fromShelleyBasedScript  :: Shelley.ShelleyBasedEra era
                        -> Cardano.Ledger.Core.Script (Shelley.ShelleyLedgerEra era)
                        -> Shelley.ScriptInEra era
fromShelleyBasedScript era script =
  case era of
    Shelley.ShelleyBasedEraShelley ->
      Shelley.ScriptInEra Shelley.SimpleScriptV1InShelley $
      Shelley.SimpleScript Shelley.SimpleScriptV1 $
      fromShelleyMultiSig script
    Shelley.ShelleyBasedEraAllegra ->
      Shelley.ScriptInEra Shelley.SimpleScriptV2InAllegra $
      Shelley.SimpleScript Shelley.SimpleScriptV2 $
      fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 script
    Shelley.ShelleyBasedEraMary ->
      Shelley.ScriptInEra Shelley.SimpleScriptV2InMary $
      Shelley.SimpleScript Shelley.SimpleScriptV2 $
      fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 script
    Shelley.ShelleyBasedEraAlonzo ->
      case script of
        Alonzo.TimelockScript s ->
          Shelley.ScriptInEra Shelley.SimpleScriptV2InAlonzo $
          Shelley.SimpleScript Shelley.SimpleScriptV2 $
          fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          Shelley.ScriptInEra Shelley.PlutusScriptV1InAlonzo $
          Shelley.PlutusScript Shelley.PlutusScriptV1 $
          Shelley.PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 _ ->
          error "fromShelleyBasedScript: PlutusV2 not supported in Alonzo era"
    Shelley.ShelleyBasedEraBabbage ->
      case script of
        Alonzo.TimelockScript s ->
          Shelley.ScriptInEra Shelley.SimpleScriptV2InBabbage $
          Shelley.SimpleScript Shelley.SimpleScriptV2 $
          fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          Shelley.ScriptInEra Shelley.PlutusScriptV1InBabbage $
          Shelley.PlutusScript Shelley.PlutusScriptV1 $
          Shelley.PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 s ->
          Shelley.ScriptInEra Shelley.PlutusScriptV2InBabbage $
          Shelley.PlutusScript Shelley.PlutusScriptV2 $
          Shelley.PlutusScriptSerialised s

  where
  fromAllegraTimelock :: Shelley.TimeLocksSupported lang
                      -> Timelock.Timelock LedgerCrypto.StandardCrypto
                      -> Shelley.SimpleScript lang
  fromAllegraTimelock timelocks = go
    where
      go (Timelock.RequireSignature kh) = Shelley.RequireSignature
                                            (Shelley.PaymentKeyHash (LedgerShelley.coerceKeyRole kh))
      go (Timelock.RequireTimeExpire t) = Shelley.RequireTimeBefore timelocks t
      go (Timelock.RequireTimeStart  t) = Shelley.RequireTimeAfter  timelocks t
      go (Timelock.RequireAllOf      s) = Shelley.RequireAllOf (map go (toList s))
      go (Timelock.RequireAnyOf      s) = Shelley.RequireAnyOf (map go (toList s))
      go (Timelock.RequireMOf      i s) = Shelley.RequireMOf i (map go (toList s))

  fromShelleyMultiSig :: LedgerShelley.MultiSig LedgerCrypto.StandardCrypto -> Shelley.SimpleScript lang
  fromShelleyMultiSig = go
    where
      go (LedgerShelley.RequireSignature kh)
                                  = Shelley.RequireSignature
                                      (Shelley.PaymentKeyHash (LedgerShelley.coerceKeyRole kh))
      go (LedgerShelley.RequireAllOf s) = Shelley.RequireAllOf (map go s)
      go (LedgerShelley.RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
      go (LedgerShelley.RequireMOf m s) = Shelley.RequireMOf m (map go s)

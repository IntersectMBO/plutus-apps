{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-
-- | Back-end support for Utxo Indexer

-- This module will create the SQL tables:
+ table: unspent_transactions
|---------+------+-------+-----------+-------+-------+------------------+--------------+------+-------------|
| Address | TxId | TxIx  | DatumHash | Datum | Value | InlineScriptHash | InlineScript | Slot | BlockNumber |
|---------+------+-------+-----------+-------+-------+------------------+--------------+------+-------------|

+ table: spent
  |------+------|--------+-----------|
  | txId | txIx | slotNo | blockHash |
  |------+------|--------+-----------|

-}
module Marconi.ChainIndex.Indexers.Utxo where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket_)
import Control.Lens.Combinators (imap)
import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty)
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
import Cardano.Api.Shelley qualified as Shelley
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (TargetAddresses, TxOut, pattern CurrentEra)
import Marconi.Core.Storable (Buffered (getStoredEvents, persistToStorage), HasPoint (getPoint),
                              QueryInterval (QEverything, QInterval), Queryable (queryStorage),
                              Resumable (resumeFromStorage), Rewindable (rewindStorage), StorableEvent, StorableMonad,
                              StorablePoint, StorableQuery, StorableResult, emptyState, filterWithQueryInterval)
import Marconi.Core.Storable qualified as Storable

type UtxoIndexer = Storable.State UtxoHandle

data UtxoHandle = UtxoHandle
  { hdlConnection :: SQL.Connection
  , hdlDpeth      :: Int
  }

newtype instance StorableQuery UtxoHandle =
  UtxoAddress C.AddressAny deriving (Show, Eq)

type QueryableAddresses = NonEmpty (StorableQuery UtxoHandle)

type instance StorableMonad UtxoHandle = IO

type instance StorablePoint UtxoHandle = C.ChainPoint

newtype Depth = Depth Int

data Utxo = Utxo
  { _address          :: C.AddressAny
  , _txId             :: !C.TxId
  , _txIx             :: !C.TxIx
  , _datum            :: Maybe C.ScriptData
  , _datumHash        :: Maybe (C.Hash C.ScriptData)
  , _value            :: C.Value
  , _inlineScript     :: Maybe Shelley.ScriptInAnyLang -- ^ Reference script
  , _inlineScriptHash :: Maybe C.ScriptHash
  } deriving (Show, Eq, Generic)

$(makeLenses ''Utxo)

instance Ord Utxo where
  left <= right
    =   _txId left <= _txId right
    &&  _txIx left <= _txIx right

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
  toJSON (Utxo addr tId tIx dtum dtumHash val scrpt scrptHash) = object
    [ "address"           .= addr
    , "txId"              .= tId
    , "txIx"              .= tIx
    , "datum"             .= dtum
    , "datumHash"         .= dtumHash
    , "value"             .= val
    -- Uses ToJSON instance of cardano-api which serialises using the 'C.HasTextEnvelope' typeclass.
    , "inlineScript"      .= scrpt
    , "inlineScriptHash"  .= scrptHash
    ]

data UtxoRow = UtxoRow
  { _urUtxo      :: Utxo
  , _urSlotNo    :: C.SlotNo
  , _urBlockHash :: C.Hash C.BlockHeader
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

eventIsBefore :: StorableEvent UtxoHandle  -> C.ChainPoint -> Bool
eventIsBefore (UtxoEvent _ _ (C.ChainPoint _ slot)) (C.ChainPoint _ slot') =  slot < slot'
eventIsBefore _ _                                                          = False

instance Semigroup (StorableEvent UtxoHandle) where
  e@(UtxoEvent u i cp) <> (UtxoEvent u' i' cp') =
    if cp == cp' then
      UtxoEvent (Set.union u u') (Set.union i i') cp
    else e -- do not combine events from different chain points

instance Monoid (StorableEvent UtxoHandle) where
  mempty = UtxoEvent mempty mempty C.ChainPointAtGenesis

data Spent = Spent
  { _sTxInTxId  :: !C.TxId                 -- ^ from TxIn, containts the Spent txId
  , _sTxInTxIx  :: !C.TxIx
  , _sSlotNo    :: !C.SlotNo
  , _sBlockHash :: !(C.Hash C.BlockHeader)
  } deriving (Show, Eq)

$(makeLenses ''Spent)

instance Ord Spent where
  compare spent spent' =
      case  (spent ^. sTxInTxId) `compare` (spent' ^. sTxInTxId) of
        EQ  -> (spent ^. sTxInTxIx) `compare` (spent' ^. sTxInTxIx)
        neq -> neq

instance HasPoint (StorableEvent UtxoHandle) C.ChainPoint where
  getPoint (UtxoEvent _ _ cp) = cp

---------------------------------------------------------------------------------
--------------- sql mappings unspent_transactions and Spent tables -------------
---------------------------------------------------------------------------------
instance ToRow UtxoRow where
  toRow u =
    [ toField (u ^. urUtxo . address)
    , toField (u ^. urUtxo . txId)
    , toField (u ^. urUtxo . txIx)
    , toField (u ^. urUtxo . datum)
    , toField (u ^. urUtxo . datumHash)
    , toField (u ^. urUtxo . value)
    , toField (u ^. urUtxo . inlineScript)
    , toField (u ^. urUtxo . inlineScriptHash)
    , toField (u ^. urSlotNo)
    , toField (u ^. urBlockHash)
    ]

instance FromRow UtxoRow where
  fromRow = UtxoRow
      <$> (Utxo <$> field <*> field <*> field <*> field
                <*> field <*> field <*> field <*> field)
      <*> field <*> field

instance FromRow Spent where
  fromRow = Spent <$> field <*> field <*> field <*> field

instance ToRow Spent where
  toRow s =
    [ toField (s ^. sTxInTxId)
    , toField (s ^. sTxInTxIx)
    , toField (s ^. sSlotNo)
    , toField (s ^. sBlockHash)
    ]

---------------------------------------------------------------------------------
------------------------------- End sql mappings ---------------------------------

-- | Open a connection to DB, and create resources
-- The parameter ((k + 1) * 2) specifies the amount of events that are buffered.
-- The larger the number, the more RAM the indexer uses. However, we get improved SQL
-- queries due to batching more events together.
open
  :: FilePath -- ^ sqlite file path
  -> Depth    -- ^ The Depth parameter k, the larger K, the more RAM the indexer uses
  -> IO UtxoIndexer
open dbPath (Depth k) = do
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
                      , slotNo INT
                      , blockHash BLOB
                      , UNIQUE (txId, txIx))|]
  SQL.execute_ c [r|CREATE TABLE IF NOT EXISTS spent
                      ( txInTxId TEXT PRIMARY KEY NOT NULL
                      , txInTxIx INT NOT NULL
                      , slotNo INT NOT NULL
                      , blockHash BLOB NOT NULL
                      , UNIQUE (txInTxId, txInTxIx))|]

  SQL.execute_ c [r|CREATE INDEX IF NOT EXISTS
                      spent_slotNo ON spent (slotNo)|]
  SQL.execute_ c [r|CREATE INDEX IF NOT EXISTS
                      unspent_transaction_address ON unspent_transactions (address)|]
  emptyState k (UtxoHandle c k)

eventToSpent :: StorableEvent UtxoHandle -> [Spent]
eventToSpent (UtxoEvent _ txIns cp) = case cp of
  C.ChainPointAtGenesis -> [] -- There are no Spent in the Genesis block
  (C.ChainPoint sn bh)  ->  fmap (\(C.TxIn txid txix) -> Spent txid txix sn bh) . Set.toList $ txIns

-- | Store UtxoEvents
-- Events are stored in memory and flushed to SQL, disk, when memory buffer has reached capacity
--
instance Buffered UtxoHandle where
  persistToStorage
    :: Foldable f
    => f (StorableEvent UtxoHandle) -- ^ ues to store
    -> UtxoHandle -- ^ handler for storing events
    -> IO UtxoHandle
  persistToStorage events h = do
    let rows = concatMap eventsToRows events
        spents = concatMap eventToSpent events
        c = hdlConnection h
    bracket_
        (SQL.execute_ c "BEGIN")
        (SQL.execute_ c "COMMIT")
        (concurrently_
         (unless
          (null rows)
          (SQL.executeMany c
            [r|INSERT OR REPLACE INTO unspent_transactions
                ( address, txId, txIx, datum, datumHash
                , value, inlineScript, inlineScriptHash
                , slotNo, blockHash
                ) VALUES (?,?,?,?,?,?,?,?,?,?)|] rows))
         (unless
          (null spents)
          (SQL.executeMany c
           [r|INSERT OR REPLACE INTO spent
               ( txInTxId, txInTxIx, slotNo, blockHash
               ) VALUES (?,?,?,?)|] spents)))
    -- We want to perform vacuum about once every 100 * buffer ((k + 1) * 2)
    rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
    when (rndCheck == 42) $ do
      SQL.execute_ c [r|DELETE FROM unspent_transactions
                          WHERE unspent_transactions.rowid IN
                            (SELECT unspent_transactions.rowid
                             FROM unspent_transactions
                               LEFT JOIN spent ON
                                 unspent_transactions.txId = spent.txInTxId
                               AND unspent_transactions.txIx = spent.txInTxIx
                             WHERE spent.txInTxId IS NOT NULL)|]
      SQL.execute_ c "VACUUM"
    pure h

  getStoredEvents :: UtxoHandle -> IO [StorableEvent UtxoHandle]
  getStoredEvents (UtxoHandle c sz) =  do
    sns <- SQL.query c
        [r|SELECT slotNo FROM unspent_transactions
           GROUP BY slotNo
           ORDER BY slotNo DESC
           LIMIT ?|] (SQL.Only sz) :: IO [[Integer]]

    -- Take the slot number of the sz'th slot
    let sn = if null sns
                then 0
                else head . last $ take sz sns

    rows :: [UtxoRow] <- SQL.query c
        [r|SELECT address, txId, txIx , datum , datumHash
                  , value, inlineScript, inlineScriptHash
                  , slotNo, blockHash
           FROM unspent_transactions
           WHERE slotNo >= ?
           GROUP by slotNo
           ORDER BY slotNo ASC|] (SQL.Only (sn :: Integer))

    rowsToEvents (getTxIns c) rows

getTxIns :: SQL.Connection -> C.ChainPoint -> IO (Set C.TxIn)
getTxIns _ C.ChainPointAtGenesis = pure Set.empty
getTxIns c (C.ChainPoint slotNo blockHash) = do
  let bh = C.serialiseToRawBytes blockHash
  ins :: [(C.TxId, C.TxIx)] <- SQL.query c
    "SELECT txInTxId, txInTxIx FROM spent WHERE blockHash=? and slotNo=?" (bh, slotNo)
  pure . Set.fromList . fmap (uncurry C.TxIn) $ ins

-- | Convert UtxoRows to UtxoEvents
rowsToEvents
  :: (C.ChainPoint -> IO (Set C.TxIn)) -- ^ Function that knows how to get corresponding TxIn
  -> [UtxoRow] -- ^ UtxoRows, source
  -> IO [StorableEvent UtxoHandle] -- ^ UtxoEvents
rowsToEvents f rows = traverse eventFromRow  rows <&> foldl' g []
  where
    g :: [StorableEvent UtxoHandle] -> StorableEvent UtxoHandle -> [StorableEvent UtxoHandle]
    g es e = case findIndex' e es of
      Just n  ->
        take n es <> [es !! n <> e] <> drop (n+1) es
      Nothing -> e : es

    findIndex' :: StorableEvent UtxoHandle -> [StorableEvent UtxoHandle] -> Maybe Int
    findIndex' x xs = elemIndex (ueChainPoint x) (ueChainPoint <$> xs)

    eventFromRow :: UtxoRow -> IO (StorableEvent UtxoHandle)
    eventFromRow utxoRow = do
      ins <- f (C.ChainPoint (utxoRow ^. urSlotNo) (utxoRow ^. urBlockHash) )
      pure $ UtxoEvent
        { ueUtxos  = Set.singleton (utxoRow ^. urUtxo)
        , ueInputs = ins
        , ueChainPoint = C.ChainPoint (utxoRow ^. urSlotNo) (utxoRow ^. urBlockHash)
        }

-- | Convert 'UtxoEvent's to 'UtxoRow's.
eventsToRows :: StorableEvent UtxoHandle -> [UtxoRow]
eventsToRows (UtxoEvent utxos _ (C.ChainPoint slotno hsh)) =
  fmap (\u -> UtxoRow u slotno hsh) . Set.toList $ utxos
eventsToRows (UtxoEvent _ _ C.ChainPointAtGenesis) = []

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
addressFilteredRows addr = concatMap eventsToRows . eventsAtAddress addr . toList

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
  queryStorage qi es (UtxoHandle c _) q@(UtxoAddress addr) = do
    persisted <- case qi of
      QEverything -> SQL.query c
          [r|SELECT u.address, u.txId, u.txIx, u.datum, u.datumHash
                  , u.value, u.inlineScript, u.inlineScriptHash
                  , u.slotNo, u.blockHash
             FROM unspent_transactions u
             LEFT JOIN spent s
             ON u.txId = s.txInTxId
                AND u.txId IS NOT NULL
                AND u.txIx = s.txInTxIx
             WHERE u.address = ?
             ORDER BY u.slotNo ASC|] (Only (C.serialiseToRawBytes addr))
      QInterval _ (C.ChainPoint sn _) -> SQL.query c
          [r|SELECT u.address, u.txId, u.txIx , u.datum , u.datumHash
                  , u.value, u.inlineScript, u.inlineScriptHash
                  , u.slotNo, u.blockHash
             FROM unspent_transactions u
             LEFT JOIN spent s ON
                    u.txId = s.txInTxId
                AND u.txIx = s.txInTxIx
             WHERE
                    u.slotNo <= ?
                AND u.txId IS NOT NULL
                AND u.address = ?
             ORDER BY u.slotNo ASC|] (sn, C.serialiseToRawBytes addr)
      QInterval _ C.ChainPointAtGenesis -> pure []

    es' <- queryBuffer qi es q (getTxIns c)
    let rows = concatMap eventsToRows es'
    pure . UtxoResult $ persisted <> rows

-- | Query the in incomming UtxoEvent
queryBuffer
  :: Foldable f
  => QueryInterval C.ChainPoint
  -> f (StorableEvent UtxoHandle)         -- ^ Utxo events
  -> StorableQuery UtxoHandle             -- ^ Query
  -> (C.ChainPoint -> IO (Set C.TxIn))    -- ^ Function that know how to get TxIns fron ChainPoint
  -> IO [StorableEvent UtxoHandle]
queryBuffer qi es q f
  = filterSpent                   -- filter out the utxo that have had Spent
  . filterWithQueryInterval qi    -- filter for the given slot interval
  . eventsAtAddress q             -- query for the given address
  $ es
  where
    filterSpent :: [StorableEvent UtxoHandle] -> IO [StorableEvent UtxoHandle]
    filterSpent = traverse unspentEvents

    unspentEvents :: StorableEvent UtxoHandle -> IO (StorableEvent UtxoHandle)
    unspentEvents e = do
      (txIns :: Set C.TxIn) <- f (ueChainPoint e)
      let utxos = Set.filter (\(Utxo _ id' ix _ _ _ _ _) ->
                                notElem (C.TxIn id' ix) txIns) . ueUtxos $ e
      pure e {ueUtxos = utxos}

instance Rewindable UtxoHandle where
  rewindStorage :: C.ChainPoint -> UtxoHandle -> IO (Maybe UtxoHandle)
  rewindStorage (C.ChainPoint sn _) h@(UtxoHandle c _) = do
    SQL.execute c "DELETE FROM unspent_transactions WHERE slotNo > ?" (SQL.Only sn)
    SQL.execute c "DELETE FROM spent WHERE slotNo > ?" (SQL.Only sn)
    pure $ Just h
  rewindStorage C.ChainPointAtGenesis _ = pure Nothing

-- For resuming we need to provide a list of points where we can resume from.
instance Resumable UtxoHandle where
  resumeFromStorage h = do
    es <- Storable.getStoredEvents h
    -- The ordering here matters. The node will try to find the first point in the
    -- ledger, then move to the next and so on, so we will send the latest point
    -- first.
    pure $ map ueChainPoint es ++ [C.ChainPointAtGenesis]

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

-- | Extract Utxos payload from Cardano Transaction
-- Note, these Utxos will be decorated with additional data points to make a UtxoEvent
getUtxos :: (C.IsCardanoEra era) => Maybe TargetAddresses -> C.Tx era -> [Utxo]
getUtxos maybeTargetAddresses (C.Tx txBody@(C.TxBody C.TxBodyContent {C.txOuts}) _) =
  either (const []) addressDiscriminator (getUtxos' txOuts)
  where
    addressDiscriminator :: [Utxo] -> [Utxo]
    addressDiscriminator = case maybeTargetAddresses of
      Just targetAddresses -> filter (isAddressInTarget targetAddresses)
      Nothing              -> id

    getUtxos' :: C.IsCardanoEra era => [C.TxOut C.CtxTx era] -> Either C.EraCastError [Utxo]
    getUtxos' = fmap (imap txoutToUtxo) . traverse (C.eraCast CurrentEra)

    txoutToUtxo :: Int -> TxOut -> Utxo
    txoutToUtxo ix out = Utxo {..}
      where
        _txIx = C.TxIx $ fromIntegral ix
        _txId = C.getTxId txBody
        C.TxOut address' value' datum' refScript = out
        _address = toAddr address'
        _value = C.txOutValueToValue value'
        (_datum, _datumHash) = getScriptDataAndHash datum'
        (_inlineScript, _inlineScriptHash) =
            getRefScriptAndHash refScript

-- | get the inlineScript and inlineScriptHash
--
getRefScriptAndHash
  :: Shelley.ReferenceScript era
  -> (Maybe Shelley.ScriptInAnyLang, Maybe C.ScriptHash)
getRefScriptAndHash refScript = case refScript of
  Shelley.ReferenceScriptNone -> (Nothing, Nothing)
  Shelley.ReferenceScript _ s@(Shelley.ScriptInAnyLang(C.SimpleScriptLanguage C.SimpleScriptV1) script) ->
      ( Just  s
      , Just . C.hashScript $ script)
  Shelley.ReferenceScript _ s@(Shelley.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV2) script)->
    ( Just s
    , Just . C.hashScript $ script)
  Shelley.ReferenceScript _ s@(Shelley.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script)->
    ( Just s
    , Just . C.hashScript $ script)
  Shelley.ReferenceScript _ s@(Shelley.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script)->
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
        C.TxIn (u ^. txId) (u ^. txIx) `Set.member` txIns

-- | Extract UtxoEvents from Cardano Transactions
getUtxoEvents
  :: C.IsCardanoEra era
  => Maybe TargetAddresses -- ^ target addresses to filter for
  -> [C.Tx era]
  -> C.ChainPoint
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents maybeTargetAddresses txs cp =
  let
    utxos = Set.fromList $ concatMap (getUtxos maybeTargetAddresses) txs
    ins = foldl' Set.union Set.empty $ getInputs <$> txs
  in UtxoEvent utxos ins cp

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
isAddressInTarget :: TargetAddresses -> Utxo -> Bool
isAddressInTarget targetAddresses utxo =
    case utxo ^. address  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

mkQueryableAddresses :: TargetAddresses -> QueryableAddresses
mkQueryableAddresses = fmap (UtxoAddress . C.toAddressAny)

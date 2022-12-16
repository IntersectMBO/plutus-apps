{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Marconi.Indexers where

import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Lens (view)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Data.List (findIndex, foldl1', intersect)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Streaming.Prelude qualified as S

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, ScriptData, SlotNo, Tx (Tx), chainPointToSlotNo)
import Cardano.Api qualified as C
import "cardano-api" Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Control.Concurrent.STM.TMVar (TMVar)
import Marconi.Index.Datum (DatumIndex)
import Marconi.Index.Datum qualified as Datum
import Marconi.Index.ScriptTx qualified as ScriptTx
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types (TargetAddresses)

import RewindableIndex.Index.VSplit qualified as Ix
import RewindableIndex.Storable qualified as Storable

-- DatumIndexer
getDatums :: BlockInMode CardanoMode -> [(SlotNo, (Hash ScriptData, ScriptData))]
getDatums (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) = concatMap extractDatumsFromTx txs
    where
        extractDatumsFromTx :: Tx era -> [(SlotNo, (Hash ScriptData, ScriptData))]
        extractDatumsFromTx (Tx txBody _) =
            fmap (slotNo,)
            . Map.assocs
            . scriptDataFromCardanoTxBody
            $ txBody

scriptDataFromCardanoTxBody :: C.TxBody era -> Map (Hash ScriptData) ScriptData
scriptDataFromCardanoTxBody (Shelley.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ dats _) _ _) =
    extractData dats
  where
    extractData :: Alonzo.TxDats era -> Map (Hash ScriptData) ScriptData
    extractData (Alonzo.TxDats' xs) =
      Map.fromList
      . fmap ((\x -> (C.hashScriptData x, x)) . Shelley.fromAlonzoData)
      . Map.elems
      $ xs
scriptDataFromCardanoTxBody _ = mempty

{- | The way we synchronise channel consumption is by waiting on a QSemN for each
     of the spawn indexers to finish processing the current event.

     The channel is used to transmit the next event to the listening indexers. Note
     that even if the channel is unbound it will actually only ever hold one event
     because it will be blocked until the processing of the event finishes on all
     indexers.

     The indexer count is where we save the number of running indexers so we know for
     how many we are waiting.
-}
data Coordinator = Coordinator
  { _channel      :: TChan (ChainSyncEvent (BlockInMode CardanoMode))
  , _barrier      :: QSemN
  , _indexerCount :: Int
  }

initialCoordinator :: Int -> IO Coordinator
initialCoordinator indexerCount =
  Coordinator <$> newBroadcastTChanIO
              <*> newQSemN 0
              <*> pure indexerCount

-- Currently this `a` should stand for [ChainPoint], but I think eventually it should
-- be constrained to a set of indexer type classes viewed through an MVar. This will
-- allow indexers to be shared and queried safely.
type Worker = Coordinator -> FilePath -> IO [Storable.StorablePoint ScriptTx.ScriptTxHandle]

datumWorker :: Worker
datumWorker Coordinator{_barrier, _channel} path = do
  ix <- Datum.open path (Datum.Depth 2160)
  workerChannel <- atomically . dupTChan $ _channel
  void . forkIO $ innerLoop workerChannel ix
  pure [ChainPointAtGenesis]
  where
    innerLoop :: TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> DatumIndex -> IO ()
    innerLoop ch index = do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward blk _ct ->
          Ix.insert (getDatums blk) index >>= innerLoop ch
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          innerLoop ch $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex (any (\(s, _) -> s < slot)) events
              Ix.rewind offset index

-- | does the transaction contain a targetAddress
isInTargetTxOut
    :: TargetAddresses        -- ^ non empty list of target address
    -> C.TxOut C.CtxTx era    -- ^  a cardano transaction out that contains an address
    -> Bool
isInTargetTxOut targetAddresses (C.TxOut address _ _ _) = case address of
    (C.AddressInEra  (C.ShelleyAddressInEra _) addr) -> addr `elem` targetAddresses
    _                                                -> False

utxoWorker
    :: (Utxo.UtxoIndex -> IO Utxo.UtxoIndex)    -- ^ CPS function used in the queryApi thread, needs to be non-blocking
    -> Maybe TargetAddresses                    -- ^ Target addresses to filter for
    -> Worker
utxoWorker indexerCallback maybeTargetAddresses Coordinator{_barrier, _channel} path = do
    ix <- Utxo.open path (Utxo.Depth 2160)
    workerChannel <- atomically . dupTChan $ _channel
    void . forkIO $ innerLoop workerChannel ix
    pure [ChainPointAtGenesis]
  where
    innerLoop :: TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> Utxo.UtxoIndex -> IO ()
    innerLoop ch index = do
      signalQSemN _barrier 1
      void $ indexerCallback index -- refresh the query STM/CPS with new storage pointers/counters state
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo _ blkNo) txs) _) _ct ->
            case Utxo.getUtxoEvents maybeTargetAddresses slotNo blkNo txs of
                  Just us ->  Ix.insert us index  >>= innerLoop ch
                  _       -> innerLoop ch index
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          innerLoop ch $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex  (\u -> (u ^. Utxo.utxoEventSlotNo) < slot) events
              Ix.rewind offset index

scriptTxWorker_
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> ScriptTx.Depth
  -> Coordinator -> TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> FilePath -> IO (IO (), MVar ScriptTx.ScriptTxIndexer)
scriptTxWorker_ onInsert depth Coordinator{_barrier} ch path = do
  indexer <- ScriptTx.open path depth
  mIndexer <- newMVar indexer
  pure (loop mIndexer, mIndexer)
  where
    loop :: MVar ScriptTx.ScriptTxIndexer -> IO ()
    loop index = do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo hsh _) txs :: Block era) _ :: BlockInMode CardanoMode) _ct -> do
          let u = ScriptTx.toUpdate txs (ChainPoint slotNo hsh)
          modifyMVar_ index (Storable.insert u)
          void $ onInsert u
          loop index

        RollBackward cp _ct -> do
          modifyMVar_ index $ \ix -> fromMaybe ix <$> Storable.rewind cp ix
          loop index

scriptTxWorker
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> Worker
scriptTxWorker onInsert coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <- scriptTxWorker_ onInsert (ScriptTx.Depth 2160) coordinator workerChannel path
  void . forkIO $ loop
  readMVar ix >>= Storable.resumeFromStorage . view Storable.handle

newtype UtxoQueryTMVar = UtxoQueryTMVar
    { unUtxoIndex  :: TMVar Utxo.UtxoIndex      -- ^ for query thread to access in-memory utxos
    }

filterIndexers
  :: Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe TargetAddresses
  -> [(Worker, FilePath)]
filterIndexers utxoPath datumPath scriptTxPath maybeTargetAddresses =
  mapMaybe liftMaybe pairs
  where
    liftMaybe (worker, maybePath) = case maybePath of
      Just path -> Just (worker, path)
      _         -> Nothing
    pairs =
        [ (utxoWorker pure maybeTargetAddresses, utxoPath)
        , (datumWorker, datumPath)
        , (scriptTxWorker (\_ -> pure []), scriptTxPath)
        ]

startIndexers
  :: [(Worker, FilePath)]
  -> IO (ChainPoint, Coordinator)
startIndexers indexers = do
  coordinator <- initialCoordinator $ length indexers
  startingPoint <- mapM (\(ix, fp) -> ix coordinator fp) indexers
  -- We want to use the minimum value across all indexers.
  pure ( foldl1' max $ foldl1' intersect startingPoint
       , coordinator )

mkIndexerStream
  :: Coordinator
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO ()
mkIndexerStream coordinator = S.foldM_ step initial finish
  where
    initial :: IO Coordinator
    initial = pure coordinator

    step :: Coordinator -> ChainSyncEvent (BlockInMode CardanoMode) -> IO Coordinator
    step c@Coordinator{_barrier, _indexerCount, _channel} event = do
      waitQSemN _barrier _indexerCount
      atomically $ writeTChan _channel event
      pure c

    finish :: Coordinator -> IO ()
    finish _ = pure ()

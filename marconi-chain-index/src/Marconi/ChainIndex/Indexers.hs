{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Marconi.ChainIndex.Indexers where

import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Lens (view, (&))
import Control.Lens.Operators ((^.))
import Control.Monad (forever, void)
import Control.Monad.Trans.Class (lift)
import Data.List (findIndex, foldl1', intersect)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Database.SQLite.Simple qualified as SQL
import Streaming.Prelude qualified as S

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, ScriptData, SlotNo, Tx (Tx), chainPointToSlotNo)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Cardano.Streaming qualified as CS
import Marconi.ChainIndex.Indexers.AddressDatum (AddressDatumDepth (AddressDatumDepth), AddressDatumHandle,
                                                 AddressDatumIndex)
import Marconi.ChainIndex.Indexers.AddressDatum qualified as AddressDatum
import Marconi.ChainIndex.Indexers.Datum (DatumIndex)
import Marconi.ChainIndex.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Indexers.EpochStakepoolSize qualified as EpochStakepoolSize
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Indexers.ScriptTx qualified as ScriptTx
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)

import Marconi.Core.Index.VSplit qualified as Ix
import Marconi.Core.Storable qualified as Storable

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

-- The points should/could provide shared access to the indexers themselves. The result
-- is a list of points (rather than just one) since it offers more resume possibilities
-- to the node (in the unlikely case there were some rollbacks during downtime).
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

utxoWorker_
  :: (Utxo.UtxoIndexer -> IO ())  -- ^ callback function used in the queryApi thread, needs to be non-blocking
  -> Utxo.Depth
  -> Maybe TargetAddresses                       -- ^ Target addresses to filter for
  -> Coordinator -> TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> FilePath -> IO (IO (), MVar Utxo.UtxoIndexer)
utxoWorker_ callback depth maybeTargetAddresses Coordinator{_barrier} ch path = do
  ix <- Utxo.open path depth
  mIndexer <- newMVar ix
  pure (loop mIndexer, mIndexer)
  where
    loop :: MVar Utxo.UtxoIndexer -> IO ()
    loop index = do
      signalQSemN _barrier 1
      readMVar index >>= callback -- refresh the query STM/CPS with new storage pointers/counters state
      event <- atomically . readTChan $ ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo hsh _) txs) _) _ct -> do
            let utxoEvents = Utxo.getUtxoEvents maybeTargetAddresses txs (C.ChainPoint slotNo hsh)
            modifyMVar_ index (Storable.insert utxoEvents)
            loop index

        RollBackward cp _ct -> do
          modifyMVar_ index $ \ix -> fromMaybe ix <$> Storable.rewind cp ix
          loop index

utxoWorker
  :: (Utxo.UtxoIndexer -> IO ())  -- ^ CPS function used in the queryApi thread, needs to be non-blocking
  -> Maybe TargetAddresses                       -- ^ Target addresses to filter for
  -> Worker
utxoWorker callback maybeTargetAddresses coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <- utxoWorker_ callback (Utxo.Depth 2160) maybeTargetAddresses coordinator workerChannel path
  void . forkIO $ loop
  readMVar ix >>= Storable.resumeFromStorage . view Storable.handle

addressDatumWorker
  :: (Storable.StorableEvent AddressDatumHandle -> IO [()])
  -> Maybe TargetAddresses
  -> Worker
addressDatumWorker onInsert targetAddresses coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <-
      addressDatumWorker_
        onInsert
        targetAddresses
        (AddressDatumDepth 2160)
        coordinator
        workerChannel
        path
  void . forkIO $ loop
  readMVar ix >>= Storable.resumeFromStorage . view Storable.handle

addressDatumWorker_
    :: (Storable.StorableEvent AddressDatumHandle -> IO [()])
    -> Maybe TargetAddresses  -- ^ Target addresses to filter for
    -> AddressDatumDepth
    -> Coordinator
    -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
    -> FilePath
    -> IO (IO (), MVar AddressDatumIndex)
addressDatumWorker_ onInsert targetAddresses depth Coordinator{_barrier} ch path = do
    index <- AddressDatum.open path depth
    mIndex <- newMVar index
    pure (innerLoop mIndex, mIndex)
  where
    innerLoop :: MVar AddressDatumIndex -> IO ()
    innerLoop index = do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo bh _) txs) _) _ -> do
            -- TODO Redo. Inefficient filtering
            let addressFilter =
                    fmap (\targetAddrs -> \addr -> addr `elem` targetAddrs)
                         targetAddresses
                addressDatumIndexEvent =
                    AddressDatum.toAddressDatumIndexEvent addressFilter txs (C.ChainPoint slotNo bh)
            modifyMVar_ index (Storable.insert addressDatumIndexEvent)
            void $ onInsert addressDatumIndexEvent
            innerLoop index
        RollBackward cp _ct -> do
          modifyMVar_ index $ \ix -> fromMaybe ix <$> Storable.rewind cp ix
          innerLoop index

-- * ScriptTx indexer

scriptTxWorker_
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> ScriptTx.Depth
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO (), MVar ScriptTx.ScriptTxIndexer)
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

-- * Epoch stakepool size indexer

epochStakepoolSizeWorker :: FilePath -> Worker
epochStakepoolSizeWorker configPath Coordinator{_barrier,_channel} dbPath = do
  tchan <- atomically $ dupTChan _channel
  let
    -- Read blocks from TChan, emit them as a stream.
    chainSyncEvents :: S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO ()
    chainSyncEvents = do
      lift $ signalQSemN _barrier 1
      S.yield =<< (lift $ atomically $ readTChan tchan)
      chainSyncEvents

  dbCon <- SQL.open dbPath
  (env, initialLedgerStateHistory) <- CS.getEnvAndInitialLedgerStateHistory configPath
  let
    indexer = chainSyncEvents
      & CS.foldLedgerState env initialLedgerStateHistory C.QuickValidation
      & EpochStakepoolSize.toEvents
      & EpochStakepoolSize.sqlite dbCon

  void . forkIO $ S.effects indexer
  pure [ChainPointAtGenesis]

-- * Mint/burn indexer

mintBurnWorker_
  :: Int
  -> (MintBurn.TxMintEvent -> IO ())
  -> Coordinator -> TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> FilePath -> IO (IO b, MVar MintBurn.MintBurnIndexer)
mintBurnWorker_ bufferSize onInsert Coordinator{_barrier} ch dbPath = do
  indexerMVar <- newMVar =<< MintBurn.open dbPath bufferSize
  let
    loop = forever $ do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward blockInMode _ct
          | Just event' <- MintBurn.toUpdate blockInMode -> do
              modifyMVar_ indexerMVar $ Storable.insert $ MintBurn.MintBurnEvent event'
              void $ onInsert event'
          | otherwise -> pure ()
        RollBackward cp _ct ->
          modifyMVar_ indexerMVar $ \ix -> fromMaybe ix <$> Storable.rewind cp ix
  pure (loop, indexerMVar)

mintBurnWorker :: (MintBurn.TxMintEvent -> IO ()) -> Worker
mintBurnWorker onInsert coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <- mintBurnWorker_ 2160 onInsert coordinator workerChannel path
  void $ forkIO loop
  readMVar ix >>= Storable.resumeFromStorage . view Storable.handle

filterIndexers
  :: Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe TargetAddresses
  -> Maybe FilePath
  -> [(Worker, FilePath)]
filterIndexers
    utxoPath
    addressDatumPath
    datumPath
    scriptTxPath
    epochStakepoolSizePath
    mintBurnPath
    maybeTargetAddresses
    maybeConfigPath =
  mapMaybe liftMaybe pairs
  where
    liftMaybe (worker, maybePath) = fmap (worker,) maybePath
    epochStakepoolSizeIndexer = case maybeConfigPath of
      Just configPath -> [(epochStakepoolSizeWorker configPath, epochStakepoolSizePath)]
      Nothing         -> []

    pairs =
        [ (utxoWorker (\_ -> pure ()) maybeTargetAddresses, utxoPath)
        , (addressDatumWorker (\_ -> pure []) maybeTargetAddresses, addressDatumPath)
        , (datumWorker, datumPath)
        , (scriptTxWorker (\_ -> pure []), scriptTxPath)
        , (mintBurnWorker (\_ -> pure ()), mintBurnPath)
        ] <> epochStakepoolSizeIndexer

startIndexers
  :: [(Worker, FilePath)]
  -> IO ([ChainPoint], Coordinator)
startIndexers indexers = do
  coordinator <- initialCoordinator $ length indexers
  startingPoints <- mapM (\(ix, fp) -> ix coordinator fp) indexers
  -- We want to use the set of points that are common to all indexers
  -- giving priority to recent ones.
  pure ( foldl1' intersect startingPoints
       , coordinator
       )

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

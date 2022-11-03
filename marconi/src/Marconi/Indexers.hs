{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}

module Marconi.Indexers where

import Control.Concurrent (forkIO)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (bracket_)
import Control.Lens.Combinators (imap)
import Control.Lens.Operators ((&), (^.))
import Control.Monad (void)
import Data.Foldable (foldl')
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Streaming.Prelude qualified as S

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode, Hash, ScriptData,
                    SlotNo, Tx (Tx), chainPointToSlotNo)
import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as Byron
import "cardano-api" Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Streaming (ChainSyncEvent (RollBackward, RollForward))

import Marconi.Index.Datum (DatumIndex)
import Marconi.Index.Datum qualified as Datum
import Marconi.Index.ScriptTx qualified as ScriptTx
import Marconi.Index.Utxo (TxOut, UtxoIndex, UtxoUpdate (UtxoUpdate, _inputs, _outputs, _slotNo))
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types (TargetAddresses, TxOutRef, pattern CurrentEra, txOutRef)

import RewindableIndex.Index.VSplit qualified as Ix

-- DatumIndexer
getDatums :: BlockInMode CardanoMode -> [(SlotNo, (Hash ScriptData, ScriptData))]
getDatums (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) = concatMap extractDatumsFromTx txs
  where
    extractData :: Alonzo.TxDats era -> Map (Hash ScriptData) ScriptData
    extractData (Alonzo.TxDats' xs) =
      Map.fromList
      . fmap ((\x -> (C.hashScriptData x, x)) . Shelley.fromAlonzoData)
      . Map.elems $ xs

    scriptDataFromCardanoTxBody :: C.TxBody era -> Map (Hash ScriptData) ScriptData
    scriptDataFromCardanoTxBody Byron.ByronTxBody {} = mempty
    scriptDataFromCardanoTxBody (Shelley.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) = mempty
    scriptDataFromCardanoTxBody
      (Shelley.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ dats _) _ _) =
          extractData dats

    extractDatumsFromTx
      :: Tx era
      -> [(SlotNo, (Hash ScriptData, ScriptData))]
    extractDatumsFromTx (Tx txBody _) =
      let hashes = Map.assocs $ scriptDataFromCardanoTxBody txBody
       in map (slotNo,) hashes


-- UtxoIndexer
getOutputs
  :: C.IsCardanoEra era
  => Maybe TargetAddresses
  -> C.Tx era
  -> Maybe [ (TxOut, TxOutRef) ]
getOutputs maybeTargetAddresses (C.Tx txBody@(C.TxBody C.TxBodyContent{C.txOuts}) _) =
    do
        let indexersFilter = case maybeTargetAddresses of
                Just targetAddresses -> filter (isInTargetTxOut targetAddresses)
                _                    -> id -- no filtering is applied
        outs  <- either (const Nothing) Just
            . traverse (C.eraCast CurrentEra)
            . indexersFilter
            $ txOuts
        pure $ outs & imap
            (\ix out -> (out, txOutRef (C.getTxId txBody) (C.TxIx $ fromIntegral ix)))
getInputs
  :: C.Tx era
  -> Set C.TxIn
getInputs (C.Tx (C.TxBody C.TxBodyContent{C.txIns, C.txScriptValidity, C.txInsCollateral}) _) =
  let inputs = case txScriptValidityToScriptValidity txScriptValidity of
        C.ScriptValid -> fst <$> txIns
        C.ScriptInvalid -> case txInsCollateral of
                                C.TxInsCollateralNone     -> []
                                C.TxInsCollateral _ txins -> txins
  in Set.fromList inputs

getUtxoUpdate
  :: C.IsCardanoEra era
  => SlotNo
  -> [C.Tx era]
  -> Maybe TargetAddresses
  -> UtxoUpdate
getUtxoUpdate slot txs maybeAddresses =
  let ins  = foldl' Set.union Set.empty $ getInputs <$> txs
      outs = concat . catMaybes $ getOutputs maybeAddresses <$> txs
  in  UtxoUpdate { _inputs  = ins
                 , _outputs = outs
                 , _slotNo  = slot
                 }

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

type Worker = Coordinator -> TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> FilePath -> IO ()

datumWorker :: Worker
datumWorker Coordinator{_barrier} ch path = Datum.open path (Datum.Depth 2160) >>= innerLoop
  where
    innerLoop :: DatumIndex -> IO ()
    innerLoop index = do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward blk _ct ->
          Ix.insert (getDatums blk) index >>= innerLoop
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          innerLoop $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex (any (\(s, _) -> s < slot)) events
              Ix.rewind offset index

-- | does the transaction contain a targetAddress
isInTargetTxOut
    :: TargetAddresses              -- ^ non empty list of target address
    -> C.TxOut C.CtxTx era    -- ^  a cardano transaction out that contains an address
    -> Bool
isInTargetTxOut targetAddresses (C.TxOut address _ _ _) = case address of
    (C.AddressInEra  (C.ShelleyAddressInEra _) addr) -> addr `elem` targetAddresses
    _                                                -> False

queryAwareUtxoWorker
    :: QSemN            -- ^ Semaphore indicating of inflight database queries
    -> TargetAddresses  -- ^ Target addresses to filter for
    -> Worker
queryAwareUtxoWorker qsem targetAddresses Coordinator{_barrier} ch path =
   Utxo.open path (Utxo.Depth 2160) >>= innerLoop
  where
    innerLoop :: UtxoIndex -> IO ()
    innerLoop index = bracket_   -- Note, Exceptions here will propegate to main thread and abend the application
        (waitQSemN qsem 1) (signalQSemN qsem 1) $
        do
            signalQSemN _barrier 1
            event <- atomically $ readTChan ch
            case event of
                RollForward (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) _ct -> do
                    let utxoRow = getUtxoUpdate slotNo txs (Just targetAddresses)
                    Ix.insert utxoRow index >>= innerLoop
                RollBackward cp _ct -> do
                    events <- Ix.getEvents (index ^. Ix.storage)
                    innerLoop $
                        fromMaybe index $ do
                            slot   <- chainPointToSlotNo cp
                            offset <- findIndex  (\u -> (u ^. Utxo.slotNo) < slot) events
                            Ix.rewind offset index


utxoWorker :: Maybe TargetAddresses -> Worker
utxoWorker maybeTargetAddresses Coordinator{_barrier} ch path =
    Utxo.open path (Utxo.Depth 2160) >>= innerLoop
  where
    innerLoop :: UtxoIndex -> IO ()
    innerLoop index = do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) _ct -> do
          let utxoRow = getUtxoUpdate slotNo txs maybeTargetAddresses
          Ix.insert utxoRow index >>= innerLoop
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          innerLoop $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex  (\u -> (u ^. Utxo.slotNo) < slot) events
              Ix.rewind offset index

scriptTxWorker_
  :: (ScriptTx.ScriptTxIndex -> ScriptTx.ScriptTxUpdate -> IO [()])
  -> ScriptTx.Depth
  -> Coordinator -> TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> FilePath -> IO (IO (), ScriptTx.ScriptTxIndex)
scriptTxWorker_ onInsert depth Coordinator{_barrier} ch path = do
  indexer <- ScriptTx.open onInsert path depth
  pure (loop indexer, indexer)
  where
    loop :: ScriptTx.ScriptTxIndex -> IO ()
    loop index = do
      signalQSemN _barrier 1
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo _ _) txs :: Block era) _ :: BlockInMode CardanoMode) _ct -> do
          Ix.insert (ScriptTx.toUpdate txs slotNo) index >>= loop
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          loop $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex  (\u -> ScriptTx.slotNo u < slot) events
              Ix.rewind offset index

scriptTxWorker
  :: (ScriptTx.ScriptTxIndex -> ScriptTx.ScriptTxUpdate -> IO [()])
  -> Worker
scriptTxWorker onInsert coordinator ch path = do
  (loop, _) <- scriptTxWorker_ onInsert (ScriptTx.Depth 2160) coordinator ch path
  loop

combinedIndexer
  :: Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe TargetAddresses
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO ()
combinedIndexer utxoPath datumPath scriptTxPath maybeTargetAddresses = combineIndexers remainingIndexers
  where
    liftMaybe (worker, maybePath) = case maybePath of
      Just path -> Just (worker, path)
      _         -> Nothing
    pairs =
        [
            (utxoWorker maybeTargetAddresses, utxoPath)
            , (datumWorker, datumPath)
            , (scriptTxWorker (\_ _ -> pure []), scriptTxPath)
        ]
    remainingIndexers = mapMaybe liftMaybe pairs

combineIndexers
  :: [(Worker, FilePath)]
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO ()
combineIndexers indexers = S.foldM_ step initial finish
  where
    initial :: IO Coordinator
    initial = do
      coordinator <- initialCoordinator $ length indexers
      mapM_ (uncurry (forkIndexer coordinator)) indexers
      pure coordinator

    step :: Coordinator -> ChainSyncEvent (BlockInMode CardanoMode) -> IO Coordinator
    step c@Coordinator{_barrier, _indexerCount, _channel} event = do
      waitQSemN _barrier _indexerCount
      atomically $ writeTChan _channel event
      pure c

    finish :: Coordinator -> IO ()
    finish _ = pure ()

forkIndexer :: Coordinator -> Worker -> FilePath -> IO ()
forkIndexer coordinator worker path = do
  ch <- atomically . dupTChan $ _channel coordinator
  void . forkIO . worker coordinator ch $ path

-- | Duplicated from cardano-api (not exposed in cardano-api)
-- This function should be removed when marconi will depend on a cardano-api version that has accepted this PR:
-- https://github.com/input-output-hk/cardano-node/pull/4569
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone                = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity


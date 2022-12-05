{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module Marconi.Indexers where

import Control.Concurrent (forkIO)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Lens.Combinators (imap)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Data.Foldable (foldl')
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Streaming.Prelude qualified as S

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode, Hash, ScriptData,
                    SlotNo, Tx (Tx), chainPointToSlotNo)
import Cardano.Api qualified as C
import "cardano-api" Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Control.Concurrent.STM.TMVar (TMVar)
import Marconi.Index.Datum (DatumIndex)
import Marconi.Index.Datum qualified as Datum
import Marconi.Index.ScriptTx qualified as ScriptTx
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types (TargetAddresses, TxOut, pattern CurrentEra)

import RewindableIndex.Index.VSplit qualified as Ix


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

-- UtxoIndexer
getUtxos
  :: (C.IsCardanoEra era)
  => Maybe TargetAddresses
  -> C.Tx era
  -> [Utxo.Utxo]
getUtxos maybeTargetAddresses (C.Tx txBody@(C.TxBody C.TxBodyContent{C.txOuts}) _) =
    either (const []) addressDiscriminator (getUtxos' txOuts)
    where
        addressDiscriminator :: [Utxo.Utxo] -> [Utxo.Utxo]
        addressDiscriminator = case maybeTargetAddresses of
            Just targetAddresses -> filter ( isAddressInTarget targetAddresses)
            _                    -> id

        getUtxos' :: C.IsCardanoEra era => [C.TxOut C.CtxTx  era] -> Either C.EraCastError [Utxo.Utxo]
        getUtxos' = fmap (imap txoutToUtxo) . traverse (C.eraCast CurrentEra)

        txoutToUtxo :: Int -> TxOut -> Utxo.Utxo
        txoutToUtxo  ix out =
            let
                _utxoTxIx = C.TxIx $ fromIntegral ix
                _utxoTxId = C.getTxId txBody
                (C.TxOut address' value' datum' refScript ) = out
                _utxoAddress = Utxo.toAddr address'
                _utxoValue = C.txOutValueToValue value'
                _utxoDatumHash = case datum' of
                    (C.TxOutDatumHash _ d ) -> Just d
                    _                       ->  Nothing
                _utxoDatum = case datum' of
                    (C.TxOutDatumInline _ d ) -> Just d
                    _                         ->  Nothing
                (_utxoInlineScript, _utxoInlineScriptHash) = case refScript of
                    Shelley.ReferenceScriptNone -> (Nothing, Nothing)
                    Shelley.ReferenceScript _
                        (Shelley.ScriptInAnyLang
                            (C.SimpleScriptLanguage (C.SimpleScriptV1) )
                            script) -> (Just . C.serialiseToCBOR $ script, Just . C.hashScript $ script)    -- TODO this is hack to get pass build , Just . C.hashScript $ script)
                    Shelley.ReferenceScript _
                        (Shelley.ScriptInAnyLang
                            (C.SimpleScriptLanguage (C.SimpleScriptV2) )
                            script) -> (Just . C.serialiseToCBOR $ script, Just . C.hashScript $ script)    -- TODO this is hack to get pass build , Just . C.hashScript $ script)
                    Shelley.ReferenceScript _
                        (Shelley.ScriptInAnyLang
                            (C.PlutusScriptLanguage (C.PlutusScriptV1) )
                            script) -> (Just . C.serialiseToCBOR $ script, Just . C.hashScript $ script)    -- TODO this is hack to get pass build , Just . C.hashScript $ script)
                    Shelley.ReferenceScript _
                        (Shelley.ScriptInAnyLang
                            (C.PlutusScriptLanguage (C.PlutusScriptV2) )
                            script) -> (Just . C.serialiseToCBOR $ script, Just . C.hashScript $ script)    -- TODO this is hack to get pass build , Just . C.hashScript $ script)
            in
                Utxo.Utxo {..}

getUtxoEvents
  :: C.IsCardanoEra era
  => Maybe TargetAddresses              -- ^ target addresses to filter for
  -> C.SlotNo
  -> C.BlockNo
  -> [C.Tx era]
  -> Maybe Utxo.UtxoEvent               -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents maybeTargetAddresses slotNo blkNo txs =
    let
        utxos = (concat . fmap (getUtxos maybeTargetAddresses) $ txs )
        ins  = foldl' Set.union Set.empty $ getInputs <$> txs
    in
        if null utxos then
            Nothing
        else
            Just (Utxo.UtxoEvent utxos ins slotNo blkNo)

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
    :: TargetAddresses        -- ^ non empty list of target addres
    -> C.TxOut C.CtxTx era    -- ^  a cardano transaction out that contains an address
    -> Bool
isInTargetTxOut targetAddresses (C.TxOut address _ _ _) = case address of
    (C.AddressInEra  (C.ShelleyAddressInEra _) addr) -> addr `elem` targetAddresses
    _                                                -> False

-- | does the transaction contain a targetAddress
isAddressInTarget
    :: TargetAddresses
    -> Utxo.Utxo
    -> Bool
isAddressInTarget targetAddresses utxo =
    case (utxo ^. Utxo.utxoAddress) of
        C.AddressByron _      -> False
        C.AddressShelley addr -> addr `elem` targetAddresses

utxoWorker
    :: (Utxo.UtxoIndex -> IO Utxo.UtxoIndex)    -- ^ CPS function used in the queryApi thread, needs to be non-blocking
    -> Maybe TargetAddresses                    -- ^ Target addresses to filter for
    -> Worker
utxoWorker indexerCallback maybeTargetAddresses Coordinator{_barrier} ch path =
    Utxo.open path (Utxo.Depth 2160) >>= indexerCallback >>= innerLoop
  where
    innerLoop :: Utxo.UtxoIndex -> IO ()
    innerLoop index = do
      signalQSemN _barrier 1
      void $ indexerCallback index -- refresh the query STM/CPS with new storage pointers/counters state
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo _ blkNo) txs) _) _ct ->
            case (getUtxoEvents maybeTargetAddresses slotNo blkNo txs) of
                  Just us ->  Ix.insert (us) index  >>= innerLoop
                  _       -> innerLoop index
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          innerLoop $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex  (\u -> (u ^. Utxo.utxoEventSlotNo) < slot) events
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

newtype UtxoQueryTMVar = UtxoQueryTMVar
    { unUtxoIndex  :: TMVar Utxo.UtxoIndex      -- ^ for query thread to access in-memory utxos
    }

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
            (utxoWorker pure maybeTargetAddresses, utxoPath)
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

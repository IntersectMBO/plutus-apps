{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
-- |
module Plutus.PAB.Core.ContractInstance.BlockchainEnv(
  startNodeClient
  , processMockBlock
  , processChainSyncEvent
  ) where

import Cardano.Api (BlockHeader (BlockHeader), BlockInMode (..), ChainPoint (..),
                    ConsensusModeParams (CardanoModeParams), EpochSlots (..), LocalNodeConnectInfo (..), NetworkId,
                    QueryInMode (QuerySystemStart), getBlockHeader, queryNodeLocalState)
import Cardano.Api qualified as C
import Cardano.Node.Types (NodeMode (..))
import Cardano.Protocol.Socket.Client (ChainSyncEvent (..))
import Cardano.Protocol.Socket.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Data.Map qualified as Map
import Data.Monoid (Last (..), Sum (..))
import Ledger (Address (..), Block, Slot (..), TxId (..))
import Plutus.PAB.Core.ContractInstance.STM (BlockchainEnv (..), InstanceClientEnv (..), InstancesState,
                                             OpenTxOutProducedRequest (..), OpenTxOutSpentRequest (..),
                                             emptyBlockchainEnv)
import Plutus.PAB.Core.ContractInstance.STM qualified as S
import Plutus.Trace.Emulator.ContractInstance (IndexedBlock (..), indexBlock)

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Tracer (nullTracer)
import Data.Aeson (toJSON)
import Data.Aeson.OneLine (renderValue)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, maybeToList)
import Data.Text qualified as T
import Ledger.TimeSlot (SlotConfig)
import Plutus.ChainIndex (BlockNumber (..), ChainIndexTx (..), ChainIndexTxOutputs (..), Depth (..),
                          InsertUtxoFailed (..), InsertUtxoSuccess (..), Point (..), ReduceBlockCountResult (..),
                          RollbackFailed (..), RollbackResult (..), Tip (..), TxConfirmedState (..), TxIdState (..),
                          TxOutBalance, TxValidity (..), UtxoIndex, UtxoState (..), blockId, citxTxId, fromOnChainTx,
                          insert, reduceBlockCount, tipAsPoint, utxoState)
import Plutus.ChainIndex.Compatibility (fromCardanoBlockHeader, fromCardanoPoint, toCardanoPoint)
import Plutus.ChainIndex.TxIdState qualified as TxIdState
import Plutus.ChainIndex.TxOutBalance qualified as TxOutBalance
import Plutus.ChainIndex.UtxoState (viewTip)
import Plutus.Contract.CardanoAPI (fromCardanoTx)

-- | Connect to the node and write node updates to the blockchain
--   env.
startNodeClient ::
     FilePath -- ^ Socket to connect to node
  -> NodeMode -- ^ Whether to connect to real node or mock node
  -> Maybe Int  -- ^ How much history do we remember for rollbacks
  -> SlotConfig -- ^ Slot config used by the node
  -> NetworkId -- ^ Cardano network ID
  -> Point
  -> InstancesState -- ^ In-memory state of running contract instances
  -> IO BlockchainEnv
startNodeClient socket mode rollbackHistory slotConfig networkId resumePoint instancesState = do
    env <- STM.atomically $ emptyBlockchainEnv rollbackHistory slotConfig
    case mode of
      MockNode -> do
        void $ MockClient.runChainSync socket slotConfig
            (\chainSyncEvent slot -> handleSyncAction Nothing $ processMockBlock instancesState env chainSyncEvent slot)
      AlonzoNode -> do
        ensureSocket socket networkId
        let resumePoints = maybeToList $ toCardanoPoint resumePoint
        void $ Client.runChainSync socket nullTracer slotConfig networkId resumePoints
          (\chainSyncEvent -> handleSyncAction (Just chainSyncEvent) $ processChainSyncEvent instancesState env chainSyncEvent)

    pure env

-- | Test the connection to the node socket.
ensureSocket :: FilePath -> NetworkId -> IO ()
ensureSocket localNodeSocketPath localNodeNetworkId =
  let
    localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
  in
    void
      $ queryNodeLocalState LocalNodeConnectInfo{..} Nothing QuerySystemStart

-- | Deal with sync action failures from running this STM action. For now, we
-- deal with them by simply calling `error`; i.e. the application exits.
handleSyncAction :: Maybe ChainSyncEvent -> STM (Either SyncActionFailure (Slot, BlockNumber)) -> IO ()
handleSyncAction event action = do
  result <- STM.atomically action
  let
    toPoint (Resume p)                        = fromCardanoPoint p
    toPoint (RollForward (BlockInMode b _) _) =
      let
        BlockHeader s h _ = getBlockHeader b
      in
        fromCardanoPoint $ ChainPoint s h
    toPoint (RollBackward p _)                = fromCardanoPoint p
  case result of
    Left err -> putStrLn $ "handleSyncAction failed with: " <> show err
    Right (Slot s, BlockNumber n) -> do
      when ((n `mod` 100) == 0) $ do
        putStrLn $ "slot = " <> show s <> "; block = " <> show n
        case event of
          Just e  -> putStrLn $ T.unpack $ renderValue $ toJSON (toPoint e)
          Nothing -> pure ()
  either (error . show) (const $ pure ()) result

updateInstances :: IndexedBlock -> InstanceClientEnv -> STM ()
updateInstances IndexedBlock{ibUtxoSpent, ibUtxoProduced} InstanceClientEnv{ceUtxoSpentRequests, ceUtxoProducedRequests} = do
  forM_ (Map.intersectionWith (,) ibUtxoSpent ceUtxoSpentRequests) $ \(onChainTx, requests) ->
    traverse (\OpenTxOutSpentRequest{osrSpendingTx} -> STM.tryPutTMVar osrSpendingTx onChainTx) requests
  forM_ (Map.intersectionWith (,) ibUtxoProduced ceUtxoProducedRequests) $ \(txns, requests) ->
    traverse (\OpenTxOutProducedRequest{otxProducingTxns} -> STM.tryPutTMVar otxProducingTxns txns) requests

blockAndSlot :: BlockchainEnv -> STM (Slot, BlockNumber)
blockAndSlot BlockchainEnv{beCurrentBlock, beCurrentSlot} =
  (,) <$> STM.readTVar beCurrentSlot <*> STM.readTVar beCurrentBlock

-- | Process a chain sync event that we receive from the alonzo node client
processChainSyncEvent
  :: InstancesState
  -> BlockchainEnv
  -> ChainSyncEvent
  -> STM (Either SyncActionFailure (Slot, BlockNumber))
processChainSyncEvent instancesState blockchainEnv event = do
  case event of
    Resume _ -> Right <$> blockAndSlot blockchainEnv
    RollForward (BlockInMode (C.Block header transactions) era) tip ->
      do
        beTip blockchainEnv `STM.writeTVar` tip
        case era of
          -- Unfortunately, we need to pattern match again all eras because
          -- 'processBlock' has the constraints 'C.IsCardanoEra era', but not
          -- 'C.BlockInMode'.
          C.ByronEraInCardanoMode   -> processBlock instancesState header blockchainEnv transactions era
          C.ShelleyEraInCardanoMode -> processBlock instancesState header blockchainEnv transactions era
          C.AllegraEraInCardanoMode -> processBlock instancesState header blockchainEnv transactions era
          C.MaryEraInCardanoMode    -> processBlock instancesState header blockchainEnv transactions era
          C.AlonzoEraInCardanoMode  -> processBlock instancesState header blockchainEnv transactions era
    RollBackward chainPoint _ -> runRollback blockchainEnv chainPoint


data SyncActionFailure
  = RollbackFailure RollbackFailed
  | InsertUtxoStateFailure InsertUtxoFailed
  deriving (Show)

-- | Roll back the chain to the given ChainPoint and slot.
runRollback :: BlockchainEnv -> ChainPoint -> STM (Either SyncActionFailure (Slot, BlockNumber))
runRollback env@BlockchainEnv{beCurrentSlot, beTxChanges, beTxOutChanges} chainPoint = do
  currentSlot <- STM.readTVar beCurrentSlot
  txIdStateIndex <- STM.readTVar beTxChanges
  txOutBalanceStateIndex <- STM.readTVar beTxOutChanges

  let point = fromCardanoPoint chainPoint
      rs    = TxIdState.rollback point txIdStateIndex
      rs'   = TxOutBalance.rollback point txOutBalanceStateIndex
      -- Check to see if the rollback is just through a sequence of empty blocks ending at the tip.
      emptyRollBack =
           point > tipAsPoint (viewTip txIdStateIndex)
        && pointSlot point <= currentSlot

  if emptyRollBack
    then Right <$> blockAndSlot env
    else case rs of
           Left e                                -> pure $ Left (RollbackFailure e)
           Right RollbackResult{rolledBackIndex=rolledBackTxIdStateIndex} ->
             case rs' of
               Left e' -> pure $ Left (RollbackFailure e')
               Right RollbackResult{rolledBackIndex=rolledBackTxOutBalanceStateIndex} -> do
                 STM.writeTVar beTxChanges rolledBackTxIdStateIndex
                 STM.writeTVar beTxOutChanges rolledBackTxOutBalanceStateIndex
                 Right <$> blockAndSlot env

-- | Get transaction ID and validity from a transaction.
txEvent :: ChainIndexTx -> (TxId, TxOutBalance, TxValidity)
txEvent tx =
  let validity = case tx of ChainIndexTx { _citxOutputs = ValidTx _ } -> TxValid
                            ChainIndexTx { _citxOutputs = InvalidTx } -> TxInvalid
   in (view citxTxId tx, TxOutBalance.fromTx tx, validity)

-- | Update the blockchain env. with changes from a new block of cardano
--   transactions in any era
processBlock :: forall era. C.IsCardanoEra era
             => InstancesState
             -> C.BlockHeader
             -> BlockchainEnv
             -> [C.Tx era]
             -> C.EraInMode era C.CardanoMode
             -> STM (Either SyncActionFailure (Slot, BlockNumber))
processBlock instancesState header env transactions era = do
  let C.BlockHeader (C.SlotNo slot) _ _ = header
  STM.writeTVar (beCurrentSlot env) (fromIntegral slot)
  if null transactions
     then Right <$> blockAndSlot env
     else do
        let tip = fromCardanoBlockHeader header
            -- We ignore cardano transactions that we couldn't convert to
            -- our 'ChainIndexTx'.
            ciTxs = catMaybes (either (const Nothing) Just . fromCardanoTx era <$> transactions)

        instEnv <- S.instancesClientEnv instancesState
        updateInstances (indexBlockAgnosticToStake ciTxs) instEnv

        updateTransactionState tip env (txEvent <$> ciTxs)

-- | Index the block both by the address and by the address without its stake credential.
--   This makes it possible to watch any payments to the payment credential, regardless of
--   the stake credential.
indexBlockAgnosticToStake :: [ChainIndexTx] -> IndexedBlock
indexBlockAgnosticToStake ciTxs =
  let
    discardStake (Address credential _) = Address credential Nothing
    ib = indexBlock ciTxs
    produced = ibUtxoProduced ib
    producedWithoutStake = Map.mapKeys discardStake produced
  in
    ib {ibUtxoProduced = produced `Map.union` producedWithoutStake}


-- | For the given transactions, perform the updates in the 'TxIdState', and
-- also record that a new block has been processed.
updateTransactionState
  :: Foldable t
  => Tip
  -> BlockchainEnv
  -> t (TxId, TxOutBalance, TxValidity)
  -> STM (Either SyncActionFailure (Slot, BlockNumber))
updateTransactionState tip env@BlockchainEnv{beRollbackHistory, beTxChanges, beTxOutChanges, beCurrentBlock} xs = do
    txIdStateIndex <- STM.readTVar beTxChanges
    let txIdState = _usTxUtxoData $ utxoState txIdStateIndex
    txUtxoBalanceIndex <- STM.readTVar beTxOutChanges
    let txUtxoBalance = _usTxUtxoData $ utxoState txUtxoBalanceIndex
    blockNumber <- STM.readTVar beCurrentBlock
    let txIdState' = foldl' (insertNewTx blockNumber) txIdState xs
        txIdStateInsert  = insert (UtxoState txIdState' tip) txIdStateIndex
        txUtxoBalance' = txUtxoBalance <> foldMap (\(_, b, _) -> b) xs
        txUtxoBalanceInsert = insert (UtxoState txUtxoBalance' tip) txUtxoBalanceIndex

    case (txIdStateInsert, txUtxoBalanceInsert) of
      (Right InsertUtxoSuccess{newIndex=newTxIdState}, Right InsertUtxoSuccess{newIndex=newTxOutBalance}) -> do -- TODO: Get tx out status another way
        STM.writeTVar beTxChanges    $ trimIx beRollbackHistory newTxIdState
        STM.writeTVar beTxOutChanges $ trimIx beRollbackHistory newTxOutBalance
        STM.writeTVar beCurrentBlock (succ blockNumber)
        Right <$> blockAndSlot env
      (Left e, _) -> pure $ Left $ InsertUtxoStateFailure e
      (_, Left e) -> pure $ Left $ InsertUtxoStateFailure e
    where
      trimIx :: Monoid a => Maybe Int -> UtxoIndex a -> UtxoIndex a
      trimIx Nothing                uix = uix
      trimIx (Just rollbackHistory) uix =
        case reduceBlockCount (Depth rollbackHistory) uix of
          BlockCountNotReduced          -> uix
          ReduceBlockCountResult uix' _ -> uix'

insertNewTx :: BlockNumber -> TxIdState -> (TxId, TxOutBalance, TxValidity) -> TxIdState
insertNewTx blockNumber TxIdState{txnsConfirmed, txnsDeleted} (txi, _, txValidity) =
  let newConfirmed = txnsConfirmed & at txi ?~ newV
   in TxIdState (txnsConfirmed <> newConfirmed) txnsDeleted
    where
      -- New state; we rely on the monoid instance to make this agree with any
      -- existing transactions already present (but perhaps rolled back.)
      newV = TxConfirmedState
              { timesConfirmed = Sum 1
              , blockAdded     = Last (Just blockNumber)
              , validity       = Last (Just txValidity)
              }

-- | Go through the transactions in a block, updating the 'BlockchainEnv'
--   when any interesting addresses or transactions have changed.
processMockBlock :: InstancesState -> BlockchainEnv -> Block -> Slot -> STM (Either SyncActionFailure (Slot, BlockNumber))
processMockBlock instancesState env@BlockchainEnv{beCurrentSlot, beCurrentBlock} transactions slot = do
  lastSlot <- STM.readTVar beCurrentSlot
  when (slot > lastSlot) $ do
    STM.writeTVar beCurrentSlot slot

  if null transactions
     then do
       result <- (,) <$> STM.readTVar beCurrentSlot <*> STM.readTVar beCurrentBlock
       pure $ Right result
     else do
      blockNumber <- STM.readTVar beCurrentBlock

      instEnv <- S.instancesClientEnv instancesState
      updateInstances (indexBlock $ fmap fromOnChainTx transactions) instEnv

      let tip = Tip { tipSlot = slot
                    , tipBlockId = blockId transactions
                    , tipBlockNo = blockNumber
                    }

      updateTransactionState tip env (txEvent <$> fmap fromOnChainTx transactions)

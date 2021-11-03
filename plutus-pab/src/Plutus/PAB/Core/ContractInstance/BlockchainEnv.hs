{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
-- |
module Plutus.PAB.Core.ContractInstance.BlockchainEnv(
  startNodeClient
  , processMockBlock
  , processChainSyncEvent
  , garbageCollect
  ) where

import Cardano.Api (BlockInMode (..), ChainPoint (..), NetworkId)
import qualified Cardano.Api as C
import Cardano.Node.Types (NodeMode (..))
import Cardano.Protocol.Socket.Client (ChainSyncEvent (..))
import qualified Cardano.Protocol.Socket.Client as Client
import qualified Cardano.Protocol.Socket.Mock.Client as MockClient
import qualified Data.Map as Map
import Data.Monoid (Last (..), Sum (..))
import Ledger (Block, Slot (..), TxId (..))
import Plutus.PAB.Core.ContractInstance.STM (BlockchainEnv (..), InstanceClientEnv (..), InstancesState,
                                             OpenTxOutProducedRequest (..), OpenTxOutSpentRequest (..),
                                             emptyBlockchainEnv)
import qualified Plutus.PAB.Core.ContractInstance.STM as S
import Plutus.Trace.Emulator.ContractInstance (IndexedBlock (..), indexBlock)

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Tracer (nullTracer)
import Data.Foldable (foldl', traverse_)
import Data.Maybe (catMaybes)
import Ledger.TimeSlot (SlotConfig)
import Plutus.ChainIndex (BlockNumber (..), ChainIndexTx (..), ChainIndexTxOutputs (..), Depth (..),
                          InsertUtxoFailed (..), InsertUtxoSuccess (..), RollbackFailed (..), RollbackResult (..),
                          Tip (..), TxConfirmedState (..), TxIdState (..), TxOutBalance, TxValidity (..),
                          UtxoState (..), blockId, citxTxId, dropOlder, fromOnChainTx, insert, utxoState)
import Plutus.ChainIndex.Compatibility (fromCardanoBlockHeader, fromCardanoPoint)
import Plutus.ChainIndex.TxIdState (chainConstant)
import qualified Plutus.ChainIndex.TxIdState as TxIdState
import qualified Plutus.ChainIndex.TxOutBalance as TxOutBalance
import Plutus.Contract.CardanoAPI (fromCardanoTx)

-- | Connect to the node and write node updates to the blockchain
--   env.
startNodeClient ::
  FilePath -- ^ Socket to connect to node
  -> NodeMode -- ^ Whether to connect to real node or mock node
  -> SlotConfig -- ^ Slot config used by the node
  -> NetworkId -- ^ Cardano network ID
  -> InstancesState -- ^ In-memory state of running contract instances
  -> IO BlockchainEnv
startNodeClient socket mode slotConfig networkId instancesState = do
    env <- STM.atomically emptyBlockchainEnv
    case mode of
      MockNode -> do
        void $ MockClient.runChainSync socket slotConfig
            (\block slot -> handleSyncAction $ processMockBlock instancesState env block slot)
      AlonzoNode -> do
        let resumePoints = []
        void $ Client.runChainSync socket nullTracer slotConfig networkId resumePoints
          (\block slot -> handleSyncAction $ processChainSyncEvent env block slot)
    pure env

-- | Deal with sync action failures from running this STM action. For now, we
-- deal with them by simply calling `error`; i.e. the application exits.
handleSyncAction :: STM (Either SyncActionFailure (Slot, BlockNumber)) -> IO ()
handleSyncAction action = do
  result <- STM.atomically action
  case result of
    Left err -> putStrLn $ "handleSyncAction failed with: " <> show err
    Right (Slot s, BlockNumber n) -> do
      let recentSlot = 41511045 -- TODO: This is a relatively recent slot number
                                -- we start logging from here to avoid spamming the terminal
                                -- should be removed when we have better logging to report
                                -- on the PAB sync status
      if (n `mod` 100_000 == 0 && n > 0) || (s >= recentSlot)
        then do
          putStrLn $ "Current block: " <> show n <> ". Current slot: " <> show s
        else pure ()
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
processChainSyncEvent :: BlockchainEnv -> ChainSyncEvent -> Slot -> STM (Either SyncActionFailure (Slot, BlockNumber))
processChainSyncEvent blockchainEnv event _slot = do
  result <- case event of
              Resume _ -> Right <$> blockAndSlot blockchainEnv
              RollForward  (BlockInMode (C.Block header transactions) era) _ ->
                case era of
                  -- Unfortunately, we need to pattern match again all eras because
                  -- 'processBlock' has the constraints 'C.IsCardanoEra era', but not
                  -- 'C.BlockInMode'.
                  C.ByronEraInCardanoMode   -> processBlock header blockchainEnv transactions era
                  C.ShelleyEraInCardanoMode -> processBlock header blockchainEnv transactions era
                  C.AllegraEraInCardanoMode -> processBlock header blockchainEnv transactions era
                  C.MaryEraInCardanoMode    -> processBlock header blockchainEnv transactions era
                  C.AlonzoEraInCardanoMode  -> processBlock header blockchainEnv transactions era
              RollBackward chainPoint _ -> runRollback blockchainEnv chainPoint
  flip traverse_ result $ \(_, BlockNumber n) -> do
    if n `mod` 5_000 == 0
      then garbageCollect blockchainEnv
      else pure ()
  pure result

data SyncActionFailure
  = RollbackFailure RollbackFailed
  | InsertUtxoStateFailure InsertUtxoFailed
  deriving (Show)

-- | Roll back the chain to the given ChainPoint and slot.
runRollback :: BlockchainEnv -> ChainPoint -> STM (Either SyncActionFailure (Slot, BlockNumber))
runRollback env@BlockchainEnv{beTxChanges, beTxOutChanges} chainPoint = do
  txIdStateIndex <- STM.readTVar beTxChanges
  txOutBalanceStateIndex <- STM.readTVar beTxOutChanges

  let point = fromCardanoPoint chainPoint
      rs    = TxIdState.rollback point txIdStateIndex
      rs'   = TxOutBalance.rollback point txOutBalanceStateIndex

  case rs of
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

-- | Drop all entries in the beTxChanges field that are older than
-- 'chainConstant'.
garbageCollect :: BlockchainEnv -> STM ()
garbageCollect BlockchainEnv{beTxChanges, beTxOutChanges, beCurrentBlock} = do
  txIdStateIndex <- STM.readTVar beTxChanges
  txOutBalanceStateIndex <- STM.readTVar beTxOutChanges
  currentBlock   <- STM.readTVar beCurrentBlock

  let targetBlock = BlockNumber $ unBlockNumber currentBlock - fromIntegral (unDepth chainConstant)

  STM.writeTVar beTxChanges $ dropOlder targetBlock txIdStateIndex
  STM.writeTVar beTxOutChanges $ dropOlder targetBlock txOutBalanceStateIndex

-- | Update the blockchain env. with changes from a new block of cardano
--   transactions in any era
processBlock :: forall era. C.IsCardanoEra era
             => C.BlockHeader
             -> BlockchainEnv
             -> [C.Tx era]
             -> C.EraInMode era C.CardanoMode
             -> STM (Either SyncActionFailure (Slot, BlockNumber))
processBlock header env transactions era = do
  let C.BlockHeader (C.SlotNo slot) _ _ = header
  STM.writeTVar (beCurrentSlot env) (fromIntegral slot)
  if null transactions
     then Right <$> blockAndSlot env
     else do
        let tip = fromCardanoBlockHeader header
            -- We ignore cardano transactions that we couldn't convert to
            -- our 'ChainIndexTx'.
            ciTxs = catMaybes (either (const Nothing) Just . fromCardanoTx era <$> transactions)
        updateTransactionState tip env (txEvent <$> ciTxs)

-- | For the given transactions, perform the updates in the 'TxIdState', and
-- also record that a new block has been processed.
updateTransactionState
  :: Foldable t
  => Tip
  -> BlockchainEnv
  -> t (TxId, TxOutBalance, TxValidity)
  -> STM (Either SyncActionFailure (Slot, BlockNumber))
updateTransactionState tip env@BlockchainEnv{beTxChanges, beTxOutChanges, beCurrentBlock} xs = do
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
        STM.writeTVar beTxChanges newTxIdState
        STM.writeTVar beTxOutChanges newTxOutBalance
        STM.writeTVar beCurrentBlock (succ blockNumber)
        Right <$> blockAndSlot env
      (Left e, _) -> pure $ Left $ InsertUtxoStateFailure e
      (_, Left e) -> pure $ Left $ InsertUtxoStateFailure e

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

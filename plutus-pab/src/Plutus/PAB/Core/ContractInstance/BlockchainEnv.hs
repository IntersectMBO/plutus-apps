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
  ) where

import Cardano.Api (BlockInMode (..), ChainPoint (..))
import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper))
import Cardano.Node.Params qualified as Params
import Cardano.Protocol.Socket.Client (ChainSyncEvent (..))
import Cardano.Protocol.Socket.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Data.Map qualified as Map
import Data.Monoid (Last (..), Sum (..))
import Ledger (Block, Slot (..), TxId (..))
import Ledger.Params (Params (pNetworkId))
import Plutus.PAB.Core.ContractInstance.STM (BlockchainEnv (..), InstanceClientEnv (..), InstancesState,
                                             OpenTxOutProducedRequest (..), OpenTxOutSpentRequest (..),
                                             emptyBlockchainEnv)
import Plutus.PAB.Core.ContractInstance.STM qualified as S
import Plutus.Trace.Emulator.ContractInstance (IndexedBlock (..), indexBlock)

import Plutus.PAB.Types (Config (Config), DevelopmentOptions (DevelopmentOptions, pabResumeFrom, pabRollbackHistory),
                         developmentOptions, nodeServerConfig)

import Cardano.Node.Types (NodeMode (..),
                           PABServerConfig (PABServerConfig, pscNetworkId, pscNodeMode, pscSlotConfig, pscSocketPath))
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Tracer (nullTracer)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, maybeToList)
import Ledger.TimeSlot qualified as TimeSlot
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
import System.Random

-- | Connect to the node and write node updates to the blockchain
--   env.
startNodeClient ::
     Config -- ^ PAB's config
  -> InstancesState -- ^ In-memory state of running contract instances
  -> IO BlockchainEnv
startNodeClient config instancesState = do
    let Config { nodeServerConfig =
                   PABServerConfig { pscSocketPath = socket
                                   , pscSlotConfig = slotConfig
                                   , pscNodeMode
                                   , pscNetworkId = NetworkIdWrapper networkId
                                   }
               , developmentOptions =
                   DevelopmentOptions { pabRollbackHistory
                                      , pabResumeFrom = resumePoint
                                      }
               } = config
    params <- Params.fromPABServerConfig $ nodeServerConfig config
    env <- STM.atomically $ emptyBlockchainEnv pabRollbackHistory params
    case pscNodeMode of
      MockNode -> do
        void $ MockClient.runChainSync socket slotConfig
            (\block slot -> handleSyncAction $ processMockBlock instancesState env block slot
            )
      AlonzoNode -> do
        let resumePoints = maybeToList $ toCardanoPoint resumePoint
        void $ Client.runChainSync socket nullTracer slotConfig networkId resumePoints
            (\block -> do
                -- We store the actual current slot in `BlockchainEnv`. Thus,
                -- at every new block from the local node, we request for the
                -- current slot number and store it. The actual current slot is
                -- useful/necessary for blocking contract actions like `awaitSlot`.
                slot <- TimeSlot.currentSlot slotConfig
                STM.atomically $ STM.writeTVar (beCurrentSlot env) slot

                handleSyncAction $ processChainSyncEvent instancesState env block
            )
    pure env

-- | Deal with sync action failures from running this STM action. For now, we
-- deal with them by simply calling `error`; i.e. the application exits.
handleSyncAction :: STM (Either SyncActionFailure (Slot, BlockNumber)) -> IO ()
handleSyncAction action = do
  result <- STM.atomically action
  case result of
    Left err -> putStrLn $ "handleSyncAction failed with: " <> show err
    Right (Slot s, BlockNumber n) -> do
      stdGen <- newStdGen
      when (fst (randomR (0 :: Int, 10_000) stdGen) == 0) $
        putStrLn $ "Current synced block: " <> show n <> ". Current synced slot: " <> show s
  either (error . show) (const $ pure ()) result

updateInstances :: IndexedBlock -> InstanceClientEnv -> STM ()
updateInstances
    IndexedBlock{ibUtxoSpent, ibUtxoProduced}
    InstanceClientEnv{ceUtxoSpentRequests, ceUtxoProducedRequests} = do

  forM_ (Map.intersectionWith (,) ibUtxoSpent ceUtxoSpentRequests) $ \(onChainTx, requests) ->
    traverse (\OpenTxOutSpentRequest{osrSpendingTx} -> STM.tryPutTMVar osrSpendingTx onChainTx) requests
  forM_ (Map.intersectionWith (,) ibUtxoProduced ceUtxoProducedRequests) $ \(txns, requests) ->
    traverse (\OpenTxOutProducedRequest{otxProducingTxns} -> STM.tryPutTMVar otxProducingTxns txns) requests

blockAndSlot :: BlockchainEnv -> STM (Slot, BlockNumber)
blockAndSlot BlockchainEnv{beLastSyncedBlockNo, beLastSyncedBlockSlot} =
  (,) <$> STM.readTVar beLastSyncedBlockSlot <*> STM.readTVar beLastSyncedBlockNo

-- | Process a chain sync event that we receive from the alonzo node client
processChainSyncEvent
  :: InstancesState
  -> BlockchainEnv
  -> ChainSyncEvent
  -> STM (Either SyncActionFailure (Slot, BlockNumber))
processChainSyncEvent instancesState blockchainEnv event = do
  case event of
    Resume _ -> Right <$> blockAndSlot blockchainEnv
    RollForward (BlockInMode (C.Block header transactions) era) _ ->
      processBlock instancesState header blockchainEnv transactions era
    RollBackward chainPoint _ -> runRollback blockchainEnv chainPoint

data SyncActionFailure
  = RollbackFailure RollbackFailed
  | InsertUtxoStateFailure InsertUtxoFailed
  deriving (Show)

-- | Roll back the chain to the given ChainPoint and slot.
runRollback :: BlockchainEnv -> ChainPoint -> STM (Either SyncActionFailure (Slot, BlockNumber))
runRollback env@BlockchainEnv{beLastSyncedBlockSlot, beTxChanges, beTxOutChanges} chainPoint = do
  currentSlot <- STM.readTVar beLastSyncedBlockSlot
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
  STM.writeTVar (beLastSyncedBlockSlot env) (fromIntegral slot)
  if null transactions
     then Right <$> blockAndSlot env
     else do
        let tip = fromCardanoBlockHeader header
            -- We ignore cardano transactions that we couldn't convert to
            -- our 'ChainIndexTx'.
            ciTxs = catMaybes (either (const Nothing) Just . fromCardanoTx era <$> transactions)

        instEnv <- S.instancesClientEnv instancesState
        updateInstances (indexBlock ciTxs) instEnv

        updateTransactionState tip env (txEvent <$> ciTxs)

-- | For the given transactions, perform the updates in the 'TxIdState', and
-- also record that a new block has been processed.
updateTransactionState
  :: Foldable t
  => Tip
  -> BlockchainEnv
  -> t (TxId, TxOutBalance, TxValidity)
  -> STM (Either SyncActionFailure (Slot, BlockNumber))
updateTransactionState
    tip
    env@BlockchainEnv{ beRollbackHistory
                     , beTxChanges
                     , beTxOutChanges
                     , beLastSyncedBlockNo
                     }
    xs = do

    txIdStateIndex <- STM.readTVar beTxChanges
    let txIdState = _usTxUtxoData $ utxoState txIdStateIndex
    txUtxoBalanceIndex <- STM.readTVar beTxOutChanges
    let txUtxoBalance = _usTxUtxoData $ utxoState txUtxoBalanceIndex
    blockNumber <- STM.readTVar beLastSyncedBlockNo
    let txIdState' = foldl' (insertNewTx blockNumber) txIdState xs
        txIdStateInsert  = insert (UtxoState txIdState' tip) txIdStateIndex
        txUtxoBalance' = txUtxoBalance <> foldMap (\(_, b, _) -> b) xs
        txUtxoBalanceInsert = insert (UtxoState txUtxoBalance' tip) txUtxoBalanceIndex

    case (txIdStateInsert, txUtxoBalanceInsert) of
      (Right InsertUtxoSuccess{newIndex=newTxIdState}
        , Right InsertUtxoSuccess{newIndex=newTxOutBalance}) -> do -- TODO: Get tx out status another way

        STM.writeTVar beTxChanges    $ trimIx beRollbackHistory newTxIdState
        STM.writeTVar beTxOutChanges $ trimIx beRollbackHistory newTxOutBalance
        STM.writeTVar beLastSyncedBlockNo (succ blockNumber)
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
processMockBlock
    :: InstancesState
    -> BlockchainEnv
    -> Block
    -> Slot
    -> STM (Either SyncActionFailure (Slot, BlockNumber))
processMockBlock
  instancesState
  env@BlockchainEnv{beCurrentSlot, beLastSyncedBlockSlot, beLastSyncedBlockNo, beParams}
  transactions
  slot = do

  -- In the mock node, contrary to the actual node, the last synced block slot
  -- and the actual slot is the same.
  lastSyncedBlockSlot <- STM.readTVar beLastSyncedBlockSlot
  when (slot > lastSyncedBlockSlot) $ do
    STM.writeTVar beLastSyncedBlockSlot slot

  lastCurrentSlot <- STM.readTVar beCurrentSlot
  when (slot > lastCurrentSlot ) $ do
    STM.writeTVar beCurrentSlot slot

  let networkId = pNetworkId beParams
  if null transactions
     then do
       result <- (,) <$> STM.readTVar beLastSyncedBlockSlot <*> STM.readTVar beLastSyncedBlockNo
       pure $ Right result
     else do
      blockNumber <- STM.readTVar beLastSyncedBlockNo

      instEnv <- S.instancesClientEnv instancesState
      updateInstances (indexBlock $ fmap (fromOnChainTx networkId) transactions) instEnv

      let tip = Tip { tipSlot = slot
                    , tipBlockId = blockId transactions
                    , tipBlockNo = blockNumber
                    }

      updateTransactionState tip env (txEvent <$> fmap (fromOnChainTx networkId) transactions)

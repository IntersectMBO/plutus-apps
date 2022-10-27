{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
-- |
module Plutus.PAB.Core.ContractInstance.BlockchainEnv(
  startNodeClient
  , processMockBlock
  ) where

import Cardano.Api (BlockInMode (..), ChainPoint (..), chainPointToSlotNo)
import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper))
import Cardano.Node.Params qualified as Params
import Cardano.Node.Types (NodeMode (..),
                           PABServerConfig (PABServerConfig, pscNetworkId, pscNodeMode, pscSlotConfig, pscSocketPath))
import Cardano.Protocol.Socket.Client (ChainSyncEvent (..))
import Cardano.Protocol.Socket.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Monad.Freer.Extras.Beam.Postgres qualified as Postgres (DbConfig (dbConfigMarconiFile))
import Control.Monad.Freer.Extras.Beam.Sqlite qualified as Sqlite (DbConfig (dbConfigFile))
import Control.Tracer (nullTracer)
import Data.Foldable (foldl')
import Data.IORef (newIORef)
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Last (..), Sum (..))
import Data.Text (Text, unpack)
import Ledger (Block, Slot (..), TxId (..))
import Ledger.TimeSlot qualified as TimeSlot
import Plutus.ChainIndex (BlockNumber (..), ChainIndexTx (..), Depth (..), InsertUtxoFailed (..),
                          InsertUtxoSuccess (..), Point (..), ReduceBlockCountResult (..), RollbackFailed (..),
                          RollbackResult (..), Tip (..), TxConfirmedState (..), TxIdState (..), TxOutBalance,
                          TxValidity (..), UtxoIndex, UtxoState (..), blockId, citxTxId, fromOnChainTx, insert,
                          reduceBlockCount, tipAsPoint, utxoState, validityFromChainIndex)
import Plutus.ChainIndex.Compatibility (fromCardanoBlockHeader, fromCardanoPoint, toCardanoPoint)
import Plutus.ChainIndex.TxIdState qualified as TxIdState
import Plutus.ChainIndex.TxOutBalance qualified as TxOutBalance
import Plutus.ChainIndex.UtxoState (viewTip)
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.PAB.Core.ContractInstance.STM (BlockchainEnv (..), InstanceClientEnv (..), InstancesState,
                                             OpenTxOutProducedRequest (..), OpenTxOutSpentRequest (..),
                                             emptyBlockchainEnv)
import Plutus.PAB.Core.ContractInstance.STM qualified as S
import Plutus.PAB.Core.Indexer.TxConfirmationStatus (TxInfo (..))
import Plutus.PAB.Core.Indexer.TxConfirmationStatus qualified as Ix
import Plutus.PAB.Types (Config (Config, dbConfig), DbConfig (..),
                         DevelopmentOptions (DevelopmentOptions, pabResumeFrom, pabRollbackHistory),
                         WebserverConfig (WebserverConfig, enableMarconi), developmentOptions, nodeServerConfig,
                         pabWebserverConfig)
import Plutus.Trace.Emulator.ContractInstance (IndexedBlock (..), indexBlock)
import RewindableIndex.Index.VSqlite qualified as Ix
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
               , pabWebserverConfig =
                   WebserverConfig { enableMarconi = useDiskIndex }
               , dbConfig = dbConf
               } = config
    params <- Params.fromPABServerConfig $ nodeServerConfig config
    env <- do
      env' <- STM.atomically $ emptyBlockchainEnv pabRollbackHistory params
      if useDiskIndex && nodeStartsInAlonzoMode pscNodeMode
      then do
        utxoIx <- Ix.open (unpack $ getDBFilePath dbConf) (Ix.Depth 10) >>= newIORef
        pure $ env' { beTxChanges = Right utxoIx }
      else do
        pure env'
    case pscNodeMode of
      MockNode -> do
        void $ MockClient.runChainSync socket slotConfig
            (\block slot -> handleSyncAction =<< processMockBlock instancesState env block slot)
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
                processChainSyncEvent instancesState env block >>= handleSyncAction'
            )
      NoChainSyncEvents -> pure ()
    pure env
    where
      getDBFilePath :: DbConfig -> Text
      getDBFilePath (SqliteDB c)   = Sqlite.dbConfigFile c
      getDBFilePath (PostgresDB c) = Postgres.dbConfigMarconiFile c

      nodeStartsInAlonzoMode :: NodeMode -> Bool
      nodeStartsInAlonzoMode AlonzoNode = True
      nodeStartsInAlonzoMode _          = False

-- | Deal with sync action failures from running this STM action. For now, we
-- deal with them by simply calling `error`; i.e. the application exits.
handleSyncAction :: STM (Either SyncActionFailure (Slot, BlockNumber)) -> IO ()
handleSyncAction action = do
  STM.atomically action >>= handleSyncAction'

handleSyncAction' :: Either SyncActionFailure (Slot, BlockNumber) -> IO ()
handleSyncAction' action = do
  case action of
    Left err -> putStrLn $ "handleSyncAction failed with: " <> show err
    Right (Slot s, BlockNumber n) -> do
      stdGen <- newStdGen
      when (fst (randomR (0 :: Int, 10_000) stdGen) == 0) $
        putStrLn $ "Current synced block: " <> show n <> ". Current synced slot: " <> show s
  either (error . show) (const $ pure ()) action

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
  -> IO (Either SyncActionFailure (Slot, BlockNumber))
processChainSyncEvent instancesState env@BlockchainEnv{beTxChanges} event = do
  case event of
    Resume _ -> STM.atomically $ Right <$> blockAndSlot env
    RollForward (BlockInMode (C.Block header transactions) era) _ ->
      processBlock instancesState header env transactions era
    RollBackward chainPoint _ -> do
      S.updateTxChangesR beTxChanges $
        \txChanges -> do
           events <- concat <$> Ix.getEvents (txChanges ^. Ix.storage)
           pure . fromMaybe txChanges $ do
             slot   <- chainPointToSlotNo chainPoint
             offset <- findIndex (\(TxInfo _ _ sn) -> sn < slot) events
             Ix.rewind offset txChanges
      STM.atomically $ runRollback env chainPoint


data SyncActionFailure
  = RollbackFailure RollbackFailed
  | InsertUtxoStateFailure InsertUtxoFailed
  deriving (Show)

-- | Roll back the chain to the given ChainPoint and slot.
runRollback :: BlockchainEnv -> ChainPoint -> STM (Either SyncActionFailure (Slot, BlockNumber))
runRollback env@BlockchainEnv{beLastSyncedBlockSlot, beTxChanges, beTxOutChanges} chainPoint = do
  currentSlot <- STM.readTVar beLastSyncedBlockSlot
  txOutBalanceStateIndex <- STM.readTVar beTxOutChanges

  let point = fromCardanoPoint chainPoint
      rs'   = TxOutBalance.rollback point txOutBalanceStateIndex
      -- Check to see if the rollback is just through a sequence of empty blocks ending at the tip.
      emptyRollBack =
           point > tipAsPoint (viewTip txOutBalanceStateIndex)
        && pointSlot point <= currentSlot

  if emptyRollBack
    then Right <$> blockAndSlot env
    else case rs' of
           Right RollbackResult{rolledBackIndex=rolledBackTxOutBalanceStateIndex} -> do
             STM.writeTVar beTxOutChanges rolledBackTxOutBalanceStateIndex
             case beTxChanges of
               Left txChanges -> do
                 txIdStateIndex <- STM.readTVar txChanges
                 let rs = TxIdState.rollback point txIdStateIndex
                 case rs of
                   Left e -> pure $ Left (RollbackFailure e)
                   Right RollbackResult{rolledBackIndex=rolledBackTxIdStateIndex} -> do
                     STM.writeTVar txChanges rolledBackTxIdStateIndex
                     Right <$> blockAndSlot env
               Right _tcsIndex -> Right <$> blockAndSlot env
           Left e' -> pure $ Left (RollbackFailure e')

-- | Get transaction ID and validity from a transaction.
txEvent :: ChainIndexTx -> (TxId, TxOutBalance, TxValidity)
txEvent tx = (view citxTxId tx, TxOutBalance.fromTx tx, validityFromChainIndex tx)

-- | Update the blockchain env. with changes from a new block of cardano
--   transactions in any era
processBlock :: forall era. C.IsCardanoEra era
             => InstancesState
             -> C.BlockHeader
             -> BlockchainEnv
             -> [C.Tx era]
             -> C.EraInMode era C.CardanoMode
             -> IO (Either SyncActionFailure (Slot, BlockNumber))
processBlock instancesState header env@BlockchainEnv{beTxChanges} transactions era = do
  let C.BlockHeader (C.SlotNo slot) _ _ = header
      tip = fromCardanoBlockHeader header
      ciTxs = fromCardanoTx era <$> transactions

  stmResult <-
    if null transactions
    then do
      STM.atomically $ do
        STM.writeTVar (beLastSyncedBlockSlot env) (fromIntegral slot)
        Right <$> blockAndSlot env
    else do
      instEnv <- S.instancesClientEnv instancesState
      STM.atomically $ do
        e <- instEnv
        STM.writeTVar (beLastSyncedBlockSlot env) (fromIntegral slot)
        updateInstances (indexBlock ciTxs) e
        updateEmulatorTransactionState tip env (txEvent <$> ciTxs)

  S.updateTxChangesR beTxChanges $ Ix.insert (mkEvent tip <$> ciTxs)

  pure stmResult


mkEvent :: Tip -> ChainIndexTx -> TxInfo
mkEvent TipAtGenesis  tx =
  TxInfo { txId        = _citxTxId tx
         , slotNumber  = fromIntegral (0 :: Int)
         , blockNumber = fromIntegral (0 :: Int)
         }
mkEvent (Tip sn _ bn) tx =
  TxInfo { txId        = _citxTxId tx
         , slotNumber  = fromIntegral sn
         , blockNumber = bn
         }

-- | For the given transactions, perform the updates in the 'TxIdState', and
-- also record that a new block has been processed.
updateEmulatorTransactionState
  :: Foldable t
  => Tip
  -> BlockchainEnv
  -> t (TxId, TxOutBalance, TxValidity)
  -> STM (Either SyncActionFailure (Slot, BlockNumber))
updateEmulatorTransactionState
    tip
    env@BlockchainEnv{ beRollbackHistory
                     , beTxChanges
                     , beTxOutChanges
                     , beLastSyncedBlockNo
                     }
    xs = do

    txUtxoBalanceIndex <- STM.readTVar beTxOutChanges
    let txUtxoBalance = _usTxUtxoData $ utxoState txUtxoBalanceIndex
    blockNumber <- STM.readTVar beLastSyncedBlockNo
    let txUtxoBalance' = txUtxoBalance <> foldMap (\(_, b, _) -> b) xs
        txUtxoBalanceInsert = insert (UtxoState txUtxoBalance' tip) txUtxoBalanceIndex

    case txUtxoBalanceInsert of
      Right InsertUtxoSuccess{newIndex=newTxOutBalance} -> do
        STM.writeTVar beTxOutChanges $ trimIx beRollbackHistory newTxOutBalance
        STM.writeTVar beLastSyncedBlockNo (succ blockNumber)
        -- We have to handle the case where we don't have a `UtxoState` indexer
        -- available in the environment. If this happens, it means that we have
        -- a disk based indexer which is updated outside of this function, as it
        -- requires `IO` to operate.
        case beTxChanges of
          Left txChanges -> do
            txIdStateIndex     <- STM.readTVar txChanges
            let txIdState       = _usTxUtxoData $ utxoState txIdStateIndex
                txIdState'      = foldl' (insertNewTx blockNumber) txIdState xs
                txIdStateInsert = insert (UtxoState txIdState' tip) txIdStateIndex
            case txIdStateInsert of
              Right InsertUtxoSuccess{newIndex=newTxIdState} -> do
                STM.writeTVar txChanges $ trimIx beRollbackHistory newTxIdState
                Right <$> blockAndSlot env
              -- We have an in-memory indexer, but for some reason it failed to
              -- insert the Utxo
              Left e -> pure $ Left $ InsertUtxoStateFailure e
          Right _ ->
            -- This means that there is no in-memory indexer available, so we are
            -- using the on-disk one, so we just return all-is-fine.
            Right <$> blockAndSlot env
      Left e -> pure $ Left $ InsertUtxoStateFailure e

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
    -> IO (STM (Either SyncActionFailure (Slot, BlockNumber)))
processMockBlock
  instancesState
  env@BlockchainEnv{beCurrentSlot, beLastSyncedBlockSlot, beLastSyncedBlockNo}
  transactions
  slot = do

  if null transactions
    then pure $ do
      updateSlot
      result <- (,) <$> STM.readTVar beLastSyncedBlockSlot <*> STM.readTVar beLastSyncedBlockNo
      pure $ Right result
    else do
      instEnv <- S.instancesClientEnv instancesState
      pure $ do
        updateSlot
        blockNumber <- STM.readTVar beLastSyncedBlockNo
        e <- instEnv
        updateInstances (indexBlock $ fmap fromOnChainTx transactions) e

        let tip = Tip { tipSlot = slot
                      , tipBlockId = blockId transactions
                      , tipBlockNo = blockNumber
                      }

        updateEmulatorTransactionState tip env (txEvent <$> fmap fromOnChainTx transactions)


  where
    updateSlot = do
      -- In the mock node, contrary to the actual node, the last synced block slot
      -- and the actual slot is the same.
      lastSyncedBlockSlot <- STM.readTVar beLastSyncedBlockSlot
      when (slot > lastSyncedBlockSlot) $ do
        STM.writeTVar beLastSyncedBlockSlot slot

      lastCurrentSlot <- STM.readTVar beCurrentSlot
      when (slot > lastCurrentSlot ) $ do
        STM.writeTVar beCurrentSlot slot

module Plutus.Streaming where

import Cardano.Api
import Cardano.Api.ChainSync.Client
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Except (runExceptT)
import Streaming
import Streaming.Prelude qualified as S

-- import Plutus.Contract.CardanoAPI (fromCardanoBlock, fromCardanoTx)

--
-- FIXME this needs IsString (Hash BlockHeader) which seems to be missing
--
-- recentPoint :: ChainPoint
-- recentPoint = ChainPoint (SlotNo 53427524) "5e2bde4e504a9888a4f218dafc79a7619083f97d48684fcdba9dc78190df8f99"

-- Simple ChainSync client (non pipelined)

data ChainSyncEvent a
  = RollForward a ChainTip
  | RollBackward ChainPoint ChainTip
  deriving (Show, Functor)

type SimpleChainSyncEvent = ChainSyncEvent (BlockInMode CardanoMode)

type ChainSyncEventWithLedgerState = ChainSyncEvent (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent]))

data EventStreamResult
  = NoIntersectionFound
  deriving (Show)

withSimpleChainSyncEventStream ::
  FilePath ->
  NetworkId ->
  ChainPoint ->
  (Stream (Of SimpleChainSyncEvent) IO EventStreamResult -> IO b) ->
  IO b
withSimpleChainSyncEventStream filePath networkId point =
  withClientStream (chainSyncStreamingClient filePath networkId point)

withChainSyncEventStreamWithLedgerState ::
  FilePath ->
  FilePath ->
  NetworkId ->
  ChainPoint ->
  (Stream (Of ChainSyncEventWithLedgerState) IO EventStreamResult -> IO b) ->
  IO b
withChainSyncEventStreamWithLedgerState networkConfigPath filePath networkId point =
  withClientStream (chainSyncStreamingClientWithLedgerState networkConfigPath filePath networkId point)

-- This adapts a streaming client to a stream
withClientStream ::
  (MVar (Maybe (Chan e)) -> IO r) ->
  (Stream (Of e) IO EventStreamResult -> IO b) ->
  IO b
withClientStream client consumer = do
  -- We use a MVar as a synchronisation point to learn if the client as
  -- successfully found an intersection. He rely on the fact that
  -- clientSyncChain will write into m, telling us whether it has found an
  -- intersection. If this doesn't happen we will be stuck waiting forever.
  -- FIXME I haven't even thought about exception safety here.
  m <- newEmptyMVar
  withAsync (client m) $ \_ -> do
    mc <- takeMVar m
    case mc of
      Nothing ->
        consumer $ return NoIntersectionFound
      Just c -> do
        -- FIXME Client gets killed when the consumer finishes, we
        -- should allow for a better clean up here
        consumer $ S.repeatM $ readChan c

--
-- this can be replaced by the almost identical function in
-- Cardano.Protocol.Socket.Client.
--
-- TODO move to pipelined version
chainSyncStreamingClient ::
  FilePath ->
  NetworkId ->
  ChainPoint ->
  MVar (Maybe (Chan SimpleChainSyncEvent)) ->
  IO ()
chainSyncStreamingClient socketPath networkId point mChan = do
  let client = chainSyncStreamingClient' point mChan

      localNodeClientProtocols =
        LocalNodeClientProtocols
          { localChainSyncClient = LocalChainSyncClient client,
            localTxSubmissionClient = Nothing,
            localStateQueryClient = Nothing
          }

      connectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams epochSlots,
            localNodeNetworkId = networkId,
            localNodeSocketPath = socketPath
          }

      -- FIXME this comes from the config file but Cardano.Api does not expose readNetworkConfig!
      epochSlots = EpochSlots 40

  connectToLocalNode
    connectInfo
    localNodeClientProtocols

chainSyncStreamingClientWithLedgerState ::
  FilePath ->
  FilePath ->
  NetworkId ->
  ChainPoint ->
  MVar (Maybe (Chan ChainSyncEventWithLedgerState)) ->
  IO ()
chainSyncStreamingClientWithLedgerState networkConfigFile socketPath networkId point mChan = do
  ils <- runExceptT (initialLedgerState networkConfigFile)

  case ils of
    (Left _) ->
      -- FIXME here we swallow the error but we can do better
      putMVar mChan Nothing
    (Right (env, ledgerState)) -> do
      let client = chainSyncClientWithLedgerState env ledgerState QuickValidation (chainSyncStreamingClient' point mChan)

          cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

          connectInfo =
            LocalNodeConnectInfo
              { localConsensusModeParams = cardanoModeParams,
                localNodeNetworkId = networkId,
                localNodeSocketPath = socketPath
              }

          localNodeClientProtocols =
            LocalNodeClientProtocols
              { localChainSyncClient = LocalChainSyncClient client,
                localTxSubmissionClient = Nothing,
                localStateQueryClient = Nothing
              }

      connectToLocalNode
        connectInfo
        localNodeClientProtocols

chainSyncStreamingClient' ::
  ChainPoint ->
  MVar (Maybe (Chan (ChainSyncEvent e))) ->
  ChainSyncClient e ChainPoint ChainTip IO ()
chainSyncStreamingClient' point mChan =
  ChainSyncClient $ do
    putStrLn "Connecting ..."
    pure $ SendMsgFindIntersect [point] onIntersect
  where
    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \point' _ ->
            ChainSyncClient $ do
              putStrLn $ "Intersection found at " ++ show point'
              c <- newChan
              putMVar mChan (Just c)
              sendRequestNext c,
          recvMsgIntersectNotFound = \_ ->
            ChainSyncClient $ do
              putStrLn "Intersection not found"
              putMVar mChan Nothing
              pure $ SendMsgDone ()
        }

    sendRequestNext c =
      pure $ SendMsgRequestNext onNext (pure onNext)
      where
        onNext =
          ClientStNext
            { recvMsgRollForward = \bim ct ->
                ChainSyncClient $ do
                  writeChan c (RollForward bim ct)
                  sendRequestNext c,
              recvMsgRollBackward = \cp ct ->
                ChainSyncClient $ do
                  writeChan c (RollBackward cp ct)
                  sendRequestNext c
            }

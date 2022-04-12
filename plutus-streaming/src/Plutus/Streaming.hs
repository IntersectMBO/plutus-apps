{-# LANGUAGE FlexibleInstances #-}

module Plutus.Streaming
  ( SimpleChainSyncEvent,
    withSimpleChainSyncEventStream,
    ChainSyncEvent (..),
    EventStreamResult (..),
  )
where

import Cardano.Api (BlockInMode, CardanoMode, ChainPoint, ChainSyncClient (ChainSyncClient), ChainTip,
                    ConsensusModeParams (CardanoModeParams), EpochSlots (EpochSlots),
                    LocalChainSyncClient (LocalChainSyncClient),
                    LocalNodeClientProtocols (LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient),
                    LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                    NetworkId, connectToLocalNode)
import Cardano.Api.ChainSync.Client (ClientStIdle (SendMsgDone, SendMsgFindIntersect, SendMsgRequestNext),
                                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward))
import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, catch)
import GHC.Generics (Generic)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

data ChainSyncEvent a
  = RollForward a ChainTip
  | RollBackward ChainPoint ChainTip
  deriving (Show, Functor, Generic)

type SimpleChainSyncEvent = ChainSyncEvent (BlockInMode CardanoMode)

data EventStreamResult
  = NoIntersectionFound
  deriving (Show)

withSimpleChainSyncEventStream ::
  FilePath ->
  NetworkId ->
  ChainPoint ->
  (Stream (Of SimpleChainSyncEvent) IO EventStreamResult -> IO b) ->
  IO b
withSimpleChainSyncEventStream socketPath networkId point consumer = do
  mChan <- newEmptyMVar
  let client = chainSyncStreamingClient point mChan

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

      clientThread =
        connectToLocalNode connectInfo localNodeClientProtocols
          `catch` \(_ :: SomeException) -> putMVar mChan Nothing

  withAsync clientThread $ \_ -> do
    mc <- takeMVar mChan
    case mc of
      Nothing ->
        consumer $ return NoIntersectionFound
      Just c -> do
        consumer $ S.repeatM $ readChan c

-- | This is the "core" client that connects to a local node and
-- runs the chain-sync mini-protocol. The only job of this client is to
-- keep sending requests for new blocks, and passing the results (a
-- `ChainSyncEvent`) to the consumer. In particular, this client is
-- fire-and-forget and does not require any control from the consumer.
chainSyncStreamingClient ::
  -- | The point on the chain to start from
  ChainPoint ->
  -- | This MVar is how we communicate back to the consumer. The idea here
  -- is that the client might fail to initialise but once it's initalised
  -- it will always be able to spit out `ChainSyncEvent`s.
  MVar (Maybe (Chan (ChainSyncEvent e))) ->
  -- | The entry point to the client to pass to `connectToLocalNode`
  ChainSyncClient e ChainPoint ChainTip IO ()
chainSyncStreamingClient point mChan =
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

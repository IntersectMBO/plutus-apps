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
  -- | The point on the chain to start streaming from
  ChainPoint ->
  (Stream (Of SimpleChainSyncEvent) IO EventStreamResult -> IO b) ->
  IO b
withSimpleChainSyncEventStream socketPath networkId point consumer = do
  -- The chain-sync client runs in a different thread. It needs to send us
  -- two kind of information 1) if it has managed to establish a connection
  -- and found an intersection 2) the blocks it gets from the protocol.
  --
  -- I encapsulated both this information in a single MVar (Maybe Chan _)
  --
  -- The MVar needs to be written to by the client.
  --
  -- If the MVar has Nothing written to it, the client has run into issues
  -- preventing it from finding an intersection.
  --
  -- If the MVar has (Just c) written to it, the client has succesfully
  -- found an intersection and blocks are going to be available from the
  -- channel c.
  --
  -- TODO the client needs to be able to reinitialise and keep going if the
  -- connection fails.
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
          -- FIXME this is still not good enough, if an exception arises
          -- after the client has started streaming, the consumer code
          -- below will ignore the value of the MVar and will be stuck
          -- waiting on the chan.
          `catch` \(_ :: SomeException) -> putMVar mChan Nothing

  withAsync clientThread $ \_ -> do
    mc <- takeMVar mChan
    case mc of
      Nothing ->
        consumer $ return NoIntersectionFound
      Just c -> do
        consumer $ S.repeatM $ readChan c

-- | `chainSyncStreamingClient` is the client that connects to a local node
-- and runs the chain-sync mini-protocol.
--
-- I am using the term "streaming client" because the only things it does
-- is to keep sending requests for new blocks.
--
-- In particular, this client is fire-and-forget and does not require any
-- control.
--
-- Blocks obtained from the chain-sync mini-protocol are passed to a
-- consumer through a channel. To understand the MVar-Maybe-Chan dance see
-- note in `withSimpleChainSyncEventStream`
chainSyncStreamingClient ::
  ChainPoint ->
  MVar (Maybe (Chan (ChainSyncEvent e))) ->
  ChainSyncClient e ChainPoint ChainTip IO ()
chainSyncStreamingClient point mChan =
  ChainSyncClient $ pure $ SendMsgFindIntersect [point] onIntersect
  where
    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \_ _ ->
            ChainSyncClient $ do
              c <- newChan
              putMVar mChan (Just c)
              sendRequestNext c,
          recvMsgIntersectNotFound = \_ ->
            ChainSyncClient $ do
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

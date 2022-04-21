module Plutus.Streaming
  ( SimpleChainSyncEvent,
    withSimpleChainSyncEventStream,
    ChainSyncEvent (..),
    ChainSyncEventException (..),
  )
where

import Cardano.Api (BlockInMode, CardanoMode, ChainPoint, ChainSyncClient (ChainSyncClient), ChainTip,
                    ConsensusModeParams (CardanoModeParams), EpochSlots (EpochSlots),
                    LocalChainSyncClient (LocalChainSyncClient),
                    LocalNodeClientProtocols (LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient),
                    LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                    NetworkId, ToJSON, connectToLocalNode)
import Cardano.Api.ChainSync.Client (ClientStIdle (SendMsgDone, SendMsgFindIntersect, SendMsgRequestNext),
                                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward))
import Cardano.Api.Extras ()
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (Exception, throw)
import GHC.Generics (Generic)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

data ChainSyncEvent a
  = RollForward a ChainTip
  | RollBackward ChainPoint ChainTip
  deriving (Show, Functor, Generic)

instance ToJSON a => ToJSON (ChainSyncEvent a)

type SimpleChainSyncEvent = ChainSyncEvent (BlockInMode CardanoMode)

data ChainSyncEventException
  = NoIntersectionFound
  deriving (Show)

instance Exception ChainSyncEventException

withSimpleChainSyncEventStream ::
  FilePath ->
  NetworkId ->
  -- | The point on the chain to start streaming from
  ChainPoint ->
  (Stream (Of SimpleChainSyncEvent) IO r -> IO b) ->
  IO b
withSimpleChainSyncEventStream socketPath networkId point consumer = do
  -- The chain-sync client runs in a different thread and it will send us
  -- block through this channel.
  chan <- newBroadcastTChanIO
  readerChannel <- atomically $ dupTChan chan

  let client = chainSyncStreamingClient point chan

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

      clientThread = do
        connectToLocalNode connectInfo localNodeClientProtocols
        -- the only reason the clien can terminate successfully is if it
        -- doesn't find an intersection, we report that case to the
        -- consumer as an exception
        throw NoIntersectionFound

  -- All exceptions in the client thread are passed to the consumer thread
  -- TODO the client should be able to reinitialise and keep going if the
  -- connection fails.
  -- FIXME we still have a problem here, if the client dies while we are
  -- waiting on the channel we get a BlockedIndefinitelyOnMVar right away
  -- before the exception that killed the client
  withAsync clientThread $ \_ -> do
    consumer $ S.repeatM $ atomically (readTChan readerChannel)

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
  TChan (ChainSyncEvent e) ->
  ChainSyncClient e ChainPoint ChainTip IO ()
chainSyncStreamingClient point chan =
  ChainSyncClient $ pure $ SendMsgFindIntersect [point] onIntersect
  where
    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \_ _ ->
            ChainSyncClient sendRequestNext,
          recvMsgIntersectNotFound = \_ ->
            ChainSyncClient $ do
              pure $ SendMsgDone ()
        }

    sendRequestNext =
      pure $ SendMsgRequestNext onNext (pure onNext)
      where
        onNext =
          ClientStNext
            { recvMsgRollForward = \bim ct ->
                ChainSyncClient $ do
                  atomically $ writeTChan chan (RollForward bim ct)
                  sendRequestNext,
              recvMsgRollBackward = \cp ct ->
                ChainSyncClient $ do
                  atomically $ writeTChan chan (RollBackward cp ct)
                  sendRequestNext
            }

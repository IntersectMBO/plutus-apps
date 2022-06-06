module Plutus.Streaming
  ( withChainSyncEventStream,
    ChainSyncEvent (..),
    ChainSyncEventException (..),
  )
where

import Cardano.Api (BlockInMode, CardanoMode, ChainPoint, ChainSyncClient (ChainSyncClient), ChainTip,
                    ConsensusModeParams (CardanoModeParams), EpochSlots (EpochSlots),
                    LocalChainSyncClient (LocalChainSyncClient),
                    LocalNodeClientProtocols (LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient),
                    LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                    NetworkId, connectToLocalNode, envSecurityParam, initialLedgerState, renderInitialLedgerStateError)
import Cardano.Api.ChainSync.Client (ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
                                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward))
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (Exception, throw)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

data ChainSyncEvent a
  = RollForward a ChainTip
  | RollBackward ChainPoint ChainTip
  deriving (Show, Functor, Generic)

data ChainSyncEventException
  = NoIntersectionFound
  deriving (Show)

instance Exception ChainSyncEventException

-- | `withChainSyncEventStream` uses the chain-sync mini-protocol to
-- connect to a locally running node and fetch blocks from the given
-- starting point.
withChainSyncEventStream ::
  -- | Path to the node config gile
  FilePath ->
  -- | Path to the node socket
  FilePath ->
  NetworkId ->
  -- | The point on the chain to start streaming from
  ChainPoint ->
  -- | Stream consumer
  (Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r -> IO b) ->
  IO b
withChainSyncEventStream configFile socketPath networkId point consumer = do
  (env, _) <- either (fail . Text.unpack . renderInitialLedgerStateError) pure =<<
    -- FIXME Ideally we want to use readNetworkConfig instead of initialLedgerState,
    -- but Cardano.Api does not expose it.
    runExceptT (initialLedgerState configFile)

  -- The chain-sync client runs in a different thread and it will send us
  -- block through this channel.

  -- By using newBroadcastTChan, messages can be garbage collected after
  -- clients have seen them, preventing pile up. The only way to read a
  -- broadcast channel is to duplicate it with dupTChan. See note at
  -- `newBroadcastTChan`.
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

      epochSlots = EpochSlots (10 * envSecurityParam env)

      clientThread = do
        connectToLocalNode connectInfo localNodeClientProtocols
        -- the only reason connectToLocalNode can terminate successfully is if it
        -- doesn't find an intersection, we report that case to the
        -- consumer as an exception
        throw NoIntersectionFound

  withAsync clientThread $ \a -> do
    -- Make sure all exceptions in the client thread are passed to the consumer thread
    link a
    -- Run the consumer
    consumer $ S.repeatM $ atomically (readTChan readerChannel)

-- | `chainSyncStreamingClient` is the client that connects to a local node
-- and runs the chain-sync mini-protocol. This client is fire-and-forget
-- and does not require any control.
--
-- Blocks obtained from the chain-sync mini-protocol are passed to a
-- consumer through a channel.
--
-- If the starting point is such that an intersection cannot be found, this
-- client will throw a NoIntersectionFound exception.
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
            ChainSyncClient $
              -- There is nothing we can do here
              throw NoIntersectionFound
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

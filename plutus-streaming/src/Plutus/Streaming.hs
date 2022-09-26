module Plutus.Streaming
  ( withChainSyncEventStream,
    ledgerStateEvents,
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    ApplyBlockException (..),
  )
where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), ChainSyncClient (ChainSyncClient), ChainTip,
                    ConsensusModeParams (CardanoModeParams), Env, EpochSlots (EpochSlots), LedgerEvent, LedgerState,
                    LedgerStateError, LocalChainSyncClient (LocalChainSyncClient),
                    LocalNodeClientProtocols (LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxMonitoringClient, localTxSubmissionClient),
                    LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                    NetworkId, SlotNo, ValidationMode, applyBlock, connectToLocalNode, envSecurityParam,
                    renderLedgerStateError)
import Cardano.Api.ChainSync.Client (ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
                                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward))
import Control.Concurrent.Async (ExceptionInLinkedThread (ExceptionInLinkedThread), link, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException (SomeException), catch, throw)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
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

newtype ApplyBlockException = ApplyBlockException LedgerStateError

instance Show ApplyBlockException where
  show (ApplyBlockException e) = Text.unpack (renderLedgerStateError e)

instance Exception ApplyBlockException

type History a = Seq (SlotNo, a)

-- | `withChainSyncEventStream` uses the chain-sync mini-protocol to
-- connect to a locally running node and fetch blocks from the given
-- starting point.
withChainSyncEventStream ::
  -- | Path to the node socket
  FilePath ->
  NetworkId ->
  -- | The point on the chain to start streaming from
  ChainPoint ->
  -- | The stream consumer
  (Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r -> IO b) ->
  IO b
withChainSyncEventStream socketPath networkId point consumer = do
  -- The chain-sync client runs in a different thread passing the blocks it
  -- receives to the stream consumer through a MVar. The chain-sync client
  -- thread and the stream consumer will each block on each other and stay
  -- in lockstep.
  --
  -- NOTE: choosing a MVar is a tradeoff towards simplicity. In this case a
  -- (bounded) queue could perform better. Indeed a properly-sized buffer
  -- can reduce the time the two threads are blocked waiting for each
  -- other. The problem here is "properly-sized". A bounded queue like
  -- Control.Concurrent.STM.TBQueue allows us to specify a max queue length
  -- but block size can vary a lot (TODO quantify this) depending on the
  -- era. We have an alternative implementation with customizable queue
  -- size (TBMQueue) but it needs to be extracted from the
  -- plutus-chain-index-core package. Using a simple MVar doesn't seem to
  -- slow down marconi's indexing, likely because the difference is
  -- negligeable compared to existing network and IO latencies.  Therefore,
  -- let's stick with a MVar now and revisit later.
  nextBlockVar <- newEmptyMVar

  let client = chainSyncStreamingClient point nextBlockVar

      localNodeClientProtocols =
        LocalNodeClientProtocols
          { localChainSyncClient = LocalChainSyncClient client,
            localStateQueryClient = Nothing,
            localTxMonitoringClient = Nothing,
            localTxSubmissionClient = Nothing
          }

      connectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams epochSlots,
            localNodeNetworkId = networkId,
            localNodeSocketPath = socketPath
          }

      -- This a parameter needed only for the Byron era. Since the Byron
      -- era is over and the parameter has never changed it is ok to
      -- hardcode this. See comment on `Cardano.Api.ConsensusModeParams` in
      -- cardano-node.
      epochSlots = EpochSlots 21600

  withAsync (connectToLocalNode connectInfo localNodeClientProtocols) $ \a -> do
    -- Make sure all exceptions in the client thread are passed to the consumer thread
    link a
    -- Run the consumer
    consumer $ S.repeatM $ takeMVar nextBlockVar
  -- Let's rethrow exceptions from the client thread unwrapped, so that the
  -- consumer does not have to know anything about async
  `catch` \(ExceptionInLinkedThread _ (SomeException e)) -> throw e

-- | `chainSyncStreamingClient` is the client that connects to a local node
-- and runs the chain-sync mini-protocol. This client is fire-and-forget
-- and does not require any control.
--
-- If the starting point is such that an intersection cannot be found, this
-- client will throw a NoIntersectionFound exception.
chainSyncStreamingClient ::
  ChainPoint ->
  MVar (ChainSyncEvent e) ->
  ChainSyncClient e ChainPoint ChainTip IO ()
chainSyncStreamingClient point nextChainEventVar =
  ChainSyncClient $ pure $ SendMsgFindIntersect [point] onIntersect
  where
    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \_ _ ->
            ChainSyncClient sendRequestNext,
          recvMsgIntersectNotFound =
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
                  putMVar nextChainEventVar (RollForward bim ct)
                  sendRequestNext,
              recvMsgRollBackward = \cp ct ->
                ChainSyncClient $ do
                  putMVar nextChainEventVar (RollBackward cp ct)
                  sendRequestNext
            }

-- | This function works under the assumption that the stream of blocks it
-- receives is valid. The function will trigger an exception if
-- 1. a block it receives does not apply on top of the ledger state
-- 2. a rollback goes past the security parameter
-- FIXME, for the moment I kept this function pure but it requires us to do
-- some up-front IO to obtain the initial ledger state from the network
-- config file.
ledgerStateEvents ::
  forall m r.
  Monad m =>
  Env ->
  LedgerState ->
  ValidationMode ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) m r ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode), (LedgerState, [LedgerEvent]))) m r
ledgerStateEvents env ls0 vm =
  S.scanned step initialHistory projection
  where
    step ::
      (History LedgerState, [LedgerEvent]) ->
      ChainSyncEvent (BlockInMode CardanoMode) ->
      (History LedgerState, [LedgerEvent])
    step (history, _) (RollForward (BlockInMode blk _) _) =
      unsafePushBlock history blk
    step _ (RollBackward ChainPointAtGenesis _) =
      initialHistory
    step (history, _) (RollBackward (ChainPoint sn _) _) =
      unsafeRollback history sn

    initialHistory :: (History LedgerState, [LedgerEvent])
    initialHistory = (Seq.singleton (0, ls0), [])

    -- This function is unsafe because it might result in an empty history,
    -- breaking the assumption of unsafePushBlock and projection
    unsafeRollback :: History LedgerState -> SlotNo -> (History LedgerState, [LedgerEvent])
    unsafeRollback history sn =
      let history' = Seq.dropWhileL ((> sn) . fst) history
       in (history', [])

    -- This function is unsafe because it will assume the given block will
    -- successfully apply on top of the ledger state.
    unsafePushBlock :: History LedgerState -> Block era -> (History LedgerState, [LedgerEvent])
    unsafePushBlock history@((_, ls) :<| _) blk@(Block (BlockHeader sn _ _) _) =
      case applyBlock env ls vm blk of
        Left e ->
          throw $ ApplyBlockException e
        Right (ls', lse) ->
          let history' = fst $ Seq.splitAt (fromIntegral $ envSecurityParam env + 1) ((sn, ls') :<| history)
           in (history', lse)
    unsafePushBlock Seq.Empty _ = error "Impossible! History should never be empty"

    projection :: (History LedgerState, [LedgerEvent]) -> (LedgerState, [LedgerEvent])
    projection ((_, ls) :<| _, lse) = (ls, lse)
    projection (Seq.Empty, _)       = error "Impossible! History should never be empty"

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Node.Socket.Emulator.Chain where

import Cardano.Node.Emulator.Internal.Node (Params)
import Cardano.Node.Emulator.Internal.Node.Chain qualified as EC
import Cardano.Node.Socket.Emulator.Types (BlockId (..), MockNodeServerChainState (..), Tip, TxPool, blockId, channel,
                                           currentSlot, index, tip, txPool)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens hiding (index)
import Control.Monad.Freer
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer.State (State, gets, modify)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Ledger (Block, CardanoTx)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Network.Block qualified as O

emptyChainState :: MonadIO m => m MockNodeServerChainState
emptyChainState = do
    chan <- liftIO . atomically $ newTChan
    pure $ MockNodeServerChainState [] mempty 0 chan O.TipGenesis

getChannel :: MonadIO m => MVar MockNodeServerChainState -> m (TChan Block)
getChannel mv = liftIO (readMVar mv) <&> view channel

-- Get the current tip.
getTip :: forall m. MonadIO m => MVar MockNodeServerChainState -> m Tip
getTip mvChainState = liftIO $ readMVar mvChainState >>= \case
    MockNodeServerChainState { _tip } -> pure _tip

handleControlChain ::
     ( Member (State MockNodeServerChainState) effs
     , Member (LogMsg EC.ChainEvent) effs
     , LastMember m effs
     , MonadIO m )
  => Params -> EC.ChainControlEffect ~> Eff effs
handleControlChain params = \case
    EC.ProcessBlock -> do
        pool  <- gets $ view txPool
        slot  <- gets $ view currentSlot
        idx   <- gets $ view index
        chan   <- gets $ view channel

        let EC.ValidatedBlock block events idx' = EC.validateBlock params slot idx pool

        modify $ txPool .~ []
        modify $ tip    .~ O.Tip (fromIntegral slot) (coerce $ blockId block) (fromIntegral slot)
        modify $ index  .~ idx'

        traverse_ EC.logEvent events

        liftIO $ atomically $ writeTChan chan block
        pure block
    EC.ModifySlot f -> modify @MockNodeServerChainState (over currentSlot f) >> gets (view currentSlot)

handleChain ::
     ( Member (State MockNodeServerChainState) effs )
  => Params
  -> EC.ChainEffect ~> Eff effs
handleChain params = \case
    EC.QueueTx tx     -> modify $ over txPool (addTxToPool tx)
    EC.GetCurrentSlot -> gets _currentSlot
    EC.GetParams      -> pure params

addTxToPool :: CardanoTx -> TxPool -> TxPool
addTxToPool = (:)

-- | Fetch the currently stored chain by iterating over the channel until
--   there is nothing left to be returned.
chainNewestFirst :: forall m b. MonadIO m => TChan b -> m [b]
chainNewestFirst ch = do
    localChannel <- liftIO $ atomically $ cloneTChan ch
    go localChannel []
    where
    go :: TChan b -> [b] -> m [b]
    go local acc =
        (liftIO $ atomically $ tryReadTChan local) >>= \case
            Nothing    -> pure acc
            Just block -> go ch (block : acc)

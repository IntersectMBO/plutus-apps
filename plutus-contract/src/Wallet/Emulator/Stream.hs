{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- | Running emulator actions that produce streams of events
module Wallet.Emulator.Stream(
    -- * Emulator streams
    EmulatorConfig(..)
    , EmulatorErr(..)
    , InitialChainState
    , initialChainState
    , initialDist
    , initialState
    , params
    , runTraceStream
    -- * Stream manipulation
    , takeUntilSlot
    , filterLogLevel
    -- * Consuming streams
    , foldStreamM
    , foldEmulatorStreamM
    ) where

import Cardano.Node.Emulator.Chain (ChainControlEffect, ChainEffect, _SlotAdd)
import Control.Foldl qualified as L
import Control.Lens (filtered, makeLenses, preview, view)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, run, subsume, type (~>))
import Control.Monad.Freer.Coroutine (Yield, yield)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras (raiseEnd, wrapError)
import Control.Monad.Freer.Extras.Log (LogLevel, LogMessage (LogMessage, _logLevel), LogMsg (LMessage),
                                       logMessageContent, mapMLog)
import Control.Monad.Freer.Extras.Stream (runStream)
import Control.Monad.Freer.State (State, gets, runState)
import Data.Bifunctor (first)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Ledger.AddressMap qualified as AM
import Ledger.Blockchain (Block, OnChainTx (Valid))
import Ledger.Slot (Slot)
import Ledger.Tx (CardanoTx)
import Plutus.ChainIndex (ChainIndexError)
import Streaming (Stream)
import Streaming qualified as S
import Streaming.Prelude (Of)
import Streaming.Prelude qualified as S
import Wallet.API (Params, WalletAPIError)
import Wallet.Emulator (EmulatorEvent, EmulatorEvent')
import Wallet.Emulator qualified as EM
import Wallet.Emulator.MultiAgent (EmulatorState, EmulatorTimeEvent (EmulatorTimeEvent), MultiAgentControlEffect,
                                   MultiAgentEffect, chainEvent, eteEvent)
import Wallet.Emulator.Wallet (Wallet, mockWalletAddress)

import Cardano.Api qualified as C
import Plutus.Contract.Trace (InitialDistribution, defaultDist, knownWallets)
import Plutus.Trace.Emulator.ContractInstance (EmulatorRuntimeError)

{- Note [Emulator event stream]

The primary way of observing the outcome of a trace is by looking at the
stream of events it produces, via 'runTraceStream'. This has the following
reasons:

* A totally ordered stream of events is a good way to characterise the
  behaviour of a dynamic system.
* By taking the stream of events as the main output of running a trace, we
  can potentially run the trace against a live system. (To really do that we'll have to change the type of log messages - 'EmulatorEvent' contains some events only make sense in the emulator. But the underlying mechanism of how the stream is produces is still the same.) See note [The Emulator Control effect]
* We have the potential of saving some work because the stream is produced
  on-demand. This also makes it possible to deal with infinite traces: We just
  evaluate them to a finite number of steps.

-}

-- | Finish the stream at the end of the given slot.
takeUntilSlot :: forall effs a. Slot -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ()
takeUntilSlot maxSlot = S.takeWhile (maybe True (\sl -> sl <= maxSlot) . preview (logMessageContent . eteEvent . chainEvent . _SlotAdd))

-- | Remove from the stream all log messages whose log level is lower than the
--   the given level.
filterLogLevel :: forall effs a. LogLevel -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
filterLogLevel lvl = S.mapMaybe (preview (filtered (\LogMessage{_logLevel} -> lvl <= _logLevel)))

-- | Apply a fold to an effectful stream of events.
foldStreamM :: forall m a b c.
    Monad m
    => L.FoldM m a b
    -> S.Stream (S.Of a) m c
    -> m (S.Of b c)
foldStreamM = L.impurely S.foldM

-- | Consume an emulator event stream.
foldEmulatorStreamM :: forall effs a b.
    L.FoldM (Eff effs) EmulatorEvent b
    -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
    -> Eff effs (S.Of b a)
foldEmulatorStreamM theFold =
    foldStreamM (L.premapM (pure . view logMessageContent) theFold)

-- | Turn an emulator action into a 'Stream' of emulator log messages, returning
--   the final state of the emulator.
runTraceStream :: forall effs a.
    EmulatorConfig
    -> Eff '[ State EmulatorState
            , LogMsg EmulatorEvent'
            , MultiAgentEffect
            , MultiAgentControlEffect
            , ChainEffect
            , ChainControlEffect
            , Error EmulatorRuntimeError
            ] (Maybe a)
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Either EmulatorErr a, EmulatorState)
runTraceStream conf@EmulatorConfig{_params} =
    fmap (first $ either Left $ maybe (Left ExitWasNeverCalled) Right)
    . S.hoist (pure . run)
    . runStream @(LogMessage EmulatorEvent) @_ @'[]
    . runState (initialState conf)
    . interpret handleLogCoroutine
    . reinterpret @_ @(LogMsg EmulatorEvent) (mkTimedLogs @EmulatorEvent')
    . runError
    . wrapError WalletErr
    . wrapError ChainIndexErr
    . wrapError AssertionErr
    . wrapError InstanceErr
    . EM.processEmulated _params
    . subsume
    . subsume @(State EmulatorState)
    . raiseEnd

data EmulatorConfig =
    EmulatorConfig
        { _initialChainState :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        , _params            :: Params -- ^ Set the protocol parameters, network ID and slot configuration for the emulator.
        } deriving (Eq, Show)

type InitialChainState = Either InitialDistribution [CardanoTx]

-- | The wallets' initial funds
initialDist :: EmulatorConfig -> InitialDistribution
initialDist EmulatorConfig{..} = either id (walletFunds . map Valid) _initialChainState where
    walletFunds :: Block -> Map Wallet C.Value
    walletFunds theBlock =
        let values = AM.values $ AM.fromChain [theBlock]
            getFunds wllt = fromMaybe mempty $ Map.lookup (mockWalletAddress wllt) values
        in Map.fromSet getFunds (Set.fromList knownWallets)

instance Default EmulatorConfig where
  def = EmulatorConfig
          { _initialChainState = Left defaultDist
          , _params = def
          }

initialState :: EmulatorConfig -> EM.EmulatorState
initialState EmulatorConfig{..} = let
    withInitialWalletValues = either
          (error . ("Cannot build the initial state: " <>) . show)
          id
          . EM.emulatorStateInitialDist _params . Map.mapKeys EM.mockWalletPaymentPubKeyHash
    in either withInitialWalletValues (EM.emulatorStatePool) _initialChainState


data EmulatorErr =
    WalletErr WalletAPIError
    | ChainIndexErr ChainIndexError
    | AssertionErr EM.AssertionError
    | InstanceErr EmulatorRuntimeError
    | ExitWasNeverCalled
    deriving (Show)

handleLogCoroutine :: forall e effs.
    Member (Yield (LogMessage e) ()) effs
    => LogMsg e
    ~> Eff effs
handleLogCoroutine = \case LMessage m -> yield m id

-- | Annotate emulator log messages with the current system time
--   (slot number)
mkTimedLogs :: forall a effs.
    ( Member (LogMsg (EmulatorTimeEvent a)) effs
    , Member (State EmulatorState) effs
    )
    => LogMsg a
    ~> Eff effs
mkTimedLogs = mapMLog f where
    f :: a -> Eff effs (EmulatorTimeEvent a)
    f a =
        EmulatorTimeEvent
            <$> gets (view $ EM.chainState . EM.chainCurrentSlot)
            <*> pure a

makeLenses ''EmulatorConfig

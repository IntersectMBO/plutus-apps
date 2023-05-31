{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- | If you want to run the node emulator without using the `Contract` monad, this module provides a simple MTL-based interface.
module Cardano.Node.Emulator.Internal.API (
  -- * Types
    EmulatorState(EmulatorState)
      , esChainState
      , esAddressMap
      , esDatumMap
  , EmulatorError(..)
  , EmulatorLogs
  , EmulatorMsg(..)
  , L.LogMessage(..)
  , MonadEmulator
  , EmulatorT
  , EmulatorM
  -- * Running Eff chain effects in MTL
  , handleChain
) where

import Cardano.Node.Emulator.Internal.Node qualified as E
import Cardano.Node.Emulator.LogMessages (EmulatorMsg (ChainEvent, GenericMsg))
import Control.Lens (makeLenses, (&))
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Freer (Eff, Member, interpret, run, type (~>))
import Control.Monad.Freer.Extras (raiseEnd)
import Control.Monad.Freer.Extras.Log qualified as L
import Control.Monad.Freer.State (State, modify, runState)
import Control.Monad.Freer.Writer qualified as F (Writer, runWriter, tell)
import Control.Monad.Identity (Identity)
import Control.Monad.RWS.Class (MonadRWS, ask, get, put, tell)
import Control.Monad.RWS.Strict (RWST)
import Data.Map (Map)
import Data.Sequence (Seq)
import Ledger (Datum, DatumHash, ToCardanoError, ValidationErrorInPhase, eitherTx, getCardanoTxData)
import Ledger.AddressMap qualified as AM


data EmulatorState = EmulatorState
  { _esChainState :: !E.ChainState
  , _esAddressMap :: !AM.AddressMap
  , _esDatumMap   :: !(Map DatumHash Datum)
  }
  deriving (Show)

makeLenses 'EmulatorState

data EmulatorError
  = BalancingError !E.BalancingError
  | ValidationError !ValidationErrorInPhase
  | ToCardanoError !ToCardanoError
  deriving (Show)

type EmulatorLogs = Seq (L.LogMessage EmulatorMsg)
type MonadEmulator m = (MonadRWS E.Params EmulatorLogs EmulatorState m, MonadError EmulatorError m)
type EmulatorT m = ExceptT EmulatorError (RWST E.Params EmulatorLogs EmulatorState m)
type EmulatorM = EmulatorT Identity

handleChain :: MonadEmulator m => Eff [E.ChainControlEffect, E.ChainEffect] a -> m a
handleChain eff = do
  params <- ask
  EmulatorState chainState am dm <- get
  let ((((a, dm'), am') , newChainState), lg) = raiseEnd eff
        & interpret (E.handleControlChain params)
        & interpret (E.handleChain params)
        & interpret handleChainLogs
        & runState dm
        & runState am
        & runState chainState
        & F.runWriter
        & run
  tell lg
  put $ EmulatorState newChainState am' dm'
  pure a
  where
    handleChainLogs
      :: ( Member (State AM.AddressMap) effs
         , Member (State (Map DatumHash Datum)) effs
         , Member (F.Writer EmulatorLogs) effs
         )
      => L.LogMsg E.ChainEvent ~> Eff effs
    handleChainLogs (L.LMessage msg@(L.LogMessage _ e)) = do
      F.tell @EmulatorLogs (pure $ ChainEvent <$> msg)
      E.chainEventOnChainTx e & maybe (pure ()) (\tx -> do
        void $ modify $ AM.updateAllAddresses tx
        void $ modify $ ((<>) . eitherTx getCardanoTxData getCardanoTxData) tx
        )

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-

A handler for the 'ContractStore'  effect that stores everything in a TVar.

-}
module Plutus.PAB.Db.Memory.ContractStore(
    InMemContractInstanceState(..)
    , handleContractStore
    , InMemInstances
    , initialInMemInstances
    ) where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Lens (at, set)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Plutus.PAB.Effects.Contract (ContractStore)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Types (ContractActivationArgs)
import Wallet.Types (ContractInstanceId)

-- | The current state of a contract instance
-- Considering InstanceState contractState as mutable to avoid bottleneck when updating InMemInstances especially
-- when an instance already exists. Note also that PutState is adjusted so as to avoid full map update
-- when instance already exists.
data InMemContractInstanceState t =
    InMemContractInstanceState
        { _contractDef   :: ContractActivationArgs (Contract.ContractDef t)
        , _contractState :: TVar (Contract.State t)
        }

newtype InMemInstances t = InMemInstances { unInMemInstances :: TVar (Map ContractInstanceId (InMemContractInstanceState t)) }

initialInMemInstances :: forall t. IO (InMemInstances t)
initialInMemInstances = InMemInstances <$> STM.newTVarIO mempty

-- | Handle the 'ContractStore' effect by writing the state to the
--   TVar in 'SimulatorState'
handleContractStore ::
    forall t effs.
    ( LastMember IO effs
    , Member (Reader (InMemInstances t)) effs
    , Member (Error PABError) effs
    )
    => ContractStore t
    ~> Eff effs
handleContractStore = \case
    Contract.PutState definition instanceId state -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        liftIO $ STM.atomically $ do
          instances <- STM.readTVar instancesTVar
          case Map.lookup instanceId instances of
              Nothing -> do
                -- adding new entry
                stateTVar <- STM.newTVar state
                let instState = InMemContractInstanceState{_contractDef = definition, _contractState = stateTVar}
                STM.modifyTVar instancesTVar (set (at instanceId) (Just instState))
              Just oldInstState -> do
                -- only update state
                STM.writeTVar (_contractState oldInstState) state

    Contract.GetState instanceId -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        instances <- liftIO $ STM.readTVarIO instancesTVar
        case Map.lookup instanceId instances of
          Nothing -> throwError (ContractInstanceNotFound instanceId)
          Just instState ->
            liftIO $ STM.atomically $ STM.readTVar $ _contractState instState

    Contract.GetContracts _ -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        fmap _contractDef <$> liftIO (STM.readTVarIO instancesTVar)
    Contract.PutStartInstance{} -> pure ()
    Contract.PutStopInstance{} -> pure ()

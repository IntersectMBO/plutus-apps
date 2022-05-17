{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-

A handler for the 'ContractStore'  effect that stores everything in a TVar.

-}
module Plutus.PAB.Db.Memory.ContractStore(
      handleContractStore
    , initialInMemInstances
    ) where

import Control.Category ((<<<))
import Control.Concurrent.STM qualified as STM
import Control.Lens (_Just, at, preview, set)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as Map
import Plutus.PAB.Db.Memory.Types (InMemContractInstanceState (InMemContractInstanceState, _contractActivityStatus, _contractDef, _contractState),
                                   InMemInstances (InMemInstances, unInMemInstances), contractState)
import Plutus.PAB.Effects.Contract (ContractStore)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Types (PABError (..))
import Wallet.Types (ContractActivityStatus (Active, Stopped))

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
            let instState = InMemContractInstanceState { _contractDef = definition , _contractState = state , _contractActivityStatus = Active }
            STM.modifyTVar instancesTVar (set (at instanceId) (Just instState))
    Contract.GetState instanceId -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        result <- preview (at instanceId . _Just . contractState) <$> liftIO (STM.readTVarIO instancesTVar)
        case result of
            Just s  -> pure s
            Nothing -> throwError (ContractInstanceNotFound instanceId)
    Contract.GetContracts status -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        let
          filterByStatus = case status of
            Just status' -> Map.filter $ \InMemContractInstanceState { _contractActivityStatus } -> _contractActivityStatus == status'
            Nothing -> id
        (fmap _contractDef <<< filterByStatus) <$> liftIO (STM.readTVarIO instancesTVar)
    Contract.PutStartInstance{} -> pure ()
    -- NOTE:
    -- This should be noop and the the internal variable `_contractActivityStatus` should has really
    -- really impact on the instances behavior. The actual contract execution loop relies
    -- on some other in memory flag and the stopping logic driven by `PutStopInstance` should be *rather* only
    -- relevant for persistent storages and PAB initialization driven by such a storage.
    Contract.PutStopInstance instanceId -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        liftIO $ STM.atomically $ do
            STM.modifyTVar instancesTVar $ \instances -> do
              let
                f mi = do
                  i <- mi
                  pure $ i { _contractActivityStatus = Stopped }
              Map.alter f instanceId instances

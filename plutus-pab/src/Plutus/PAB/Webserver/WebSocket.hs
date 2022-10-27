{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-

Handlers for the websockets exposed by the PAB.

-}
module Plutus.PAB.Webserver.WebSocket
    ( wsHandler
    , combinedWebsocket
    , contractInstanceUpdates
    -- * Reports
    , getContractReport
    -- ** Streams of PAB events
    , openEndpoints
    , slotChange
    , observableStateChange
    ) where

import Control.Concurrent.Async (Async, async, waitAnyCancel)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.Extras.Stream (STMStream, foldM, singleton, unfold, unfoldOn)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, void)
import Control.Monad.Freer.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (PubKeyHash)
import Ledger.Slot (Slot)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection (Connection, PendingConnection)
import Plutus.Contract.Effects (ActiveEndpoint)
import Plutus.Contract.Wallet (ExportTx)
import Plutus.PAB.Core (PABAction)
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Core.ContractInstance.STM (BlockchainEnv, OpenEndpoint (oepName))
import Plutus.PAB.Core.ContractInstance.STM qualified as Instances
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Events.ContractInstanceState (fromResp)
import Plutus.PAB.Types (PABError (OtherError))
import Plutus.PAB.Webserver.API ()
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient (InstanceUpdate, SlotChange),
                                   CombinedWSStreamToServer (Subscribe, Unsubscribe),
                                   ContractReport (ContractReport, crActiveContractStates, crAvailableContracts),
                                   ContractSignatureResponse (ContractSignatureResponse),
                                   InstanceStatusToClient (ContractFinished, NewActiveEndpoints, NewObservableState, NewYieldedExportTxs))
import Servant ((:<|>) ((:<|>)))
import Wallet.Types (ContractInstanceId)

getContractReport :: forall t env. Contract.PABContract t => PABAction t env (ContractReport (Contract.ContractDef t))
getContractReport = do
    availableContracts <- Contract.getDefinitions @t
    activeContractIDs <- fmap fst . Map.toList <$> Contract.getActiveContracts @t
    crAvailableContracts <-
        traverse
            (\t -> ContractSignatureResponse t <$> Contract.exportSchema @t t)
            availableContracts
    crActiveContractStates <- traverse (\i -> Contract.getState @t i >>= \s -> pure (i, fromResp $ Contract.serialisableState (Proxy @t) s)) activeContractIDs
    pure ContractReport {crAvailableContracts, crActiveContractStates}

combinedUpdates :: forall t env. WSState -> PABAction t env (STMStream CombinedWSStreamToClient)
combinedUpdates wsState =
    combinedWSStreamToClient wsState
        <$> (Core.askBlockchainEnv @t @env)

-- | The subscriptions for a websocket (wallet funds and contract instance notifications)
data WSState = WSState
    { wsInstances :: STM.TVar (Map.Map ContractInstanceId Instances.InstanceState) -- ^ Contract instances that we want updates for
    , wsWallets   :: STM.TVar (Set PubKeyHash) -- ^ Wallets whose funds we are watching
    }

combinedWSStreamToClient :: WSState -> BlockchainEnv -> STMStream CombinedWSStreamToClient
combinedWSStreamToClient WSState{wsInstances} blockchainEnv = do
    instances <- unfoldOn Map.keysSet (STM.readTVar wsInstances)
    let mkInstanceStream (instanceId, instanceState) = InstanceUpdate instanceId <$> instanceUpdates instanceState
    fold
        [ SlotChange <$> slotChange blockchainEnv
        , foldMap mkInstanceStream (Map.toList instances)
        ]

initialWSState :: STM WSState
initialWSState = WSState <$> STM.newTVar mempty <*> STM.newTVar mempty

slotChange :: BlockchainEnv -> STMStream Slot
slotChange = unfold . Instances.currentSlot

observableStateChange :: Instances.InstanceState -> STMStream JSON.Value
observableStateChange = unfold . Instances.observableContractState

openEndpoints :: Instances.InstanceState -> STMStream [ActiveEndpoint]
openEndpoints instanceState =
    unfold $ fmap (fmap (oepName . snd) . Map.toList) $ Instances.openEndpoints instanceState

yieldedExportTxsChange :: Instances.InstanceState -> STMStream [ExportTx]
yieldedExportTxsChange = unfold . Instances.yieldedExportTxs

finalValue :: Instances.InstanceState -> STMStream (Maybe JSON.Value)
finalValue = singleton . Instances.finalResult

-- | Get a stream of instance updates for a given 'InstanceState'
instanceUpdates :: Instances.InstanceState -> STMStream InstanceStatusToClient
instanceUpdates instanceState =
    fold $
        [ NewObservableState  <$> observableStateChange instanceState
        , NewActiveEndpoints  <$> openEndpoints instanceState
        , NewYieldedExportTxs <$> yieldedExportTxsChange instanceState
        , ContractFinished    <$> finalValue instanceState
        ]

-- | Send all updates from an 'STMStream' to a websocket until it finishes.
streamToWebsocket :: forall t env a. ToJSON a => Connection -> STMStream a -> PABAction t env ()
streamToWebsocket connection stream = liftIO $
    foldM stream (WS.sendTextData connection . JSON.encode) (pure ())

-- | Handler for WSAPI
wsHandler ::
    forall t env.
    (ContractInstanceId -> PendingConnection -> PABAction t env ())
    :<|> (PendingConnection -> PABAction t env ())
wsHandler =
    contractInstanceUpdates :<|> combinedWebsocket

sendContractInstanceUpdatesToClient :: forall t env. ContractInstanceId -> Connection -> PABAction t env ()
sendContractInstanceUpdatesToClient instanceId connection = do
    instanceState <- Core.instanceStateInternal @t @env instanceId
    streamToWebsocket connection (instanceUpdates instanceState)

contractInstanceUpdates :: forall t env. ContractInstanceId -> PendingConnection -> PABAction t env ()
contractInstanceUpdates contractInstanceId pending = do
    Core.PABRunner{Core.runPABAction} <- Core.pabRunner
    liftIO $ do
        connection <- WS.acceptRequest pending
        handle disconnect . WS.withPingThread connection 30 (pure ()) $ fmap (either (error . show) id) . runPABAction $ sendContractInstanceUpdatesToClient contractInstanceId connection
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

combinedWebsocket :: forall t env. PendingConnection -> PABAction t env ()
combinedWebsocket pending = do
    pabRunner <- Core.pabRunner
    wsState <- liftIO $ STM.atomically initialWSState
    liftIO $ do
        connection <- WS.acceptRequest pending
        handle disconnect . WS.withPingThread connection 30 (pure ()) $ combinedWebsocketThread pabRunner wsState connection
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

combinedWebsocketThread :: forall t env. Core.PABRunner t env -> WSState -> Connection -> IO ()
combinedWebsocketThread Core.PABRunner{Core.runPABAction} wsState connection = do
        tasks :: [Async (Either PABError ())] <-
            traverse
                asyncApp
                [ sendCombinedUpdatesToClient connection wsState
                , receiveMessagesFromClient connection wsState
                ]
        void $ waitAnyCancel tasks
    where
        asyncApp = async . runPABAction

sendCombinedUpdatesToClient :: forall t env. Connection -> WSState -> PABAction t env ()
sendCombinedUpdatesToClient connection wsState = combinedUpdates wsState >>= streamToWebsocket connection

receiveMessagesFromClient :: forall t env. Connection -> WSState -> PABAction t env ()
receiveMessagesFromClient connection wsState = forever $ do
    msg <- liftIO $ WS.receiveData connection
    let result :: Either Text CombinedWSStreamToServer
        result = first Text.pack $ JSON.eitherDecode msg
    case result of
        Right (Subscribe (Right l)) -> liftIO $ STM.atomically $ addWallet wsState l
        Right (Subscribe (Left i))  -> do
            state <- Core.instanceStateInternal i
            liftIO $ STM.atomically $ addInstanceId wsState i state
        Right (Unsubscribe l) -> liftIO $ STM.atomically $ either (removeInstanceId wsState) (removeWallet wsState) l
        Left e                -> throwError (OtherError e)

addInstanceId :: WSState -> ContractInstanceId -> Instances.InstanceState -> STM ()
addInstanceId WSState{wsInstances} k v = STM.modifyTVar wsInstances (Map.insert k v)

addWallet :: WSState -> PubKeyHash -> STM ()
addWallet WSState{wsWallets} w = STM.modifyTVar wsWallets (Set.insert w)

removeInstanceId :: WSState -> ContractInstanceId -> STM ()
removeInstanceId WSState{wsInstances} i = STM.modifyTVar wsInstances (Map.delete i)

removeWallet :: WSState -> PubKeyHash -> STM ()
removeWallet WSState{wsWallets} w = STM.modifyTVar wsWallets (Set.delete w)

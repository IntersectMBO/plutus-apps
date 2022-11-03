{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-

Start the threads for contract instances

-}
module Plutus.PAB.Core.ContractInstance(
    RequestHandlers.ContractInstanceMsg(..)
    , activateContractSTM
    , activateContractSTM'
    , initContractInstanceState
    , ContractInstanceState(..)
    , updateState
    -- * STM instances
    , startSTMInstanceThread
    , startContractInstanceThread'
    , AppBackendConstraints
    -- * Calling endpoints
    , callEndpointOnInstance
    -- * Indexed block
    ) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow ((>>>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens (preview)
import Control.Lens.Operators
import Control.Monad (forM_)
import Control.Monad.Freer (Eff, LastMember, Member, raise, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, LogObserve, logDebug, logInfo)
import Control.Monad.Freer.NonDet (NonDet)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value)
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as Text
import Ledger (TxId, TxOutRef (..))
import Plutus.ChainIndex (ChainIndexQueryEffect, Depth (..), RollbackState (..), TxConfirmedState (..), TxOutState (..),
                          TxOutStatus, TxStatus, TxValidity (..), transactionOutputState)
import Plutus.ChainIndex.UtxoState (UtxoState (_usTxUtxoData), utxoState)
import Plutus.Contract.Effects (ActiveEndpoint (aeDescription),
                                PABReq (AwaitUtxoProducedReq, AwaitUtxoSpentReq, ExposeEndpointReq),
                                PABResp (AwaitSlotResp, AwaitTimeResp, AwaitTxOutStatusChangeResp, AwaitTxStatusChangeResp, AwaitUtxoProducedResp, AwaitUtxoSpentResp, ExposeEndpointResp))
import Plutus.Contract.Effects qualified as Contract.Effects
import Plutus.Contract.Resumable (Request (Request, itID, rqID, rqRequest), Response (Response))
import Plutus.Contract.State (ContractResponse (ContractResponse, err, hooks, newState), State (State, observableState))
import Plutus.Contract.Trace qualified as RequestHandler
import Plutus.Contract.Trace.RequestHandler (RequestHandler (RequestHandler), RequestHandlerLogMsg, extract,
                                             maybeToHandler, tryHandler', wrapHandler)
import Plutus.PAB.Core.ContractInstance.RequestHandlers (ContractInstanceMsg (ActivatedContractInstance, HandlingRequests, InitialisingContract))
import Plutus.PAB.Core.ContractInstance.RequestHandlers qualified as RequestHandlers
import Plutus.PAB.Core.ContractInstance.STM (Activity (Done, Stopped), BlockchainEnv (..),
                                             InstanceState (InstanceState, issStop), InstancesState,
                                             callEndpointOnInstance, emptyInstanceState)
import Plutus.PAB.Core.ContractInstance.STM qualified as InstanceState
import Plutus.PAB.Core.Indexer.TxConfirmationStatus (TCSIndex)
import Plutus.PAB.Effects.Contract (ContractEffect, ContractStore, PABContract (ContractDef, serialisableState))
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Effects.UUID (UUIDEffect, uuidNextRandom)
import Plutus.PAB.Events.Contract (ContractInstanceId (ContractInstanceId))
import Plutus.PAB.Types (PABError)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID, caWallet))
import RewindableIndex.Index.VSplit qualified as Ix
import Wallet.Effects (NodeClientEffect, WalletEffect)
import Wallet.Emulator.LogMessages (TxBalanceMsg)
import Wallet.Emulator.Wallet qualified as Wallet

-- | Container for holding a few bits of state related to the contract
-- instance that we may want to pass in.
data ContractInstanceState t =
  ContractInstanceState
    { contractState :: Contract.State t
    , stmState      :: STM InstanceState
    }

-- | Create a new instance of the contract, but where the
-- activeContractInstanceId and the initial state are provided.
activateContractSTM' ::
    forall t m appBackend effs.
    ( Member (LogMsg (ContractInstanceMsg t)) effs
    , Member (ContractStore t) effs
    , Member (Reader InstancesState) effs
    , Contract.PABContract t
    , AppBackendConstraints t m appBackend
    , LastMember m (Reader ContractInstanceId ': appBackend)
    , LastMember m effs
    )
    => ContractInstanceState t
    -> ContractInstanceId
    -> (ContractInstanceId -> Eff appBackend ~> IO)
    -> ContractActivationArgs (ContractDef t)
    -> Eff effs ContractInstanceId
activateContractSTM' c@ContractInstanceState{contractState} activeContractInstanceId runAppBackend a@ContractActivationArgs{caID, caWallet} = do
  logInfo @(ContractInstanceMsg t) $ InitialisingContract caID activeContractInstanceId
  Contract.putStartInstance @t a activeContractInstanceId
  Contract.putState @t a activeContractInstanceId contractState
  cid <- startContractInstanceThread' c activeContractInstanceId runAppBackend a
  let wallet = fromMaybe (Wallet.knownWallet 1) caWallet
  logInfo @(ContractInstanceMsg t) $ ActivatedContractInstance caID wallet activeContractInstanceId
  pure cid

-- | Spin up the STM Instance thread for the provided contract and add it to
-- the STM instance state.
startContractInstanceThread' ::
    forall t m appBackend effs.
    ( Member (Reader InstancesState) effs
    , Contract.PABContract t
    , AppBackendConstraints t m appBackend
    , LastMember m (Reader ContractInstanceId ': appBackend)
    , LastMember m effs
    )
    => ContractInstanceState t
    -> ContractInstanceId
    -> (ContractInstanceId -> Eff appBackend ~> IO)
    -> ContractActivationArgs (ContractDef t)
    -> Eff effs ContractInstanceId
startContractInstanceThread' ContractInstanceState{stmState} activeContractInstanceId runAppBackend a = do
  s <- startSTMInstanceThread'
    @t @m stmState runAppBackend a activeContractInstanceId
  ask >>= liftIO . InstanceState.insertInstance activeContractInstanceId s
  pure activeContractInstanceId

-- | Create a new instance of the contract
activateContractSTM ::
    forall t m appBackend effs.
    ( Member (LogMsg (ContractInstanceMsg t)) effs
    , Member UUIDEffect effs
    , Member (ContractEffect t) effs
    , Member (ContractStore t) effs
    , Member (Reader InstancesState) effs
    , Contract.PABContract t
    , AppBackendConstraints t m appBackend
    , LastMember m (Reader ContractInstanceId ': appBackend)
    , LastMember m effs
    )
    => (ContractInstanceId -> Eff appBackend ~> IO)
    -> ContractActivationArgs (ContractDef t)
    -> Eff effs ContractInstanceId
activateContractSTM runAppBackend a = do
  (cid, initState) <- initContractInstanceState a
  activateContractSTM' @t @m @appBackend @effs initState cid runAppBackend a

-- | Build a new ContractInstanceState and return it, along with
-- the corresponding new intsance id.
initContractInstanceState ::
    forall t effs.
    ( Member UUIDEffect effs
    , Member (ContractEffect t) effs
    , Contract.PABContract t
    )
    => ContractActivationArgs (ContractDef t)
    -> Eff effs (ContractInstanceId, ContractInstanceState t)
initContractInstanceState ContractActivationArgs{caID} = do
  activeContractInstanceId <- ContractInstanceId <$> uuidNextRandom
  initialState <- Contract.initialState @t activeContractInstanceId caID
  pure (activeContractInstanceId, ContractInstanceState initialState emptyInstanceState)

processAwaitSlotRequestsSTM ::
    forall effs.
    ( Member (Reader BlockchainEnv) effs
    )
    => RequestHandler effs PABReq (STM PABResp)
processAwaitSlotRequestsSTM =
    maybeToHandler (extract Contract.Effects._AwaitSlotReq)
    >>> (RequestHandler $ \targetSlot_ -> fmap AwaitSlotResp . InstanceState.awaitSlot targetSlot_ <$> ask)

processAwaitTimeRequestsSTM ::
    forall effs.
    ( Member (Reader BlockchainEnv) effs
    )
    => RequestHandler effs PABReq (STM PABResp)
processAwaitTimeRequestsSTM =
    maybeToHandler (extract Contract.Effects._AwaitTimeReq) >>>
        (RequestHandler $ \time ->
            fmap AwaitTimeResp . InstanceState.awaitTime time <$> ask
        )

processTxStatusChangeRequestsSTM ::
    forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member (Reader BlockchainEnv) (NonDet : effs)
    )
    => RequestHandler effs PABReq (STM PABResp)
processTxStatusChangeRequestsSTM =
    maybeToHandler (extract Contract.Effects._AwaitTxStatusChangeReq)
    >>> RequestHandler handler
    where
        handler txId = do
            env <- ask
            case InstanceState.beTxChanges env of
              Left _      ->
                  pure (AwaitTxStatusChangeResp txId <$> InstanceState.waitForTxStatusChange Unknown txId env)
              Right ixRef -> do
                  txStatus <- raise . liftIO $ processTxStatusChangeRequestIO ixRef env txId
                  pure (AwaitTxStatusChangeResp txId <$> txStatus)

processTxStatusChangeRequestIO
  :: IORef TCSIndex
  -> BlockchainEnv
  -> TxId
  -> IO (STM TxStatus)
processTxStatusChangeRequestIO ixRef env txId = do
    ix           <- readIORef ixRef
    _blockNumber <- STM.readTVarIO $ InstanceState.beLastSyncedBlockNo env
    events       <- Ix.getEvents (ix ^. Ix.storage)
    queryResult  <- (ix ^. Ix.query) ix txId events
    pure . pure $ case queryResult of
        -- On this branch the transaction has not yet been indexed. This means
        -- that the transaction status has not changed from `Unknown` which is
        -- why we wait and re-poll.
        Nothing -> Unknown
        -- If we get any kind of update we can return. Due to the way the indexer
        -- works we can compute if the tx has been confirmed or not.
        Just (TxConfirmedState (Sum 0) _ _) -> Committed TxValid ()
        Just (TxConfirmedState (Sum n) _ _) ->
            TentativelyConfirmed (Depth n) TxValid ()

processTxOutStatusChangeRequestsSTM ::
    forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member (Reader BlockchainEnv) effs
    )
    => RequestHandler effs PABReq (STM PABResp)
processTxOutStatusChangeRequestsSTM =
    maybeToHandler (extract Contract.Effects._AwaitTxOutStatusChangeReq)
    >>> RequestHandler handler
    where
        handler txOutRef = do
            env <- ask
            case InstanceState.beTxChanges env of
              Left _ ->
                pure (AwaitTxOutStatusChangeResp txOutRef <$> InstanceState.waitForTxOutStatusChange Unknown txOutRef env)
              Right txChange -> do
                 txOutStatus <- raise . liftIO $ processTxOutStatusChangeRequestsIO txChange env txOutRef
                 pure (AwaitTxOutStatusChangeResp txOutRef <$> txOutStatus)

processTxOutStatusChangeRequestsIO
  :: IORef TCSIndex
  -> BlockchainEnv
  -> TxOutRef
  -> IO (STM TxOutStatus)
processTxOutStatusChangeRequestsIO tcsIx BlockchainEnv{beTxOutChanges} txOutRef = do
  txOutBalance  <- _usTxUtxoData . utxoState <$> STM.atomically (STM.readTVar beTxOutChanges)
  case transactionOutputState txOutBalance txOutRef of
    Nothing             -> pure empty
    Just s@(Spent txId) -> queryTx s txId
    Just s@(Unspent)    -> queryTx s $ txOutRefId txOutRef
  where
    queryTx :: TxOutState -> TxId -> IO (STM TxOutStatus)
    queryTx s txId = do
      ix <- readIORef tcsIx
      events <- Ix.getEvents (ix ^. Ix.storage)
      queryResult <- (ix ^. Ix.query) ix txId events
      pure . pure $ case queryResult of
        Nothing -> Unknown
        Just (TxConfirmedState (Sum 0) _ _) -> Committed TxValid s
        Just (TxConfirmedState (Sum n) _ _) ->
          TentativelyConfirmed (Depth n) TxValid s

processUtxoSpentRequestsSTM ::
    forall effs.
    ( Member (Reader InstanceState) effs
    )
    => RequestHandler effs (Request PABReq) (Response (STM PABResp))
processUtxoSpentRequestsSTM = RequestHandler $ \req -> do
    case traverse (preview Contract.Effects._AwaitUtxoSpentReq) req of
        Just request@Request{rqID, itID} -> do
            env <- ask
            pure $ Response rqID itID (AwaitUtxoSpentResp <$> InstanceState.waitForUtxoSpent request env)
        _ -> empty

processUtxoProducedRequestsSTM ::
    forall effs.
    ( Member (Reader InstanceState) effs
    )
    => RequestHandler effs (Request PABReq) (Response (STM PABResp))
processUtxoProducedRequestsSTM = RequestHandler $ \req -> do
    case traverse (preview Contract.Effects._AwaitUtxoProducedReq) req of
        Just request@Request{rqID, itID} -> do
            env <- ask
            pure $ Response rqID itID (AwaitUtxoProducedResp <$> InstanceState.waitForUtxoProduced request env)
        _ -> empty

processEndpointRequestsSTM ::
    forall effs.
    ( Member (Reader InstanceState) effs
    )
    => RequestHandler effs (Request PABReq) (Response (STM PABResp))
processEndpointRequestsSTM =
    maybeToHandler (traverse (extract Contract.Effects._ExposeEndpointReq))
    >>> (RequestHandler $ \q@Request{rqID, itID, rqRequest} -> fmap (Response rqID itID) (fmap (ExposeEndpointResp (aeDescription rqRequest)) . InstanceState.awaitEndpointResponse q <$> ask))

-- | 'RequestHandler' that uses TVars to wait for events
stmRequestHandler ::
    forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member ChainIndexQueryEffect effs
    , Member WalletEffect effs
    , Member NodeClientEffect effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member (LogObserve (LogMessage Text.Text)) effs
    , Member (Reader ContractInstanceId) effs
    , Member (Reader BlockchainEnv) effs
    , Member (Reader InstanceState) effs
    )
    => RequestHandler effs (Request PABReq) (STM (Response PABResp))
stmRequestHandler = fmap sequence (wrapHandler (fmap pure nonBlockingRequests) <> blockingRequests) where

    -- requests that can be handled by 'WalletEffect', 'ChainIndexQueryEffect', etc.
    nonBlockingRequests =
        RequestHandler.handleOwnAddressesQueries @effs
        <> RequestHandler.handleChainIndexQueries @effs
        <> RequestHandler.handleUnbalancedTransactions @effs
        <> RequestHandler.handlePendingTransactions @effs
        <> RequestHandler.handleOwnInstanceIdQueries @effs
        <> RequestHandler.handleCurrentNodeClientSlotQueries @effs
        <> RequestHandler.handleCurrentChainIndexSlotQueries @effs
        <> RequestHandler.handleCurrentNodeClientTimeRangeQueries @effs
        <> RequestHandler.handleYieldedUnbalancedTx @effs
        <> RequestHandler.handleAdjustUnbalancedTx @effs
        <> RequestHandler.handleGetParams @effs

    -- requests that wait for changes to happen
    blockingRequests =
        wrapHandler (processAwaitSlotRequestsSTM @effs)
        <> wrapHandler (processAwaitTimeRequestsSTM @effs)
        <> wrapHandler (processTxStatusChangeRequestsSTM @_ @effs)
        <> wrapHandler (processTxOutStatusChangeRequestsSTM @_ @effs)
        <> processEndpointRequestsSTM @effs
        <> processUtxoSpentRequestsSTM @effs
        <> processUtxoProducedRequestsSTM @effs

-- | Start the thread for the contract instance
startSTMInstanceThread' ::
    forall t m appBackend effs.
    ( LastMember m effs
    , Contract.PABContract t
    , AppBackendConstraints t m appBackend
    , LastMember m (Reader InstanceState ': Reader ContractInstanceId ': appBackend)
    )
    => STM InstanceState
    -> (ContractInstanceId -> Eff appBackend ~> IO)
    -> ContractActivationArgs (ContractDef t)
    -> ContractInstanceId
    -> Eff effs InstanceState
startSTMInstanceThread' stmState runAppBackend def instanceID =  do
    state <- liftIO $ STM.atomically stmState
    _ <- liftIO
        $ forkIO
        $ runAppBackend instanceID
        $ runReader instanceID
        $ runReader state
        $ stmInstanceLoop @t @m @(Reader InstanceState ': Reader ContractInstanceId ': appBackend) def instanceID
    pure state

-- | Start the thread for the contract instance
startSTMInstanceThread ::
    forall t m appBackend effs.
    ( LastMember m effs
    , Contract.PABContract t
    , AppBackendConstraints t m appBackend
    , LastMember m (Reader InstanceState ': Reader ContractInstanceId ': appBackend)
    )
    => (ContractInstanceId -> Eff appBackend ~> IO)
    -> ContractActivationArgs (ContractDef t)
    -> ContractInstanceId
    -> Eff effs InstanceState
startSTMInstanceThread = startSTMInstanceThread' @t @m @appBackend emptyInstanceState

type AppBackendConstraints t m effs =
    ( LastMember m effs
    , MonadIO m
    , Member (Error PABError) effs
    , Member (LogMsg (ContractInstanceMsg t)) effs
    , Member ChainIndexQueryEffect effs
    , Member WalletEffect effs
    , Member NodeClientEffect effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member (LogObserve (LogMessage Text.Text)) effs
    , Member (LogMsg TxBalanceMsg) effs
    , Member (Reader BlockchainEnv) effs
    , Member (ContractEffect t) effs
    , Member (ContractStore t) effs
    )

-- | Handle requests using 'respondToRequestsSTM' until the contract is done.
stmInstanceLoop ::
    forall t m effs.
    ( AppBackendConstraints t m effs
    , Member (Reader InstanceState) effs
    , Member (Reader ContractInstanceId) effs
    , Contract.PABContract t
    )
    => ContractActivationArgs (ContractDef t)
    -> ContractInstanceId
    -> Eff effs ()
stmInstanceLoop def instanceId = do
    (currentState :: Contract.State t) <- Contract.getState @t instanceId
    InstanceState{issStop} <- ask
    let resp = serialisableState (Proxy @t) currentState
    updateState resp
    case Contract.requests @t currentState of
        [] -> do
            let ContractResponse{err} = resp
            ask >>= liftIO . STM.atomically . InstanceState.setActivity (Done err)
        _ -> do
            response <- respondToRequestsSTM @_ @t instanceId currentState
            let rsp' = Right <$> response
                stop = Left <$> STM.takeTMVar issStop
            event <- liftIO $ STM.atomically (stop <|> rsp')
            case event of
                Left () -> do
                    ask >>= liftIO . STM.atomically . InstanceState.setActivity Stopped
                Right event' -> do
                    (newState :: Contract.State t) <- Contract.updateContract @t instanceId (caID def) currentState event'
                    Contract.putState @t def instanceId newState
                    stmInstanceLoop @t def instanceId

-- | Update the TVars in the 'InstanceState' with data from the list
--   of requests.
updateState ::
    forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member (Reader InstanceState) effs
    )
    => ContractResponse Value Value PABResp PABReq
    -> Eff effs ()
updateState ContractResponse{newState = State{observableState}, hooks} = do
    state <- ask
    liftIO $ STM.atomically $ do
        InstanceState.clearEndpoints state
        forM_ hooks $ \r -> do
            case rqRequest r of
                ExposeEndpointReq endpoint -> InstanceState.addEndpoint (r { rqRequest = endpoint}) state
                AwaitUtxoSpentReq txOutRef -> InstanceState.addUtxoSpentReq (r { rqRequest = txOutRef }) state
                AwaitUtxoProducedReq addr  -> InstanceState.addUtxoProducedReq (r { rqRequest = addr }) state
                _                          -> pure ()
        InstanceState.setObservableState observableState state

-- | Run the STM-based request handler on a non-empty list
--   of requests.
respondToRequestsSTM ::
    forall m t effs.
    ( LastMember m effs
    , MonadIO m
    , Member ChainIndexQueryEffect effs
    , Member WalletEffect effs
    , Member NodeClientEffect effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member (LogObserve (LogMessage Text.Text)) effs
    , Member (LogMsg (ContractInstanceMsg t)) effs
    , Member (Reader ContractInstanceId) effs
    , Member (Reader BlockchainEnv) effs
    , Member (Reader InstanceState) effs
    , Contract.PABContract t
    )
    => ContractInstanceId
    -> Contract.State t
    -> Eff effs (STM (Response PABResp))
respondToRequestsSTM instanceId currentState = do
    let rqs = Contract.requests @t currentState
    logDebug @(ContractInstanceMsg t) $ HandlingRequests instanceId rqs
    tryHandler' stmRequestHandler rqs

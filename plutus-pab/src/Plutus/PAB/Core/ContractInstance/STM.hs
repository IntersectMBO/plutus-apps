{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-

Types and functions for contract instances that communicate with the outside
world via STM. See note [Contract instance thread model].

-}
module Plutus.PAB.Core.ContractInstance.STM(
    BlockchainEnv(..)
    , emptyBlockchainEnv
    , awaitSlot
    , awaitTime
    , awaitEndpointResponse
    , waitForTxStatusChange
    , updateTxChangesR
    , waitForTxOutStatusChange
    , currentSlot
    , lastSyncedBlockSlot
    , InstanceState(..)
    , emptyInstanceState
    , OpenEndpoint(..)
    , OpenTxOutProducedRequest(..)
    , OpenTxOutSpentRequest(..)
    , clearEndpoints
    , addEndpoint
    , addUtxoSpentReq
    , waitForUtxoSpent
    , addUtxoProducedReq
    , waitForUtxoProduced
    , setActivity
    , setObservableState
    , openEndpoints
    , callEndpoint
    , finalResult
    , Activity(..)
    -- * State of all running contract instances
    , InstancesState
    , emptyInstancesState
    , insertInstance
    , removeInstance
    , callEndpointOnInstance
    , callEndpointOnInstanceTimeout
    , observableContractState
    , yieldedExportTxs
    , instanceState
    , instanceIDs
    , instancesWithStatuses
    , instancesClientEnv
    , InstanceClientEnv(..)
    ) where

import Cardano.Node.Emulator.Internal.Node.Params (Params (pSlotConfig))
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Cardano.Wallet.LocalClient.ExportTx (ExportTx)
import Control.Applicative (Alternative (empty))
import Control.Concurrent.STM (STM, TMVar, TVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad (guard)
import Data.Aeson (Value)
import Data.Foldable (fold)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Ledger (CardanoAddress, Slot, TxOutRef)
import Ledger.Time (POSIXTime)
import Plutus.ChainIndex (BlockNumber (BlockNumber), ChainIndexTx, TxIdState, TxOutBalance, TxOutStatus, TxStatus,
                          transactionStatus)
import Plutus.ChainIndex.TxOutBalance (transactionOutputStatus)
import Plutus.ChainIndex.UtxoState (UtxoIndex, UtxoState (_usTxUtxoData), utxoState)
import Plutus.Contract.Effects (ActiveEndpoint (ActiveEndpoint, aeDescription))
import Plutus.Contract.Resumable (IterationID, Request (Request, itID, rqID, rqRequest), RequestID)
import Plutus.PAB.Core.Indexer.TxConfirmationStatus (TCSIndex)
import Plutus.V1.Ledger.Api (TxId)
import Wallet.Types (ContractInstanceId, EndpointDescription, EndpointValue (EndpointValue),
                     NotificationError (EndpointNotAvailable, InstanceDoesNotExist, MoreThanOneEndpointAvailable))
import Wallet.Types qualified as Wallet (ContractActivityStatus (Active, Done, Stopped))

{- Note [Contract instance thread model]

In the PAB we run each contract instance in its own thread, following the
design principles for concurrency described in [1].

As a result
* We use STM for concurrency
* We try to avoid queues and use TVars where possible

Contract instances can make requests to services that are managed by the PAB,
such as the wallet backend, the chain index and the node. From the contract's
POV we assume that these requests are responded to instantaneously. To handle
these requests the PAB uses the
'Wallet.Emulator.MultiAgent.EmulatedWalletEffects' list of effects.

In addition to making requests to PAB services, contract instances can wait for
events to happen. The events that can be waited upon are produced by the
blockchain (transactions added to the ledger, new slots starting) and by
external clients of the PAB including end-users (calling contract endpoints).
To handle this kind of waiting the PAB uses STM and the types defined in this
module.

# QoS

One of the main goals of the queueless STM design is to avoid a degradation
of the quality of service (QoS) when the system is operating at capacity. This
comes at a price: When the system is under pressure, some updates may be
dropped. In practice this a result of the behaviour of STM's 'retry' primitive,
which only guarantees to retry at some point (not immediately) after a variable
has changed. So if the variable changes again before the retry happens, the
intermediate state is not visible.

# Event types

What does this imply for the PAB? Rather than being notified of changes, we want
to be notified of new states. Therefore we choose the following types for the
events that we want to know about.

* Time: TVar with current time
* Modifications to an address: TVar with current UTXO set of that address
* Transactions & rollbacks: TVar with current status of transactions
* Endpoints: For each endpoint a TMVar that changes from empty to full if & when
  the endpoint gets called.

All other requests of the contract are handled using the wallet effects (chain index,
tx construction and signing).

[1] Keynote by Duncan Coutts at the Haskell Symposium 2020. https://vimeo.com/452222780.

-}

-- | An open endpoint that can be responded to.
data OpenEndpoint =
        OpenEndpoint
            { oepName     :: ActiveEndpoint -- ^ Name of the endpoint
            , oepResponse :: TMVar (EndpointValue Value) -- ^ A place to write the response to.
            }

-- | A TxOutRef that a contract instance is watching
data OpenTxOutSpentRequest =
    OpenTxOutSpentRequest
        { osrOutRef     :: TxOutRef -- ^ The 'TxOutRef' that the instance is watching
        , osrSpendingTx :: TMVar ChainIndexTx -- ^ A place to write the spending transaction to
        }

data OpenTxOutProducedRequest =
    OpenTxOutProducedRequest
        { otxAddress       :: CardanoAddress -- ^ 'Address' that the contract instance is watching (TODO: Should be ViewAddress -- SCP-2628)
        , otxProducingTxns :: TMVar (NonEmpty ChainIndexTx) -- ^ A place to write the producing transactions to
        }

-- | Data about the blockchain that contract instances
--   may be interested in.
data BlockchainEnv =
    BlockchainEnv
        { beRollbackHistory     :: Maybe Int -- ^ How much history do we retain in the environment. Zero signifies no trimming is done.
        , beCurrentSlot         :: TVar Slot -- ^ Actual current slot
        , beLastSyncedBlockSlot :: TVar Slot -- ^ Slot of the last synced block from 'startNodeClient'
        , beLastSyncedBlockNo   :: TVar BlockNumber -- ^ Last synced block number from 'startNodeClient'.
        , beTxChanges           :: Either (TVar (UtxoIndex TxIdState)) (IORef TCSIndex)-- ^ Map holding metadata which determines the status of transactions.
        , beTxOutChanges        :: TVar (UtxoIndex TxOutBalance) -- ^ Map holding metadata which determines the status of transaction outputs.
        , beParams              :: Params -- ^ The set of parameters, like protocol parameters and slot configuration.
        }

updateTxChangesR
  :: Either (TVar (UtxoIndex TxIdState)) (IORef TCSIndex)
  -> (TCSIndex -> IO TCSIndex)
  -> IO ()
updateTxChangesR env f =
    case env of
      Left  _     -> pure ()
      Right ixRef -> IORef.readIORef ixRef >>= f >>= IORef.writeIORef ixRef

-- | Initialise an empty 'BlockchainEnv' value
emptyBlockchainEnv :: Maybe Int -> Params -> STM BlockchainEnv
emptyBlockchainEnv rollbackHistory params =
    BlockchainEnv rollbackHistory
        <$> STM.newTVar 0
        <*> STM.newTVar 0
        <*> STM.newTVar (BlockNumber 0)
        <*> (Left <$> STM.newTVar mempty)
        <*> STM.newTVar mempty
        <*> pure params

-- | Wait until the current slot is greater than or equal to the
--   target slot, then return the current slot.
awaitSlot :: Slot -> BlockchainEnv -> STM Slot
awaitSlot targetSlot BlockchainEnv{beCurrentSlot} = do
    current <- STM.readTVar beCurrentSlot
    guard (current >= targetSlot)
    pure current

-- | Wait until the current time is greater than or equal to the
-- target time, then return the current time.
awaitTime :: POSIXTime -> BlockchainEnv -> STM POSIXTime
awaitTime targetTime be@BlockchainEnv{beParams} = do
    let slotConfig = pSlotConfig beParams
    let targetSlot = TimeSlot.posixTimeToEnclosingSlot slotConfig targetTime
    TimeSlot.slotToEndPOSIXTime slotConfig <$> awaitSlot targetSlot be

-- | Wait for an endpoint response.
awaitEndpointResponse :: Request ActiveEndpoint -> InstanceState -> STM (EndpointValue Value)
awaitEndpointResponse Request{rqID, itID} InstanceState{issEndpoints} = do
    currentEndpoints <- STM.readTVar issEndpoints
    let openEndpoint = Map.lookup (rqID, itID) currentEndpoints
    case openEndpoint of
        Nothing                        -> empty
        Just OpenEndpoint{oepResponse} -> STM.readTMVar oepResponse

-- | Whether the contract instance is still waiting for an event.
data Activity =
        Active
        | Stopped -- ^ Instance was stopped before all requests were handled
        | Done (Maybe Value) -- ^ Instance finished, possibly with an error
        deriving (Eq, Show)

-- | The state of an active contract instance.
data InstanceState =
    InstanceState
        { issEndpoints        :: TVar (Map (RequestID, IterationID) OpenEndpoint) -- ^ Open endpoints that can be responded to.
        , issStatus           :: TVar Activity -- ^ Whether the instance is still running.
        , issObservableState  :: TVar (Maybe Value) -- ^ Serialised observable state of the contract instance (if available)
        , issStop             :: TMVar () -- ^ Stop the instance if a value is written into the TMVar.
        , issTxOutRefs        :: TVar (Map (RequestID, IterationID) OpenTxOutSpentRequest)
        , issAddressRefs      :: TVar (Map (RequestID, IterationID) OpenTxOutProducedRequest)
        , issYieldedExportTxs :: TVar [ExportTx] -- ^ Partial tx that needs to be balanced, signed and submitted by an external agent.
        }

-- | An 'InstanceState' value with empty fields
emptyInstanceState :: STM InstanceState
emptyInstanceState =
    InstanceState
        <$> STM.newTVar mempty
        <*> STM.newTVar Active
        <*> STM.newTVar Nothing
        <*> STM.newEmptyTMVar
        <*> STM.newTVar mempty
        <*> STM.newTVar mempty
        <*> STM.newTVar mempty

-- | Events that the contract instances are waiting for, indexed by keys that are
--   readily available in the node client (ie. that can be produced from just a
--   block without any additional information)
data InstanceClientEnv = InstanceClientEnv
  { ceUtxoSpentRequests    :: Map TxOutRef [OpenTxOutSpentRequest]
  , ceUtxoProducedRequests :: Map CardanoAddress [OpenTxOutProducedRequest] -- TODO: ViewAddress
  }

instance Semigroup InstanceClientEnv where
    l <> r =
        InstanceClientEnv
            { ceUtxoProducedRequests = Map.unionWith (<>) (ceUtxoProducedRequests l) (ceUtxoProducedRequests r)
            , ceUtxoSpentRequests = Map.unionWith (<>) (ceUtxoSpentRequests l) (ceUtxoSpentRequests r)
            }

instance Monoid InstanceClientEnv where
    mappend = (<>)
    mempty = InstanceClientEnv mempty mempty

instancesClientEnv :: InstancesState -> IO (STM InstanceClientEnv)
instancesClientEnv = fmap (fmap fold . traverse instanceClientEnv) . IORef.readIORef . getInstancesState

instanceClientEnv :: InstanceState -> STM InstanceClientEnv
instanceClientEnv InstanceState{issTxOutRefs, issAddressRefs} =
  InstanceClientEnv
    <$> (Map.fromList . fmap ((\r@OpenTxOutSpentRequest{osrOutRef} -> (osrOutRef, [r])) . snd) . Map.toList <$> STM.readTVar issTxOutRefs)
    <*> (Map.fromList . fmap ((\r@OpenTxOutProducedRequest{otxAddress} -> (otxAddress, [r])) . snd) . Map.toList  <$> STM.readTVar issAddressRefs)

-- | Set the 'Activity' of the instance
setActivity :: Activity -> InstanceState -> STM ()
setActivity a InstanceState{issStatus} = STM.writeTVar issStatus a

-- | Empty the list of open enpoints that can be called on the instance
clearEndpoints :: InstanceState -> STM ()
clearEndpoints InstanceState{issEndpoints, issTxOutRefs, issAddressRefs} = do
    STM.writeTVar issEndpoints Map.empty
    STM.writeTVar issTxOutRefs Map.empty
    STM.writeTVar issAddressRefs Map.empty

-- | Add an active endpoint to the instance's list of active endpoints.
addEndpoint :: Request ActiveEndpoint -> InstanceState -> STM ()
addEndpoint Request{rqID, itID, rqRequest} InstanceState{issEndpoints} = do
    endpoint <- OpenEndpoint rqRequest <$> STM.newEmptyTMVar
    STM.modifyTVar issEndpoints (Map.insert (rqID, itID) endpoint)

-- | Add a new 'OpenTxOutSpentRequest' to the instance's list of
--   utxo spent requests
addUtxoSpentReq :: Request TxOutRef -> InstanceState -> STM ()
addUtxoSpentReq Request{rqID, itID, rqRequest} InstanceState{issTxOutRefs} = do
    request <- OpenTxOutSpentRequest rqRequest <$> STM.newEmptyTMVar
    STM.modifyTVar issTxOutRefs (Map.insert (rqID, itID) request)

waitForUtxoSpent :: Request TxOutRef -> InstanceState -> STM ChainIndexTx
waitForUtxoSpent Request{rqID, itID} InstanceState{issTxOutRefs} = do
    theMap <- STM.readTVar issTxOutRefs
    case Map.lookup (rqID, itID) theMap of
        Nothing                                   -> empty
        Just OpenTxOutSpentRequest{osrSpendingTx} -> STM.readTMVar osrSpendingTx

-- | Add a new 'OpenTxOutProducedRequest' to the instance's list of
--   utxo produced requests
addUtxoProducedReq :: Request CardanoAddress -> InstanceState -> STM ()
addUtxoProducedReq Request{rqID, itID, rqRequest} InstanceState{issAddressRefs} = do
    request <- OpenTxOutProducedRequest rqRequest <$> STM.newEmptyTMVar
    STM.modifyTVar issAddressRefs (Map.insert (rqID, itID) request)

waitForUtxoProduced :: Request CardanoAddress -> InstanceState -> STM (NonEmpty ChainIndexTx)
waitForUtxoProduced Request{rqID, itID} InstanceState{issAddressRefs} = do
    theMap <- STM.readTVar issAddressRefs
    case Map.lookup (rqID, itID) theMap of
        Nothing                                         -> empty
        Just OpenTxOutProducedRequest{otxProducingTxns} -> STM.readTMVar otxProducingTxns

-- | Write a new value into the contract instance's observable state.
setObservableState :: Value -> InstanceState -> STM ()
setObservableState vl InstanceState{issObservableState} =
    STM.writeTVar issObservableState (Just vl)

-- | The list of all endpoints that can be called on the instance
openEndpoints :: InstanceState -> STM (Map (RequestID, IterationID) OpenEndpoint)
openEndpoints = STM.readTVar . issEndpoints

-- | Call an endpoint with a JSON value.
callEndpoint :: OpenEndpoint -> EndpointValue Value -> STM ()
callEndpoint OpenEndpoint{oepResponse} = STM.putTMVar oepResponse

-- | Call an endpoint on a contract instance. Fail immediately if the endpoint is not active.
callEndpointOnInstance :: InstancesState -> EndpointDescription -> Value -> ContractInstanceId -> IO (STM (Maybe NotificationError))
callEndpointOnInstance s endpointDescription value instanceID =
    let err = pure $ Just $ EndpointNotAvailable instanceID endpointDescription
    in callEndpointOnInstance' err s endpointDescription value instanceID

-- | Call an endpoint on a contract instance. If the endpoint is not active, wait until the
--   TMVar is filled, then fail. (if the endpoint becomes active in the meantime it will be
--   called)
callEndpointOnInstanceTimeout :: STM.TMVar () -> InstancesState -> EndpointDescription -> Value -> ContractInstanceId -> IO (STM (Maybe NotificationError))
callEndpointOnInstanceTimeout tmv s endpointDescription value instanceID =
    let err = do
            _ <- STM.takeTMVar tmv
            pure $ Just $ EndpointNotAvailable instanceID endpointDescription
    in callEndpointOnInstance' err s endpointDescription value instanceID

-- | Call an endpoint on a contract instance. The caller can define what to do if the endpoint
--   is not available.
callEndpointOnInstance' ::
    STM (Maybe NotificationError) -- ^ What to do when the endpoint is not available
    -> InstancesState
    -> EndpointDescription
    -> Value
    -> ContractInstanceId
    -> IO (STM (Maybe NotificationError))
callEndpointOnInstance' notAvailable (InstancesState m) endpointDescription value instanceID = do
    instances <- IORef.readIORef m
    case Map.lookup instanceID instances of
        Nothing -> pure $ pure (Just $ InstanceDoesNotExist instanceID)
        Just is -> pure $ do
            mp <- openEndpoints is
            let match OpenEndpoint{oepName=ActiveEndpoint{aeDescription=d}} = endpointDescription == d
            case filter match $ fmap snd $ Map.toList mp of
                []   -> notAvailable
                [ep] -> callEndpoint ep (EndpointValue value) >> pure Nothing
                _    -> pure $ Just $ MoreThanOneEndpointAvailable instanceID endpointDescription

-- | The list of all partial txs that need to be balanced on the instance.
yieldedExportTxs :: InstanceState -> STM [ExportTx]
yieldedExportTxs = STM.readTVar . issYieldedExportTxs

-- | State of all contract instances that are currently running
newtype InstancesState = InstancesState { getInstancesState :: IORef (Map ContractInstanceId InstanceState) }

-- | Initialise the 'InstancesState' with an empty value
emptyInstancesState :: IO InstancesState
emptyInstancesState = InstancesState <$> IORef.newIORef mempty

-- | The IDs of all contract instances
instanceIDs :: InstancesState -> IO (Set ContractInstanceId)
instanceIDs (InstancesState m) = Map.keysSet <$> IORef.readIORef m

-- | The 'InstanceState' of the contract instance. Retries if the state can't
--   be found in the map.
instanceState :: ContractInstanceId -> InstancesState -> IO (Maybe InstanceState)
instanceState instanceId (InstancesState m) = do
    mp <- IORef.readIORef m
    return (Map.lookup instanceId mp)

-- | Get the observable state of the contract instance. Blocks if the
--   state is not available yet.
observableContractState :: InstanceState -> STM Value
observableContractState InstanceState{issObservableState} = do
    v <- STM.readTVar issObservableState
    maybe empty pure v

-- | Return the final state of the contract when it is finished (possibly an
--   error)
finalResult :: InstanceState -> STM (Maybe Value)
finalResult InstanceState{issStatus} = do
    v <- STM.readTVar issStatus
    case v of
        Done r  -> pure r
        Stopped -> pure Nothing
        _       -> empty

-- | Insert an 'InstanceState' value into the 'InstancesState'
insertInstance :: ContractInstanceId -> InstanceState -> InstancesState -> IO ()
insertInstance instanceID state (InstancesState m) = IORef.modifyIORef' m (Map.insert instanceID state)

-- | Delete an instance from the 'InstancesState'
removeInstance :: ContractInstanceId -> InstancesState -> IO ()
removeInstance instanceID (InstancesState m) = IORef.modifyIORef' m (Map.delete instanceID)

-- | Wait for the status of a transaction to change.
waitForTxStatusChange
  :: TxStatus -> TxId -> BlockchainEnv -> STM TxStatus
waitForTxStatusChange oldStatus tx BlockchainEnv{beTxChanges, beLastSyncedBlockNo} = do
    case beTxChanges of
      Left ix -> do
        blockNumber <- STM.readTVar beLastSyncedBlockNo
        txIdState <- _usTxUtxoData . utxoState <$> STM.readTVar ix
        let txStatus  = transactionStatus blockNumber txIdState tx
        -- Succeed only if we _found_ a status and it was different; if
        -- the status hasn't changed, _or_ there was an error computing
        -- the status, keep retrying.
        case txStatus of
          Right s | s /= oldStatus -> pure s
          _                        -> empty
      -- This branch gets intercepted in `processTxStatusChangeRequestIO` and
      -- handled separateley, so we should never reach this place.
      Right _ ->
          error "waitForTxStatusChange called without the STM index available"

-- | Wait for the status of a transaction output to change.
waitForTxOutStatusChange :: TxOutStatus -> TxOutRef -> BlockchainEnv -> STM TxOutStatus
waitForTxOutStatusChange oldStatus txOutRef BlockchainEnv{beTxChanges, beTxOutChanges, beLastSyncedBlockNo} = do
    case beTxChanges of
      Left txChanges -> do
        txIdState    <- _usTxUtxoData . utxoState <$> STM.readTVar txChanges
        txOutBalance <- _usTxUtxoData . utxoState <$> STM.readTVar beTxOutChanges
        blockNumber  <- STM.readTVar beLastSyncedBlockNo
        let txOutStatus = transactionOutputStatus blockNumber txIdState txOutBalance txOutRef
        -- Succeed only if we _found_ a status and it was different; if
        -- the status hasn't changed, _or_ there was an error computing
        -- the status, keep retrying.
        case txOutStatus of
          Right s | s /= oldStatus -> pure s
          _                        -> empty
      -- This branch gets intercepted in `processTxOutStatusChangeRequestIO` and
      -- handled separateley, so we should never reach this place.
      Right _ ->
          error "waitForTxOutStatusChange called without the STM index available"

-- | The current slot number
currentSlot :: BlockchainEnv -> STM Slot
currentSlot BlockchainEnv{beCurrentSlot} = STM.readTVar beCurrentSlot

lastSyncedBlockSlot :: BlockchainEnv -> STM Slot
lastSyncedBlockSlot BlockchainEnv{beLastSyncedBlockSlot} = STM.readTVar beLastSyncedBlockSlot

-- | The IDs of contract instances with their statuses
instancesWithStatuses :: InstancesState -> IO (STM (Map ContractInstanceId Wallet.ContractActivityStatus))
instancesWithStatuses (InstancesState m) = do
    let parseStatus :: Activity -> Wallet.ContractActivityStatus
        parseStatus = \case
            Active  -> Wallet.Active
            Stopped -> Wallet.Stopped
            Done _  -> Wallet.Done
    let flt :: InstanceState -> STM Wallet.ContractActivityStatus
        flt InstanceState{issStatus} = do
            status <- STM.readTVar issStatus
            return $ parseStatus status
    mp <- IORef.readIORef m
    pure (traverse flt mp)

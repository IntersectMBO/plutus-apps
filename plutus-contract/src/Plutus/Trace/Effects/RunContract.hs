{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-

An effect for starting contract instances and calling endpoints on them.

-}
module Plutus.Trace.Effects.RunContract(
    RunContract(..)
    , StartContract(..)
    , ContractConstraints
    , ContractInstanceTag
    , activateContract
    , activateContractWallet
    , callEndpoint
    , getContractState
    , activeEndpoints
    , observableState
    , walletInstanceTag
    , handleRunContract
    , handleStartContract
    , startContractThread
    ) where

import Control.Lens (preview)
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret, send, type (~>))
import Control.Monad.Freer.Coroutine (Yield (..))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logError, mapLog)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State (State)
import Control.Monad.Freer.TH (makeEffect)

import Data.Aeson qualified as JSON
import Data.Maybe (mapMaybe)
import Data.Profunctor (Profunctor (..))
import Data.Proxy (Proxy (..))
import Data.Row.Internal qualified as V
import GHC.TypeLits qualified
import Plutus.Contract (Contract, ContractInstanceId, HasEndpoint)
import Plutus.Contract.Effects (ActiveEndpoint, PABReq, PABResp (ExposeEndpointResp), _ExposeEndpointReq)
import Plutus.Contract.Resumable (Request (rqRequest), Requests (..))
import Plutus.Contract.Schema (Input, Output)
import Plutus.Contract.Trace.RequestHandler (RequestHandler)
import Plutus.Contract.Types (IsContract (..), ResumableResult (..))
import Plutus.Trace.Effects.ContractInstanceId (ContractInstanceIdEff, nextId)
import Plutus.Trace.Emulator.ContractInstance (contractThread, getThread)
import Plutus.Trace.Emulator.Types (ContractHandle (..), ContractInstanceState (..), ContractInstanceTag,
                                    EmulatedWalletEffects,
                                    EmulatorMessage (ContractInstanceStateRequest, ContractInstanceStateResponse, EndpointCall),
                                    EmulatorRuntimeError (EmulatorJSONDecodingError), EmulatorThreads,
                                    UserThreadMsg (UserThreadErr), walletInstanceTag)
import Plutus.Trace.Scheduler (AgentSystemCall, EmSystemCall, MessageCall (Message), Priority (..), Tag, ThreadId, fork,
                               mkSysCall, sleep)
import Wallet.Emulator.MultiAgent (EmulatorEvent' (..), MultiAgentEffect, handleMultiAgentEffects)
import Wallet.Emulator.Wallet (Wallet (..))
import Wallet.Types (EndpointDescription (..), EndpointValue (..))

type ContractConstraints s =
    ( V.Forall (Output s) V.Unconstrained1
    , V.Forall (Input s) V.Unconstrained1
    , V.AllUniqueLabels (Input s)
    , V.AllUniqueLabels (Output s)
    , V.Forall (Input s) JSON.FromJSON
    , V.Forall (Input s) JSON.ToJSON
    , V.Forall (Output s) JSON.FromJSON
    , V.Forall (Output s) JSON.ToJSON
    )

-- | Start a Plutus contract (client side)
data StartContract r where
    ActivateContract :: (IsContract contract, ContractConstraints s, Show e, JSON.FromJSON e, JSON.ToJSON e, JSON.ToJSON w, Monoid w, JSON.FromJSON w) => Wallet -> contract PABReq PABResp w s e a -> ContractInstanceTag -> StartContract (ContractHandle PABReq PABResp w s e)

makeEffect ''StartContract

-- | Run a Plutus contract (client side)
data RunContract r where
    CallEndpointP :: forall l ep i o w s e. (ContractConstraints s, HasEndpoint l ep s, JSON.ToJSON ep) => Proxy l -> ContractHandle i o w s e -> ep -> RunContract ()
    GetContractState :: forall i o w s e. (ContractConstraints s, JSON.FromJSON i, JSON.FromJSON o, JSON.FromJSON e, JSON.FromJSON w, JSON.ToJSON w) => ContractHandle i o w s e -> RunContract (ContractInstanceState i o w s e ())

makeEffect ''RunContract

-- | Call an endpoint on a contract instance.
callEndpoint ::
    forall l ep i o w s e effs.
    (JSON.ToJSON ep, ContractConstraints s, HasEndpoint l ep s, Member RunContract effs) => ContractHandle i o w s e -> ep -> Eff effs ()
callEndpoint = callEndpointP (Proxy @l)

-- | Like 'activateContract', but using 'walletInstanceTag' for the tag.
activateContractWallet
    :: forall contract w s e effs.
    ( IsContract contract
    , ContractConstraints s
    , Show e
    , JSON.ToJSON e
    , JSON.FromJSON e
    , JSON.ToJSON w
    , JSON.FromJSON w
    , Member StartContract effs
    , Monoid w
    )
    => Wallet
    -> contract PABReq PABResp w s e ()
    -> Eff effs (ContractHandle PABReq PABResp w s e)
activateContractWallet w contract = activateContract w contract (walletInstanceTag w)

-- | Handle the 'RunContract' effect by running each contract instance in an
--   emulator thread.
handleRunContract :: forall effs effs2.
    ( Member (State EmulatorThreads) effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (Error EmulatorRuntimeError) effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorThreads) effs
    , Member (Reader ThreadId) effs
    , Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => RunContract
    ~> Eff effs
handleRunContract = \case
    CallEndpointP p h v -> handleCallEndpoint @_ @_ @_ @_ @_ @_ @_ @effs @effs2 p h v
    GetContractState hdl ->
        interpret (mapLog UserThreadEvent)
            $ handleGetContractState @_ @_ @_ @_ @_ @(LogMsg UserThreadMsg ': effs) @effs2 hdl

-- | Handle the 'StartContract' effect by starting each contract instance in an
--   emulator thread.
handleStartContract :: forall effs effs2.
    ( Member (State EmulatorThreads) effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (Reader (RequestHandler (Reader ContractInstanceId ': EmulatedWalletEffects) PABReq PABResp)) effs2
    , Member MultiAgentEffect effs2
    , Member (LogMsg EmulatorEvent') effs2
    , Member ContractInstanceIdEff effs
    , Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => StartContract
    ~> Eff effs
handleStartContract = \case
    ActivateContract w c t -> handleActivate @_ @_ @_ @effs @effs2 w t (void (toContract c))

handleGetContractState ::
    forall i o w s e effs effs2.
    ( Member (State EmulatorThreads) effs
    , Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    , Member (Reader ThreadId) effs
    , Member (Error EmulatorRuntimeError) effs
    , JSON.FromJSON e
    , JSON.FromJSON w
    , JSON.FromJSON i
    , JSON.FromJSON o
    , Member (LogMsg UserThreadMsg) effs
    )
    => ContractHandle i o w s e
    -> Eff effs (ContractInstanceState i o w s e ())
handleGetContractState ContractHandle{chInstanceId} = do
    ownId <- ask @ThreadId
    threadId <- getThread chInstanceId
    void $ mkSysCall @effs2 @EmulatorMessage Normal (Left $ Message threadId $ ContractInstanceStateRequest ownId)

    let checkResponse = \case
            Just (ContractInstanceStateResponse r) -> do
                case JSON.fromJSON @(ContractInstanceState i o w s e ()) r of
                    JSON.Error e' -> do
                        let msg = EmulatorJSONDecodingError e' r
                        logError $ UserThreadErr msg
                        throwError msg
                    JSON.Success event' -> pure event'
            _ -> sleep @effs2 Sleeping >>= checkResponse
    sleep @effs2 Normal >>= checkResponse

handleActivate :: forall w s e effs effs2.
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs2
    , Member MultiAgentEffect effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (Reader (RequestHandler (Reader ContractInstanceId ': EmulatedWalletEffects) PABReq PABResp)) effs2
    , Member (LogMsg EmulatorEvent') effs2
    , Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    , Show e
    , JSON.ToJSON e
    , JSON.ToJSON w
    , Monoid w
    )
    => Wallet
    -> ContractInstanceTag
    -> Contract PABReq PABResp w s e ()
    -> Eff effs (ContractHandle PABReq PABResp w s e)
handleActivate wllt tag con = do
    i <- nextId
    let handle = ContractHandle{chContract=con, chInstanceId = i, chInstanceTag = tag}
    void $ startContractThread @w @s @e @effs @effs2 wllt handle
    pure handle

runningContractInstanceTag :: Tag
runningContractInstanceTag = "contract instance"

-- | Start a new thread for a contract instance (given by the handle).
--   The thread runs in the context of the wallet.
startContractThread ::
    forall w s e effs effs2.
    ( Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    , Member (State EmulatorThreads) effs2
    , Member MultiAgentEffect effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (Reader (RequestHandler (Reader ContractInstanceId ': EmulatedWalletEffects) PABReq PABResp)) effs2
    , Member (LogMsg EmulatorEvent') effs2
    , ContractConstraints s
    , Show e
    , JSON.ToJSON e
    , JSON.ToJSON w
    , Monoid w
    )
    => Wallet
    -> ContractHandle PABReq PABResp w s e
    -> Eff effs (Maybe EmulatorMessage)
startContractThread wallet handle =
    fork @effs2 @EmulatorMessage runningContractInstanceTag Normal
        (interpret (mapYieldEm @_ @effs2)
            $ handleMultiAgentEffects wallet
            $ interpret (mapLog InstanceEvent)
            $ contractThread handle)

mapYieldEm ::
    forall effs effs2 c.
    (Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs)
    => Yield (AgentSystemCall EmulatorMessage) (Maybe EmulatorMessage) c
    -> Eff effs c
mapYieldEm = mapYield @_ @(EmSystemCall effs2 EmulatorMessage) (fmap Left) id

-- | Handle a @Yield a b@ with a @Yield a' b'@ effect.
mapYield ::
    forall a a' b b' effs c.
    (Member (Yield a' b') effs)
    => (a -> a')
    -> (b' -> b)
    -> Yield a b c
    -> Eff effs c
mapYield f g = \case
    Yield a cont -> send @(Yield a' b') $ Yield (f a) (lmap g cont)

handleCallEndpoint :: forall i o w s l e ep effs effs2.
    ( HasEndpoint l ep s
    , JSON.ToJSON ep
    , Member (State EmulatorThreads) effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => Proxy l
    -> ContractHandle i o w s e
    -> ep
    -> Eff effs ()
handleCallEndpoint p ContractHandle{chInstanceId} ep = do
    let epJson = JSON.toJSON $ ExposeEndpointResp description $ EndpointValue $ JSON.toJSON ep
        description = EndpointDescription $ GHC.TypeLits.symbolVal p
        thr = do
            threadId <- getThread chInstanceId
            ownId <- ask @ThreadId
            void $ mkSysCall @effs2 @EmulatorMessage Normal (Left $ Message threadId $ EndpointCall ownId description epJson)
    void $ fork @effs2 @EmulatorMessage callEndpointTag Normal thr

-- | Get the active endpoints of a contract instance.
activeEndpoints :: forall w s e effs.
    ( Member RunContract effs
    , ContractConstraints s
    , JSON.FromJSON e
    , JSON.FromJSON w
    , JSON.ToJSON w
    )
    => ContractHandle PABReq PABResp w s e
    -> Eff effs [ActiveEndpoint]
activeEndpoints hdl = do
    ContractInstanceState{instContractState=ResumableResult{_requests=Requests rq}} <- getContractState hdl
    pure $ mapMaybe (preview _ExposeEndpointReq . rqRequest) rq

-- | Get the observable state @w@ of a contract instance.
observableState :: forall i o w s e effs.
    ( Member RunContract effs
    , ContractConstraints s
    , JSON.FromJSON e
    , JSON.FromJSON w
    , JSON.ToJSON w
    , JSON.FromJSON i
    , JSON.FromJSON o
    )
    => ContractHandle i o w s e -> Eff effs w
observableState hdl = do
    ContractInstanceState{instContractState=ResumableResult{_observableState}} <- getContractState hdl
    pure _observableState

callEndpointTag :: Tag
callEndpointTag = "call endpoint"

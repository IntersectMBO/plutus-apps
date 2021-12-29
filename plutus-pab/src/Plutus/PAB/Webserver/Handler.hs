{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.PAB.Webserver.Handler
    ( apiHandler
    , swagger
    , walletProxy
    , walletProxyClientEnv
    -- * Reports
    , getFullReport
    ) where

import Cardano.Wallet.Mock.Client qualified as Wallet.Client
import Cardano.Wallet.Mock.Types (WalletInfo (WalletInfo, wiPaymentPubKeyHash, wiWallet))
import Control.Lens (preview)
import Control.Monad (join)
import Control.Monad.Freer (sendM)
import Control.Monad.Freer.Error (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as JSON
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OpenApi.Schema (ToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Ledger (Value)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Tx (Tx)
import Plutus.Contract.Effects (PABReq, _ExposeEndpointReq)
import Plutus.Contract.Wallet (ExportTx)
import Plutus.PAB.Core (PABAction)
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (hooks), fromResp)
import Plutus.PAB.Types (PABError (ContractInstanceNotFound, EndpointCallError, WalletClientError))
import Plutus.PAB.Webserver.API (API)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID, caWallet),
                                   ContractInstanceClientState (ContractInstanceClientState, cicContract, cicCurrentState, cicDefinition, cicStatus, cicWallet, cicYieldedExportTxs),
                                   ContractReport (ContractReport, crActiveContractStates),
                                   FullReport (FullReport, chainReport, contractReport), emptyChainReport)
import Servant (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Client (ClientEnv, ClientM, runClientM)
import Servant.OpenApi (toOpenApi)
import Servant.Server qualified as Servant
import Servant.Swagger.UI (SwaggerSchemaUI', swaggerSchemaUIServer)
import Wallet.Effects qualified
import Wallet.Emulator.Error (WalletAPIError)
import Wallet.Emulator.Wallet (Wallet (Wallet), WalletId, knownWallet)
import Wallet.Types (ContractActivityStatus, ContractInstanceId, parseContractActivityStatus)

healthcheck :: forall t env. PABAction t env ()
healthcheck = pure ()

getContractReport :: forall t env. Contract.PABContract t => PABAction t env (ContractReport (Contract.ContractDef t))
getContractReport = do
    activeContractIDs <- fmap fst . Map.toList <$> Contract.getActiveContracts @t
    crActiveContractStates <- traverse (\i -> Contract.getState @t i >>= \s -> pure (i, fromResp $ Contract.serialisableState (Proxy @t) s)) activeContractIDs
    pure ContractReport {crActiveContractStates}

getFullReport :: forall t env. Contract.PABContract t => PABAction t env (FullReport (Contract.ContractDef t))
getFullReport = do
    contractReport <- getContractReport @t
    pure FullReport {contractReport, chainReport = emptyChainReport}

-- | Handler for the API
apiHandler ::
       forall t env.
       Contract.PABContract t =>
       PABAction t env ()
       :<|> PABAction t env (FullReport (Contract.ContractDef t))
       :<|> (ContractActivationArgs (Contract.ContractDef t) -> PABAction t env ContractInstanceId)
              :<|> (ContractInstanceId -> PABAction t env (ContractInstanceClientState (Contract.ContractDef t))
                                          :<|> (String -> JSON.Value -> PABAction t env ())
                                          :<|> PABAction t env ()
                                          )
              :<|> (WalletId -> Maybe Text -> PABAction t env [ContractInstanceClientState (Contract.ContractDef t)])
              :<|> (Maybe Text -> PABAction t env [ContractInstanceClientState (Contract.ContractDef t)])

apiHandler =
        healthcheck
        :<|> getFullReport
        :<|> activateContract
              :<|> (\cid -> contractInstanceState cid :<|> (\y z -> callEndpoint cid y z) :<|> shutdown cid)
              :<|> instancesForWallets
              :<|> allInstanceStates

swagger :: forall t api dir. (Servant.Server api ~ Servant.Handler JSON.Value, ToSchema (Contract.ContractDef t)) => Servant.Server (SwaggerSchemaUI' dir api)
swagger = swaggerSchemaUIServer $ toOpenApi (Proxy @(API (Contract.ContractDef t) Integer))

fromInternalState
    :: t
    -> ContractInstanceId
    -> ContractActivityStatus
    -> Wallet
    -> [ExportTx]
    -> PartiallyDecodedResponse PABReq
    -> ContractInstanceClientState t
fromInternalState t i s wallet yieldedExportTxs resp =
    ContractInstanceClientState
        { cicContract = i
        , cicCurrentState =
            let hks' = mapMaybe (traverse (preview _ExposeEndpointReq)) (hooks resp)
            in resp { hooks = hks' }
        , cicWallet = wallet
        , cicDefinition = t
        , cicStatus = s
        , cicYieldedExportTxs = yieldedExportTxs
        }

-- HANDLERS

activateContract :: forall t env. Contract.PABContract t => ContractActivationArgs (Contract.ContractDef t) -> PABAction t env ContractInstanceId
activateContract ContractActivationArgs{caID, caWallet} = do
    Core.activateContract (fromMaybe (knownWallet 1) caWallet) caID

contractInstanceState
    :: forall t env. Contract.PABContract t
    => ContractInstanceId
    -> PABAction t env (ContractInstanceClientState (Contract.ContractDef t))
contractInstanceState i = do
    definition <- Contract.getDefinition @t i
    instWithStatuses <- Core.instancesWithStatuses
    case (definition, Map.lookup i instWithStatuses) of
        (Just ContractActivationArgs{caWallet, caID}, Just s) -> do
            let wallet = fromMaybe (knownWallet 1) caWallet
            yieldedExportedTxs <- Core.yieldedExportTxs i
            fmap ( fromInternalState caID i s wallet yieldedExportedTxs
                 . fromResp
                 . Contract.serialisableState (Proxy @t)
                 ) $ Contract.getState @t i
        _ -> throwError @PABError (ContractInstanceNotFound i)

callEndpoint :: forall t env. ContractInstanceId -> String -> JSON.Value -> PABAction t env ()
callEndpoint a b v = Core.callEndpointOnInstance a b v >>= traverse_ (throwError @PABError . EndpointCallError)

instancesForWallets :: forall t env. Contract.PABContract t => WalletId -> Maybe Text -> PABAction t env [ContractInstanceClientState (Contract.ContractDef t)]
instancesForWallets wallet mStatus = filter ((==) (Wallet wallet) . cicWallet) <$> allInstanceStates mStatus

allInstanceStates :: forall t env. Contract.PABContract t => Maybe Text -> PABAction t env [ContractInstanceClientState (Contract.ContractDef t)]
allInstanceStates mStatus = do
    instWithStatuses <- Core.instancesWithStatuses
    let mActivityStatus = join $ parseContractActivityStatus <$> mStatus
        isInstanceStatusMatch s = maybe True ((==) s) mActivityStatus
        getStatus (i, args) = (i, args,) <$> Map.lookup i instWithStatuses
        get (i, ContractActivationArgs{caWallet, caID}, s) =
            let wallet = fromMaybe (knownWallet 1) caWallet
             in do
                yieldedExportedTxs <- Core.yieldedExportTxs i
                fmap ( fromInternalState caID i s wallet yieldedExportedTxs
                     . fromResp
                     . Contract.serialisableState (Proxy @t)
                     ) $ Contract.getState @t i
    mp <- Contract.getContracts @t mActivityStatus
    filter (isInstanceStatusMatch . cicStatus)
        <$> traverse get (mapMaybe getStatus $ Map.toList mp)

shutdown :: forall t env. ContractInstanceId -> PABAction t env ()
shutdown = Core.stopInstance

-- | Proxy for the wallet API
walletProxyClientEnv ::
    forall t env.
    ClientEnv ->
    (PABAction t env WalletInfo -- Create new wallet
    :<|> (WalletId -> Tx -> PABAction t env NoContent) -- Submit txn
    :<|> (WalletId -> PABAction t env WalletInfo)
    :<|> (WalletId -> UnbalancedTx -> PABAction t env (Either WalletAPIError Tx))
    :<|> (WalletId -> PABAction t env Value)
    :<|> (WalletId -> Tx -> PABAction t env Tx))
walletProxyClientEnv clientEnv =
    let createWallet = runWalletClientM clientEnv Wallet.Client.createWallet
    in walletProxy createWallet

-- | Run a 'ClientM' action against a remote host using the given 'ClientEnv'.
runWalletClientM :: forall t env a. ClientEnv -> ClientM a -> PABAction t env a
runWalletClientM clientEnv action = do
    x <- sendM $ liftIO $ runClientM action clientEnv
    case x of
        Left err     -> throwError @PABError (WalletClientError err)
        Right result -> pure result

-- | Proxy for the wallet API
walletProxy ::
    forall t env.
    PABAction t env WalletInfo -> -- default action for creating a new wallet
    (PABAction t env WalletInfo -- Create new wallet
    :<|> (WalletId -> Tx -> PABAction t env NoContent) -- Submit txn
    :<|> (WalletId -> PABAction t env WalletInfo)
    :<|> (WalletId -> UnbalancedTx -> PABAction t env (Either WalletAPIError Tx))
    :<|> (WalletId -> PABAction t env Value)
    :<|> (WalletId -> Tx -> PABAction t env Tx))
walletProxy createNewWallet =
    createNewWallet
    :<|> (\w tx -> fmap (const NoContent) (Core.handleAgentThread (Wallet w) Nothing $ Wallet.Effects.submitTxn $ Right tx))
    :<|> (\w -> (\pkh -> WalletInfo{wiWallet=Wallet w, wiPaymentPubKeyHash = pkh })
            <$> Core.handleAgentThread (Wallet w) Nothing Wallet.Effects.ownPaymentPubKeyHash)
    :<|> (\w -> fmap (fmap (fromRight (error "Plutus.PAB.Webserver.Handler: Expecting a mock tx, not an Alonzo tx when submitting it.")))
              . Core.handleAgentThread (Wallet w) Nothing . Wallet.Effects.balanceTx)
    :<|> (\w -> Core.handleAgentThread (Wallet w) Nothing Wallet.Effects.totalFunds)
    :<|> (\w tx -> fmap (fromRight (error "Plutus.PAB.Webserver.Handler: Expecting a mock tx, not an Alonzo tx when adding a signature."))
                 $ Core.handleAgentThread (Wallet w) Nothing
                 $ Wallet.Effects.walletAddSignature
                 $ Right tx)

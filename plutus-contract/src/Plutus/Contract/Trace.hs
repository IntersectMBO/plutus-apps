{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | A trace is a sequence of actions by simulated wallets that can be run
--   on the mockchain. This module contains the functions needed to build
--   traces.
module Plutus.Contract.Trace
    ( TraceError(..)
    , EndpointError(..)
    , AsTraceError(..)
    , toNotifyError
    -- * Handle contract requests
    , handleAdjustUnbalancedTx
    , handleSlotNotifications
    , handleTimeNotifications
    , handleOwnPaymentPubKeyHashQueries
    , handleCurrentSlotQueries
    , handleCurrentTimeQueries
    , handleTimeToSlotConversions
    , handleUnbalancedTransactions
    , handlePendingTransactions
    , handleChainIndexQueries
    , handleOwnInstanceIdQueries
    , handleYieldedUnbalancedTx
    -- * Initial distributions of emulated chains
    , InitialDistribution
    , defaultDist
    , defaultDistFor
    -- * Wallets
    , EM.Wallet(..)
    , EM.mockWalletPaymentPubKey
    , EM.mockWalletPaymentPubKeyHash
    , EM.knownWallets
    , EM.knownWallet
    ) where

import Control.Lens (makeClassyPrisms, preview)
import Control.Monad.Freer (Member)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, LogObserve)
import Control.Monad.Freer.Reader (Reader)
import Data.Aeson.Types qualified as JSON
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Prettyprinter (Pretty, pretty, (<+>))

import Data.Text (Text)

import Plutus.Contract.Effects (PABReq, PABResp)
import Plutus.Contract.Effects qualified as E
import Plutus.Contract.Trace.RequestHandler (RequestHandler, RequestHandlerLogMsg, generalise)
import Plutus.Contract.Trace.RequestHandler qualified as RequestHandler

import Ledger.Ada qualified as Ada
import Ledger.Value (Value)

import Plutus.ChainIndex (ChainIndexQueryEffect)
import Wallet.Effects (NodeClientEffect, WalletEffect)
import Wallet.Emulator (Wallet)
import Wallet.Emulator qualified as EM
import Wallet.Types (ContractInstanceId, EndpointDescription, NotificationError (EndpointNotAvailable))

data EndpointError =
    EndpointNotActive (Maybe Wallet) EndpointDescription
    deriving stock (Eq, Show, Generic)
    deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Pretty EndpointError where
    pretty = \case
        EndpointNotActive w e ->
            "Endpoint not active:" <+> pretty w <+> pretty e

toNotifyError :: ContractInstanceId -> EndpointError -> NotificationError
toNotifyError i = \case
    EndpointNotActive _ e       -> EndpointNotAvailable i e

-- | Error produced while running a trace. Either a contract-specific
--   error (of type 'e'), or an 'EM.AssertionError' from the emulator.
data TraceError e =
    TraceAssertionError EM.AssertionError
    | TContractError e
    | HookError EndpointError
    deriving (Eq, Show)

type InitialDistribution = Map Wallet Value

handleSlotNotifications ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleSlotNotifications =
    generalise (preview E._AwaitSlotReq) E.AwaitSlotResp RequestHandler.handleSlotNotifications

handleTimeNotifications ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleTimeNotifications =
    generalise (preview E._AwaitTimeReq) E.AwaitTimeResp RequestHandler.handleTimeNotifications

handleCurrentSlotQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleCurrentSlotQueries =
    generalise (preview E._CurrentSlotReq) E.CurrentSlotResp RequestHandler.handleCurrentSlot

handleCurrentTimeQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleCurrentTimeQueries =
    generalise (preview E._CurrentTimeReq) E.CurrentTimeResp RequestHandler.handleCurrentTime

handleTimeToSlotConversions ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleTimeToSlotConversions =
    generalise (preview E._PosixTimeRangeToContainedSlotRangeReq) (E.PosixTimeRangeToContainedSlotRangeResp . Right) RequestHandler.handleTimeToSlotConversions

handleUnbalancedTransactions ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleUnbalancedTransactions =
    generalise
        (preview E._BalanceTxReq)
        (E.BalanceTxResp . either E.BalanceTxFailed E.BalanceTxSuccess)
        RequestHandler.handleUnbalancedTransactions

-- | Submit the wallet's pending transactions to the blockchain.
handlePendingTransactions ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs PABReq PABResp
handlePendingTransactions =
    generalise
        (preview E._WriteBalancedTxReq)
        (E.WriteBalancedTxResp . either E.WriteBalancedTxFailed E.WriteBalancedTxSuccess)
        RequestHandler.handlePendingTransactions

handleChainIndexQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member ChainIndexQueryEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleChainIndexQueries =
    generalise (preview E._ChainIndexQueryReq)
               E.ChainIndexQueryResp
               RequestHandler.handleChainIndexQueries

handleOwnPaymentPubKeyHashQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleOwnPaymentPubKeyHashQueries =
    generalise (preview E._OwnPaymentPublicKeyHashReq) E.OwnPaymentPublicKeyHashResp RequestHandler.handleOwnPaymentPubKeyHash

handleOwnInstanceIdQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (Reader ContractInstanceId) effs
    )
    => RequestHandler effs PABReq PABResp
handleOwnInstanceIdQueries =
    generalise (preview E._OwnContractInstanceIdReq) E.OwnContractInstanceIdResp RequestHandler.handleOwnInstanceIdQueries

handleYieldedUnbalancedTx ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleYieldedUnbalancedTx =
    generalise
        (preview E._YieldUnbalancedTxReq)
        E.YieldUnbalancedTxResp
        RequestHandler.handleYieldedUnbalancedTx

handleAdjustUnbalancedTx ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleAdjustUnbalancedTx =
    generalise
        (preview E._AdjustUnbalancedTxReq)
        E.AdjustUnbalancedTxResp
        RequestHandler.handleAdjustUnbalancedTx

defaultDist :: InitialDistribution
defaultDist = defaultDistFor EM.knownWallets

defaultDistFor :: [EM.Wallet] -> InitialDistribution
defaultDistFor wallets = Map.fromList $ zip wallets (repeat (Ada.lovelaceValueOf 100_000_000))

makeClassyPrisms ''TraceError

instance EM.AsAssertionError (TraceError e) where
    _AssertionError = _TraceAssertionError

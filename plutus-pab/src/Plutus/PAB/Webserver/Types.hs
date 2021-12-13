{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutus.PAB.Webserver.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Map (Map)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Ledger (PubKeyHash, Tx, TxId)
import Ledger.Index (UtxoIndex)
import Ledger.Slot (Slot)
import Playground.Types (FunctionSchema)
import Plutus.Contract.Effects (ActiveEndpoint, PABReq)
import Plutus.Contract.Wallet (ExportTx)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Prettyprinter (Pretty, pretty, (<+>))
import Schema (FormSchema)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Rollup.Types (AnnotatedTx)
import Wallet.Types (ContractActivityStatus, ContractInstanceId)

data ContractReport t =
    ContractReport
        { crAvailableContracts   :: [ContractSignatureResponse t]
        , crActiveContractStates :: [(ContractInstanceId, PartiallyDecodedResponse PABReq)]
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

data ChainReport =
    ChainReport
        { transactionMap      :: Map TxId Tx
        , utxoIndex           :: UtxoIndex
        , annotatedBlockchain :: [[AnnotatedTx]]
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

emptyChainReport :: ChainReport
emptyChainReport = ChainReport mempty mempty mempty

data FullReport t =
    FullReport
        { contractReport :: ContractReport t
        , chainReport    :: ChainReport
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

data ContractSignatureResponse t =
    ContractSignatureResponse
        { csrDefinition :: t
        , csrSchemas    :: [FunctionSchema FormSchema]
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

deriving instance OpenApi.ToSchema t => OpenApi.ToSchema (ContractSignatureResponse t)

-- | Data needed to start a new instance of a contract.
data ContractActivationArgs t =
    ContractActivationArgs
        { caID     :: t -- ^ ID of the contract
        , caWallet :: Maybe Wallet -- ^ Wallet that should be used for this instance, `knownWallet 1` is used in the Nothing case.
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

deriving instance OpenApi.ToSchema t => OpenApi.ToSchema (ContractActivationArgs t)

instance Pretty t => Pretty (ContractActivationArgs t) where
    pretty ContractActivationArgs{caID, caWallet} =
        pretty caID <+> "on" <+> pretty caWallet

-- | Current state of a contract instance
--   (to be sent to external clients)
data ContractInstanceClientState t =
    ContractInstanceClientState
        { cicContract         :: ContractInstanceId
        , cicCurrentState     :: PartiallyDecodedResponse ActiveEndpoint
        , cicWallet           :: Wallet
        , cicDefinition       :: t
        , cicStatus           :: ContractActivityStatus
        , cicYieldedExportTxs :: [ExportTx]
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

deriving instance OpenApi.ToSchema t => OpenApi.ToSchema (ContractInstanceClientState t)

-- | Status updates for contract instances streamed to client
data InstanceStatusToClient
    = NewObservableState JSON.Value -- ^ The observable state of the contract has changed.
    | NewActiveEndpoints [ActiveEndpoint] -- ^ The set of active endpoints has changed.
    | NewYieldedExportTxs [ExportTx] -- ^ Partial txs that need to be balanced, signed and submitted by an external client.
    | ContractFinished (Maybe JSON.Value) -- ^ Contract instance is done with an optional error message.
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | Data sent to the client through the combined websocket API
data CombinedWSStreamToClient
    = InstanceUpdate ContractInstanceId InstanceStatusToClient
    | SlotChange Slot -- ^ New slot number
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | Instructions sent to the server through the combined websocket API
data CombinedWSStreamToServer
    = Subscribe (Either ContractInstanceId PubKeyHash)
    | Unsubscribe (Either ContractInstanceId PubKeyHash)
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

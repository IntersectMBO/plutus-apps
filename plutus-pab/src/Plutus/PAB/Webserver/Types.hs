{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.PAB.Webserver.Types where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Wallet.LocalClient.ExportTx (ExportTx, ExportTxInput, ExportTxRedeemer)
import Cardano.Wallet.Primitive.Types qualified as Cardano.Wallet
import Control.Lens ((&), (.~), (?~))
import Crypto.Hash qualified as Crypto
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Data (Proxy (Proxy))
import Data.HashMap.Strict.InsOrd qualified as InsOrdMap
import Data.Map (Map)
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiObject), byteSchema, declareSchemaRef, properties,
                     required, type_)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Ledger (Certificate, Datum, POSIXTime (POSIXTime), PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash, TxId,
               TxOut, Value, Withdrawal)
import Ledger.Crypto (PubKey (PubKey), Signature (Signature))
import Ledger.Index (UtxoIndex)
import Ledger.Slot (Slot)
import Ledger.Tx (CardanoTx, TxInput, TxInputType)
import Ledger.Tx.CardanoAPI (CardanoBuildTx)
import Ledger.Tx.Constraints.OffChain (UnbalancedTx)
import Plutus.Contract.Effects (ActiveEndpoint, ChainIndexQuery, PABReq)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.V1.Ledger.Api (DCert, LedgerBytes (LedgerBytes))
import Prettyprinter (Pretty, pretty, (<+>))
import Wallet.Emulator.Wallet (Wallet, WalletId (WalletId))
import Wallet.Rollup.Types (AnnotatedTx, BeneficialOwner, DereferencedInput, SequenceId, TxKey)
import Wallet.Types (ContractActivityStatus, ContractInstanceId, EndpointDescription)

instance OpenApi.ToSchema TxOut where
    declareNamedSchema _ = do
      addressSchema <- declareSchemaRef (Proxy :: Proxy (C.AddressInEra C.BabbageEra))
      valueSchema <- declareSchemaRef (Proxy :: Proxy Value)
      bsSchema <- declareSchemaRef (Proxy :: Proxy Datum)
      pure $ NamedSchema (Just "TxOut") $ mempty
        & type_ ?~ OpenApiObject
        & properties .~
          InsOrdMap.fromList [ ("address", addressSchema)
          , ("value", valueSchema)
          , ("datum", bsSchema)
          , ("referenceScript", bsSchema)
          ]
        & required .~ ["address","value"]

deriving newtype instance OpenApi.ToSchema Cardano.Wallet.WalletId
deriving newtype instance OpenApi.ToSchema WalletId
deriving newtype instance OpenApi.ToSchema POSIXTime
deriving newtype instance OpenApi.ToSchema PaymentPubKeyHash
deriving anyclass instance OpenApi.ToSchema DCert
deriving anyclass instance OpenApi.ToSchema EndpointDescription
deriving anyclass instance OpenApi.ToSchema ActiveEndpoint
deriving anyclass instance OpenApi.ToSchema Wallet
deriving anyclass instance OpenApi.ToSchema ContractActivityStatus
deriving anyclass instance OpenApi.ToSchema ExportTx

instance OpenApi.ToSchema (C.Hash C.ScriptData) where
    declareNamedSchema _ = pure $ NamedSchema (Just "HashScriptData") byteSchema
instance OpenApi.ToSchema C.AssetName where
    declareNamedSchema _ = pure $ NamedSchema (Just "AssetName") byteSchema
instance OpenApi.ToSchema C.ScriptHash where
    declareNamedSchema _ = pure $ NamedSchema (Just "ScriptHash") mempty
instance OpenApi.ToSchema (Crypto.Digest Crypto.Blake2b_160) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Digest") mempty
instance OpenApi.ToSchema (Hash.Hash Hash.Blake2b_256 Hashes.EraIndependentTxBody) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Hash") mempty

deriving anyclass instance OpenApi.ToSchema C.Quantity
deriving anyclass instance OpenApi.ToSchema C.PolicyId
deriving anyclass instance OpenApi.ToSchema C.Lovelace
deriving anyclass instance OpenApi.ToSchema C.TxIx
deriving anyclass instance OpenApi.ToSchema C.TxId
deriving anyclass instance OpenApi.ToSchema ExportTxInput
deriving anyclass instance OpenApi.ToSchema ExportTxRedeemer
deriving anyclass instance OpenApi.ToSchema ChainIndexQuery
deriving anyclass instance OpenApi.ToSchema UnbalancedTx
deriving anyclass instance OpenApi.ToSchema SequenceId
deriving anyclass instance OpenApi.ToSchema PABReq
deriving anyclass instance OpenApi.ToSchema ContractInstanceId

instance OpenApi.ToSchema CardanoBuildTx where
  -- TODO: implement the schema
  declareNamedSchema _ = return $ NamedSchema (Just "CardanoBuildTx") mempty

data ContractReport t =
    ContractReport
        { crAvailableContracts   :: [ContractSignatureResponse t]
        , crActiveContractStates :: [(ContractInstanceId, PartiallyDecodedResponse PABReq)]
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

deriving newtype instance OpenApi.ToSchema LedgerBytes
deriving newtype instance OpenApi.ToSchema Signature
deriving newtype instance OpenApi.ToSchema PubKey
deriving instance OpenApi.ToSchema TxInputType
deriving instance OpenApi.ToSchema TxInput
deriving instance OpenApi.ToSchema Withdrawal
deriving instance OpenApi.ToSchema Certificate
deriving anyclass instance OpenApi.ToSchema UtxoIndex
deriving anyclass instance OpenApi.ToSchema DereferencedInput
deriving anyclass instance OpenApi.ToSchema BeneficialOwner
deriving anyclass instance OpenApi.ToSchema TxKey
deriving anyclass instance OpenApi.ToSchema AnnotatedTx

data ChainReport =
    ChainReport
        { transactionMap      :: Map TxId CardanoTx
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

newtype ContractSignatureResponse t =
    ContractSignatureResponse
        { csrDefinition :: t
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance OpenApi.ToSchema t => OpenApi.ToSchema (ContractSignatureResponse t)

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

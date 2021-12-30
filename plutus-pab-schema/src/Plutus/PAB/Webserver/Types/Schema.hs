{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutus.PAB.Webserver.Types.Schema where

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

data ContractSchemaResponse t =
    ContractSchemaResponse
        { csrDefinition :: t
        , csrSchemas    :: [FunctionSchema FormSchema]
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

deriving instance OpenApi.ToSchema t => OpenApi.ToSchema (ContractSchemaResponse t)


class HasSchema a where
    getSchema :: a -> [FunctionSchema FormSchema] -- List of schemas for contract type `a`

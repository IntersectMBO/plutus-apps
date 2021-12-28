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
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)

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

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

module Plutus.PAB.Webserver.Handler.Schema where

import Control.Monad.Freer.Error (throwError)
import Data.Aeson (ToJSON)
import Data.Proxy (Proxy (Proxy))
import Plutus.PAB.Core (PABAction, PABRunner)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Types (PABError (ContractInstanceNotFound))
import Plutus.PAB.Webserver.API.Schema
import Plutus.PAB.Webserver.Server (asHandler)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID))
import Plutus.PAB.Webserver.Types.Schema
import Servant qualified
import Wallet.Types (ContractInstanceId)

contractSchema :: forall t env. HasSchema (Contract.ContractDef t) => ContractInstanceId -> PABAction t env (ContractSchemaResponse (Contract.ContractDef t))
contractSchema contractId = do
    def <- Contract.getDefinition @t contractId
    case def of
        Just ContractActivationArgs{caID} -> pure $ ContractSchemaResponse caID (getSchema caID)
        Nothing                           -> throwError (ContractInstanceNotFound contractId)

-- | Handler for the API
schemaApiHandler :: forall t env. HasSchema (Contract.ContractDef t) => ContractInstanceId -> PABAction t env (ContractSchemaResponse (Contract.ContractDef t))
schemaApiHandler cid = contractSchema cid

schemaServer :: forall t env.
    ( ToJSON (Contract.ContractDef t)
    , HasSchema (Contract.ContractDef t)
    ) => PABRunner t env -> Servant.ServerT (SchemaAPI (Contract.ContractDef t)) Servant.Handler
schemaServer pabRunner = Servant.hoistServer
    (Proxy @(SchemaAPI (Contract.ContractDef t)))
    (asHandler pabRunner)
    schemaApiHandler

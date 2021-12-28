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
import Plutus.PAB.Core (PABAction)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Types (PABError (ContractInstanceNotFound))
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID))
import Plutus.PAB.Webserver.Types.Schema
import Wallet.Types (ContractInstanceId)

contractSchema :: forall t env. HasSchema (Contract.ContractDef t) => ContractInstanceId -> PABAction t env (ContractSchemaResponse (Contract.ContractDef t))
contractSchema contractId = do
    def <- Contract.getDefinition @t contractId
    case def of
        Just ContractActivationArgs{caID} -> pure $ ContractSchemaResponse caID (getSchema caID)
        Nothing                           -> throwError (ContractInstanceNotFound contractId)

-- | Handler for the API
apiHandler :: forall t env. HasSchema (Contract.ContractDef t) => ContractInstanceId -> PABAction t env (ContractSchemaResponse (Contract.ContractDef t))
apiHandler cid = contractSchema cid

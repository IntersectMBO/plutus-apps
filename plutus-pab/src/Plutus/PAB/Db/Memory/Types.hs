{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.PAB.Db.Memory.Types(
    InMemContractInstanceState(..)
  , InMemInstances(..)
  , contractState
  ) where

import Control.Concurrent.STM (TVar)
import Control.Lens (makeLensesFor)
import Data.Map (Map)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Webserver.Types (ContractActivationArgs)
import Wallet.Types (ContractInstanceId)
import Wallet.Types qualified as Contract

-- | The current state of a contract instance
data InMemContractInstanceState t =
    InMemContractInstanceState
        { _contractDef            :: ContractActivationArgs (Contract.ContractDef t)
        , _contractState          :: Contract.State t
        , _contractActivityStatus :: Contract.ContractActivityStatus
        }

makeLensesFor [("_contractState", "contractState")] ''InMemContractInstanceState

newtype InMemInstances t = InMemInstances { unInMemInstances :: TVar (Map ContractInstanceId (InMemContractInstanceState t)) }

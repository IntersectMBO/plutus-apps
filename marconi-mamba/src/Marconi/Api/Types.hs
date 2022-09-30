{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- This module provides support for writing handlers for JSON-RPC endpoints
module Marconi.Api.Types where

import Cardano.Api qualified
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens
import Data.Map.Strict qualified as Map
import Ledger (TxOutRef)

type RpcPortNumber = Int

type AddressTxOutRefMap = (Map.Map(Cardano.Api.Address Cardano.Api.ShelleyAddr) TxOutRef)

type AddressTxOutRefCache = TVar AddressTxOutRefMap

data HttpEnv = HttpEnv {
    _portNumber             :: RpcPortNumber
    , _addressTxOutRefCache :: AddressTxOutRefCache
    }
makeClassy ''HttpEnv

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
import Control.Lens (makeClassy)
import Ledger (TxOutRef)
import Marconi.IndexerCache (ApiCache)
import Network.Wai.Handler.Warp (Settings)


-- | Typre represents mamba specific marconi indexer cache
type MambaCache = ApiCache (Cardano.Api.Address Cardano.Api.ShelleyAddr ) TxOutRef

type RpcPortNumber = Int

data JsonRpcEnv = JsonRpcEnv {
    _httpSettings           :: Settings -- ^ HTTP server setting
    , _addressTxOutRefCache :: MambaCache -- ^ In memory cache used between marconi indexer and the JSON-RPC server
    }
makeClassy ''JsonRpcEnv

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
import Marconi.IndexersHotStore (IndexerHotStore)
import Network.Wai.Handler.Warp (Settings)


-- | Typre represents mamba specific marconi indexer cache
type RpcPortNumber = Int

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv {
    _httpSettings           :: Settings -- ^ HTTP server setting
    , _addressTxOutRefCache :: IndexerHotStore -- ^ In memory cache used between marconi indexer and the JSON-RPC server
    }
makeClassy ''JsonRpcEnv

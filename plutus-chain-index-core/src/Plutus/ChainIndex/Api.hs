{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.ChainIndex.Api
  ( API
  , FromHashAPI
  , FullAPI
  , IsUtxoResponse(..)
  , SwaggerAPI
  , UtxoAtAddressRequest(..)
  , UtxosResponse(..)
  , UtxoWithCurrencyRequest(..)
  , swagger
  , TxoAtAddressRequest(..)
  , TxosResponse(..)
  , QueryAtAddressRequest (..)
  , QueryResponse(..)
  , collectQueryResponse
  ) where

import Control.Monad.Freer.Extras.Pagination (Page, PageQuery)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Default (def)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Ledger (AssetClass, TxId)
import Ledger.Credential (Credential)
import Ledger.Tx (DecoratedTxOut, TxOutRef, Versioned)
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.ChainIndex.Types (Diagnostics, Tip)
import Plutus.V1.Ledger.Api (Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash, StakeValidator,
                             StakeValidatorHash, Validator, ValidatorHash)
import Servant qualified
import Servant.API (Description, Get, JSON, NoContent, Post, Put, ReqBody, (:<|>), (:>))
import Servant.OpenApi (toOpenApi)
import Servant.Swagger.UI (SwaggerSchemaUI, SwaggerSchemaUI', swaggerSchemaUIServer)

-- | When requesting UTxOs of a given address, you need to provide the address,
-- and optionnally the number of elements per page and the last item of the last
-- requested page.
--
-- Here's an example for requesting the first page:
--
-- {
--   "credential": {
--     "tag": "PubKeyCredential",
--     "contents": {
--       "getPubKeyHash": "88ff402b0522f27649ac742238c697c579beeb344eb723099d1f16ce"
--     }
--   }
-- }
--
-- or
--
-- {
--   "pageQuery": {
--     "pageQuerySize": {
--       "getPageSize": 10
--     }
--   },
--   "credential": {
--     "tag": "PubKeyCredential",
--     "contents": {
--       "getPubKeyHash": "88ff402b0522f27649ac742238c697c579beeb344eb723099d1f16ce"
--     }
--   }
-- }
--
-- Here's an example for requesting the next page:
--
-- {
--   "pageQuery": {
--     "pageQuerySize": {
--       "getPageSize": 10
--     },
--     "pageQueryLastItem": {
--       "txOutRefId": {
--         "getTxId": "009b8c674b878cc68bd1d40562c5f14cdbb21be9266f605cfb68ed978e1a965b"
--       },
--       "txOutRefIdx": 0
--     }
--   },
--   "credential": {
--     "tag": "PubKeyCredential",
--     "contents": {
--       "getPubKeyHash": "88ff402b0522f27649ac742238c697c579beeb344eb723099d1f16ce"
--     }
--   }
-- }
data UtxoAtAddressRequest = UtxoAtAddressRequest
    { pageQuery  :: Maybe (PageQuery TxOutRef)
    , credential :: Credential
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

-- | See the comment on 'UtxoAtAddressRequest'.
--
-- The difference is using @currency@ field instead of @credential@.
-- {
--   "pageQuery": {
--     ...
--   },
--   "currency": {
--     "unAssetClass": [
--       {
--         "unCurrencySymbol": ""
--       },
--       {
--         "unTokenName": ""
--       }
--     ]
--   }
-- }
data UtxoWithCurrencyRequest = UtxoWithCurrencyRequest
    { pageQuery :: Maybe (PageQuery TxOutRef)
    , currency  :: AssetClass
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

-- | Response type for the utxo-{at-address|with-currency} endpoints.
data UtxosResponse = UtxosResponse
    { currentTip :: Tip
    , page       :: Page TxOutRef
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

-- | Response type for the is-utxo endpoint.
data IsUtxoResponse = IsUtxoResponse
    { currentTip :: Tip
    , isUtxo     :: Bool
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

data TxoAtAddressRequest = TxoAtAddressRequest
    { pageQuery  :: Maybe (PageQuery TxOutRef)
    , credential :: Credential
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

-- | Response type for the txo-at-address endpoint.
data TxosResponse = TxosResponse
    { paget :: Page TxOutRef
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)


data QueryAtAddressRequest = QueryAtAddressRequest
    { pageQuery  :: Maybe (PageQuery TxOutRef)
    , credential :: Credential
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

-- | generic response type endpoint
-- This type is introduced to avoid querying the chain index twice to obtain the expected info.
-- Indeed, it returns the next page query if more items are available
data QueryResponse a = QueryResponse
    { queryResult :: a
    , nextQuery   :: Maybe (PageQuery TxOutRef)
    }
    deriving (Show, Generic, Eq)

deriving instance (FromJSON a, Generic a) => FromJSON (QueryResponse a)
deriving instance (ToJSON a, Generic a) => ToJSON (QueryResponse a)
deriving instance (OpenApi.ToSchema a, Generic a) => OpenApi.ToSchema (QueryResponse a)

type API
    = "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
    :<|> "from-hash" :> FromHashAPI
    :<|> "tx-out" :> Description "Get a transaction output from its reference." :> ReqBody '[JSON] TxOutRef :> Post '[JSON] DecoratedTxOut
    :<|> "unspent-tx-out" :> Description "Get a unspent transaction output from its reference." :> ReqBody '[JSON] TxOutRef :> Post '[JSON] DecoratedTxOut
    :<|> "tx" :> Description "Get a transaction from its id." :> ReqBody '[JSON] TxId :> Post '[JSON] ChainIndexTx
    :<|> "is-utxo" :> Description "Check if the reference is an UTxO." :> ReqBody '[JSON] TxOutRef :> Post '[JSON] IsUtxoResponse
    :<|> "utxo-at-address" :> Description "Get all UTxOs at an address." :> ReqBody '[JSON] UtxoAtAddressRequest :> Post '[JSON] UtxosResponse
    :<|> "unspent-txouts-at-address" :> Description "Get all unspent transaction output at an address." :> ReqBody '[JSON] QueryAtAddressRequest :> Post '[JSON] (QueryResponse [(TxOutRef, DecoratedTxOut)])
    :<|> "datums-at-address" :> Description "Get all Datums at an address." :> ReqBody '[JSON] QueryAtAddressRequest :> Post '[JSON] (QueryResponse [Datum])
    :<|> "utxo-with-currency" :> Description "Get all UTxOs with a currency." :> ReqBody '[JSON] UtxoWithCurrencyRequest :> Post '[JSON] UtxosResponse
    :<|> "txs" :> Description "Get transactions from a list of their ids." :> ReqBody '[JSON] [TxId] :> Post '[JSON] [ChainIndexTx]
    :<|> "txo-at-address" :> Description "Get TxOs at an address." :> ReqBody '[JSON] TxoAtAddressRequest :> Post '[JSON] TxosResponse
    :<|> "tip" :> Description "Get the current synced tip." :> Get '[JSON] Tip
    :<|> "collect-garbage" :> Description "Collect chain index garbage to free up space." :> Put '[JSON] NoContent
    :<|> "diagnostics" :> Description "Get the current stats of the chain index." :> Get '[JSON] Diagnostics

type FromHashAPI =
    "datum" :> Description "Get a datum from its hash." :> ReqBody '[JSON] DatumHash :> Post '[JSON] Datum
    :<|> "validator" :> Description "Get a validator script from its hash." :> ReqBody '[JSON] ValidatorHash :> Post '[JSON] (Versioned Validator)
    :<|> "minting-policy" :> Description "Get a minting policy from its hash." :> ReqBody '[JSON] MintingPolicyHash :> Post '[JSON] (Versioned MintingPolicy)
    :<|> "stake-validator" :> Description "Get a stake validator from its hash." :> ReqBody '[JSON] StakeValidatorHash :> Post '[JSON] (Versioned StakeValidator)
    :<|> "redeemer" :> Description "Get a redeemer from its hash." :> ReqBody '[JSON] RedeemerHash :> Post '[JSON] Redeemer

type SwaggerAPI = "swagger" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: forall dir api. Servant.Server api ~ Servant.Handler Value => Servant.Server (SwaggerSchemaUI' dir api)
swagger = swaggerSchemaUIServer (toOpenApi (Proxy @API))

-- We don't include `SwaggerAPI` into `API` to exclude it from the effects code.
type FullAPI = API :<|> SwaggerAPI

-- | Go through each 'Page's of 'QueryResponse', and collect the results.
collectQueryResponse ::
    ( Monad m )
    => (PageQuery TxOutRef -> m (QueryResponse a)) -- ^ query response function
    -> m [a]
collectQueryResponse q = go (Just def)
  where
    go Nothing = pure []
    go (Just pq) = do
      res <- q pq
      (queryResult res :) <$> go (nextQuery res)

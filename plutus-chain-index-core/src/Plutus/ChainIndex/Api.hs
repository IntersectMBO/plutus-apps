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
  , SwaggerAPI
  , UtxoAtAddressRequest(..)
  , UtxoWithCurrencyRequest(..)
  , swagger
  ) where

import Control.Monad.Freer.Extras.Pagination (Page, PageQuery)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Ledger (AssetClass, Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash, StakeValidator,
               StakeValidatorHash, TxId, Validator, ValidatorHash)
import Ledger.Credential (Credential)
import Ledger.Tx (ChainIndexTxOut, TxOutRef)
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.ChainIndex.Types (Diagnostics, Tip)
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

type API
    = "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
    :<|> "from-hash" :> FromHashAPI
    :<|> "tx-out" :> Description "Get a transaction output from its reference." :> ReqBody '[JSON] TxOutRef :> Post '[JSON] ChainIndexTxOut
    :<|> "tx" :> Description "Get a transaction from its id." :> ReqBody '[JSON] TxId :> Post '[JSON] ChainIndexTx
    :<|> "is-utxo" :> Description "Check if the reference is an UTxO." :> ReqBody '[JSON] TxOutRef :> Post '[JSON] (Tip, Bool)
    :<|> "utxo-at-address" :> Description "Get all UTxOs at an address." :> ReqBody '[JSON] UtxoAtAddressRequest :> Post '[JSON] (Tip, Page TxOutRef)
    :<|> "utxo-with-currency" :> Description "Get all UTxOs with a currency." :> ReqBody '[JSON] UtxoWithCurrencyRequest :> Post '[JSON] (Tip, Page TxOutRef)
    :<|> "tip" :> Description "Get the current synced tip." :> Get '[JSON] Tip
    :<|> "collect-garbage" :> Description "Collect chain index garbage to free up space." :> Put '[JSON] NoContent
    :<|> "diagnostics" :> Description "Get the current stats of the chain index." :> Get '[JSON] Diagnostics

type FromHashAPI =
    "datum" :> Description "Get a datum from its hash." :> ReqBody '[JSON] DatumHash :> Post '[JSON] Datum
    :<|> "validator" :> Description "Get a validator script from its hash." :> ReqBody '[JSON] ValidatorHash :> Post '[JSON] Validator
    :<|> "minting-policy" :> Description "Get a minting policy from its hash." :> ReqBody '[JSON] MintingPolicyHash :> Post '[JSON] MintingPolicy
    :<|> "stake-validator" :> Description "Get a stake validator from its hash." :> ReqBody '[JSON] StakeValidatorHash :> Post '[JSON] StakeValidator
    :<|> "redeemer" :> Description "Get a redeemer from its hash." :> ReqBody '[JSON] RedeemerHash :> Post '[JSON] Redeemer

type SwaggerAPI = "swagger" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: forall dir api. Servant.Server api ~ Servant.Handler Value => Servant.Server (SwaggerSchemaUI' dir api)
swagger = swaggerSchemaUIServer (toOpenApi (Proxy @API))

-- We don't include `SwaggerAPI` into `API` to exclude it from the effects code.
type FullAPI = API :<|> SwaggerAPI

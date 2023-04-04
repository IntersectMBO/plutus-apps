{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Marconi.Sidechain.Api.HttpServer where

import Cardano.Api ()
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word64)
import Marconi.Sidechain.Api.Query.Indexers.EpochState qualified as EpochState
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as Q.Utxo
import Marconi.Sidechain.Api.Routes (API, AddressUtxoResult, CurrentSyncedPointResult, EpochNonceResult,
                                     EpochStakePoolDelegationResult, JsonRpcAPI, MintingPolicyHashTxResult, RestAPI)
import Marconi.Sidechain.Api.Types (QueryExceptions, SidechainEnv, sidechainAddressUtxoIndexer,
                                    sidechainEnvHttpSettings, sidechainEnvIndexers)
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpcErr (JsonRpcErr, errorCode, errorData, errorMessage), parseErrorCode)
import Network.Wai.Handler.Warp (runSettings)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (Application, Handler, Server, serve)

-- | Bootstraps the HTTP server
bootstrap :: SidechainEnv -> IO ()
bootstrap env =  runSettings
        (env ^. sidechainEnvHttpSettings)
        (marconiApp env)

marconiApp :: SidechainEnv -> Application
marconiApp env = serve (Proxy @API) (httpRpcServer env)

jsonRpcServer
  :: SidechainEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Server JsonRpcAPI
jsonRpcServer env = echo
  :<|> getTargetAddressesQueryHandler env
  :<|> getCurrentSyncedPointHandler env
  :<|> getAddressUtxoHandler env
  :<|> getMintingPolicyHashTxHandler env
  :<|> getEpochStakePoolDelegationHandler env
  :<|> getEpochNonceHandler env

restApiServer
  :: SidechainEnv
  -> Server RestAPI
restApiServer env  = getTimeHandler :<|> getTargetAddressesHandler env

httpRpcServer
  :: SidechainEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Server API
httpRpcServer env = jsonRpcServer env :<|> restApiServer env

-- | Echos message back as a Jsonrpc response. Used for testing the server.
echo
    :: String
    ->  Handler (Either (JsonRpcErr String) String)
echo = return . Right

-- | Echos current time as REST response. Used for testing the http server outside of jsonrpc
-- protocol.
getTimeHandler :: Handler String
getTimeHandler = timeString <$> liftIO getCurrentTime
    where
    timeString = formatTime defaultTimeLocale "%T"

-- | Prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler
    :: SidechainEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
    -> Handler [Text]
getTargetAddressesHandler env =
    pure $ Q.Utxo.reportBech32Addresses
         $ env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer

-- | prints TargetAddresses Bech32 representation as thru JsonRpc
getTargetAddressesQueryHandler
    :: SidechainEnv -- ^ database configuration
    -> String
    -- ^ Will always be an empty string as we are ignoring this param, and returning everything
    -> Handler (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler env _ =
    pure $ Right
         $ Q.Utxo.reportBech32Addresses (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)

-- | Handler for retrieving current synced chain point.
getCurrentSyncedPointHandler
    :: SidechainEnv -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
    -> String
    -- ^ Will always be an empty string as we are ignoring this param, and returning everything
    -> Handler (Either (JsonRpcErr String) CurrentSyncedPointResult)
getCurrentSyncedPointHandler env _ = liftIO $
    first toRpcErr
    <$> Q.Utxo.currentSyncedPoint
           (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)

-- | Handler for retrieving UTXOs by Address
getAddressUtxoHandler
    :: SidechainEnv -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
    -> String -- ^ Bech32 addressCredential
    -> Handler (Either (JsonRpcErr String) AddressUtxoResult)
getAddressUtxoHandler env address = liftIO $
    first toRpcErr
    <$> Q.Utxo.findByBech32Address
            (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
            (pack address)

-- | Handler for retrieving Txs by Minting Policy Hash.
getMintingPolicyHashTxHandler
    :: SidechainEnv -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
    -> String           -- ^ Minting policy hash
    -> Handler (Either (JsonRpcErr String) MintingPolicyHashTxResult)
getMintingPolicyHashTxHandler _ _ = pure $ Left $ JsonRpcErr 1 "Endpoint not implemented yet" Nothing

-- | Handler for retrieving stake pool delegation per epoch
getEpochStakePoolDelegationHandler
    :: SidechainEnv -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
    -> Word64           -- ^ EpochNo
    -> Handler (Either (JsonRpcErr String) EpochStakePoolDelegationResult)
getEpochStakePoolDelegationHandler env epochNo = liftIO $
    first toRpcErr
    <$> EpochState.querySDDByEpochNo env epochNo

-- | Handler for retrieving stake pool delegation per epoch
getEpochNonceHandler
    :: SidechainEnv -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
    -> Word64           -- ^ EpochNo
    -> Handler (Either (JsonRpcErr String) EpochNonceResult)
getEpochNonceHandler env epochNo = liftIO $
    first toRpcErr
    <$> EpochState.queryNonceByEpochNo env epochNo

-- | Convert to JSON-RPC protocol error.
toRpcErr
    :: QueryExceptions
    -> JsonRpcErr String
toRpcErr e = JsonRpcErr {
           errorCode = parseErrorCode
           , errorMessage = "marconi RPC query related error!"
           , errorData = Just . show $ e
           }

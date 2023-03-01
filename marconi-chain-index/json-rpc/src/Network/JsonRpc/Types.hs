{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module: Network.JsonRpc.Types
--
-- Work with JSON-RPC protocol
--
module Network.JsonRpc.Types
    (
    -- * API specification types
      RawJsonRpc
    , JsonRpc
    , JsonRpcNotification
    , JSONRPC

    -- * JSON-RPC messages
    , Request (..)
    , JsonRpcErr (..)
    , JsonRpcResponse (..)

    -- ** Standard error codes
    , parseErrorCode
    , invalidRequestCode
    , methodNotFoundCode
    , invalidParamsCode
    , internalErrorCode

    -- * Type rewriting
    , JsonRpcEndpoint
    ) where


import Control.Applicative (liftA3, (<|>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Null), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Read (decimal)
import Data.Word (Word64)
import GHC.TypeLits (Symbol)
import Network.HTTP.Media ((//))
import Servant.API (Accept (contentTypes), JSON, MimeRender (mimeRender), MimeUnrender (mimeUnrender), NoContent, Post,
                    ReqBody, (:>))

-- | Client messages
data Request p
    = Request
    { method    :: String
    , params    :: p
    , requestId :: Maybe Word64 -- ^ omitted for notification type messages
    } deriving (Eq, Show)

instance ToJSON p => ToJSON (Request p) where
    toJSON (Request m p ix) =
        object
            . maybe id (onValue "id") ix
            $ [ "jsonrpc" .= ("2.0" :: String)
              , "method" .= m
              , "params" .= p
              ]
        where
        onValue n v = ((n .= v) :)

instance FromJSON p => FromJSON (Request p) where
    parseJSON = withObject "JsonRpc Request" $ \obj -> do
        ix <- obj .:? "id"
        m  <- obj .:  "method"
        p  <- obj .:  "params"
        v  <- obj .:  "jsonrpc"

        versionGuard v . pure $ Request m p ix

-- | JSON-RPC supported version, 2.0 at this time
versionGuard :: Maybe String -> Parser a -> Parser a
versionGuard v x
    | v == Just "2.0" = x
    | isNothing v     = x
    | otherwise       = fail "unknown version"

-- | Server 'Ack' message
data JsonRpcResponse e r
    = Result Word64 r
    | Ack Word64
    | Errors (Maybe Word64) (JsonRpcErr e)
    deriving (Eq, Show)

data JsonRpcErr e = JsonRpcErr
    { errorCode    :: Int
    , errorMessage :: String
    , errorData    :: Maybe e
    } deriving (Eq, Show)

-- | JSON-RPC error codes based on [JSONRPC Spec](https://www.jsonrpc.org/specification#error_object)
parseErrorCode :: Int
parseErrorCode      = -32700

invalidRequestCode :: Int
invalidRequestCode  = -32600

methodNotFoundCode :: Int
methodNotFoundCode  = -32601

invalidParamsCode :: Int
invalidParamsCode   = -32602

internalErrorCode :: Int
internalErrorCode   = -32603

instance (FromJSON e, FromJSON r) => FromJSON (JsonRpcResponse e r) where
    parseJSON = withObject "Response" $ \obj -> do
        ix      <- obj .:  "id" <|> (obj .: "id" >>= parseDecimalString)
        version <- obj .:? "jsonrpc"
        result  <- obj .:? "result"
        err     <- obj .:? "error"
        versionGuard version $ pack ix result err
        where
        parseDecimalString = either fail (pure . fmap fst) . traverse decimal
        pack (Just ix) (Just r) Nothing = pure $ Result ix r
        pack ix Nothing (Just e)        = Errors ix <$> parseErr e
        pack (Just ix) Nothing Nothing  = pure $ Ack ix
        pack _ _ _                      = fail "invalid response"
        parseErr = withObject "Error" $
            liftA3 JsonRpcErr <$> (.: "code") <*> (.: "message") <*> (.:? "data")

instance (ToJSON e, ToJSON r) => ToJSON (JsonRpcResponse e r) where
    toJSON (Result ix r) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "result"  .= r
               , "id"      .= ix
               ]

    toJSON (Ack ix) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "id"      .= ix
               , "result"  .= Null
               , "error"   .= Null
               ]

    toJSON (Errors ix (JsonRpcErr c msg err)) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "id"      .= ix
               , "error"   .= detail
               ]
         where
         detail = object [ "code"    .= c
                         , "message" .= msg
                         , "data"    .= err
                         ]

-- | A JSON RPC server handles any number of methods.
data RawJsonRpc api

-- | JSON-RPC endpoints which respond with a result
data JsonRpc (method :: Symbol) p e r

-- | JSON-RPC endpoints which do not respond
data JsonRpcNotification (method :: Symbol) p

-- | JSON-RPC
type family JsonRpcEndpoint a where
    JsonRpcEndpoint (JsonRpc m p e r)
        = ReqBody '[JSONRPC] (Request p) :> Post '[JSONRPC] (JsonRpcResponse e r)

    JsonRpcEndpoint (JsonRpcNotification m p)
        = ReqBody '[JSONRPC] (Request p) :> Post '[JSONRPC] NoContent

-- | The JSON-RPC content type
data JSONRPC

instance Accept JSONRPC where
    contentTypes _ = "application" // "json-rpc" :| ["application" // "json"]

instance ToJSON a => MimeRender JSONRPC a where
    mimeRender _ = mimeRender (Proxy @JSON)

instance FromJSON a => MimeUnrender JSONRPC a where
    mimeUnrender _ = mimeUnrender (Proxy @JSON)

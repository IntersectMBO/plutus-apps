{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- |
-- This module provides support for writing handlers for JSON-RPC endpoints
module Marconi.Server.Types where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Marconi.JsonRpc.Types (JSONRPC, JsonRpc, JsonRpcErr (JsonRpcErr, errorData), JsonRpcNotification,
                              JsonRpcResponse (Errors, Result), RawJsonRpc, Request (Request), invalidParamsCode,
                              invalidRequestCode, methodNotFoundCode)
import Servant.API (NoContent (NoContent), Post, ReqBody, (:<|>) ((:<|>)), (:>))
import Servant.API.ContentTypes (AllCTRender (handleAcceptH))
import Servant.Server (DefaultErrorFormatters, ErrorFormatters, Handler, HasContextEntry,
                       HasServer (hoistServerWithContext, route, type ServerT), type (.++))

-- | The entire JSON RPC api is collapsed to a single endpoint.
-- Therefore, we need a type that may or may not return content.
data MaybeContent a
    = SomeContent a
    | EmptyContent

instance ToJSON a => AllCTRender '[JSONRPC] (MaybeContent a) where
    handleAcceptH px h = \case
        SomeContent x -> handleAcceptH px h x
        EmptyContent  -> handleAcceptH px h NoContent

type MaybeJsonRpcResponse = MaybeContent (JsonRpcResponse Value Value)

type RawJsonRpcEndpoint
    = ReqBody '[JSONRPC] (Request Value)
   :> Post '[JSONRPC] MaybeJsonRpcResponse

instance (RouteJsonRpc api, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters)
    => HasServer (RawJsonRpc api) context where
    type ServerT (RawJsonRpc api) m = RpcHandler api m
    route _ cx = route endpoint cx . fmap (serveJsonRpc pxa pxh)
        where
            endpoint = Proxy @RawJsonRpcEndpoint
            pxa      = Proxy @api
            pxh      = Proxy @Handler

    hoistServerWithContext _ _ f x = hoistRpcRouter (Proxy @api) f x

-- | This internal class is how we accumulate a map of handlers for dispatch
class RouteJsonRpc a where
    type RpcHandler a (m :: * -> *)
    jsonRpcRouter
        :: Monad m => Proxy a -> Proxy m -> RpcHandler a m
        -> Map.Map String (Value -> m (MaybeContent (Either (JsonRpcErr Value) Value)))
    hoistRpcRouter :: Proxy a -> (forall x . m x -> n x) -> RpcHandler a m -> RpcHandler a n

generalizeResponse
    :: (ToJSON e, ToJSON r)
    => Either (JsonRpcErr e) r
    -> Either (JsonRpcErr Value) Value
generalizeResponse = bimap repack toJSON
    where
    repack e = e { errorData = toJSON <$> errorData e }

onDecodeFail :: String -> JsonRpcErr e
onDecodeFail msg = JsonRpcErr invalidParamsCode msg Nothing

instance (KnownSymbol method, FromJSON p, ToJSON e, ToJSON r)
    => RouteJsonRpc (JsonRpc method p e r) where
    type RpcHandler (JsonRpc method p e r) m = p -> m (Either (JsonRpcErr e) r)
    jsonRpcRouter _ _ h = Map.fromList [ (methodName, h') ]
        where
        methodName = symbolVal $ Proxy @method
        onDecode   = fmap generalizeResponse . h
        h' = fmap SomeContent
           . either (return . Left . onDecodeFail) onDecode
           . parseEither parseJSON

    hoistRpcRouter _ f x = f . x

instance (KnownSymbol method, FromJSON p) => RouteJsonRpc (JsonRpcNotification method p) where
    type RpcHandler (JsonRpcNotification method p) m = p -> m NoContent
    jsonRpcRouter _ _ h = Map.fromList [ (methodName, h') ] where
        methodName = symbolVal $ Proxy @method
        onDecode x = EmptyContent <$ h x
        h' = either (return . SomeContent . Left . onDecodeFail) onDecode
           . parseEither parseJSON
    hoistRpcRouter _ f x = f . x

instance (RouteJsonRpc a, RouteJsonRpc b) => RouteJsonRpc (a :<|> b) where
    type RpcHandler (a :<|> b) m = RpcHandler a m :<|> RpcHandler b m
    jsonRpcRouter _ pxm (ha :<|> hb) = jsonRpcRouter pxa pxm ha <> jsonRpcRouter pxb pxm hb
        where
        pxa = Proxy @a
        pxb = Proxy @b
    hoistRpcRouter _ f (x :<|> y) = hoistRpcRouter (Proxy @a) f x :<|> hoistRpcRouter (Proxy @b) f y

-- | Collapse a to a single handler to handle RawJsonRpc
serveJsonRpc
    :: (Monad m, RouteJsonRpc a)
    => Proxy a
    -> Proxy m
    -> RpcHandler a m
    -> Request Value
    -> m MaybeJsonRpcResponse
serveJsonRpc px pxm hs (Request m v ix')
    | Just h <- Map.lookup m hmap
    = h v >>= \case
        SomeContent (Right x) | Just ix <- ix' -> return . SomeContent $ Result ix x
                              | otherwise      -> return . SomeContent $ Errors ix' invalidRequest
        SomeContent (Left e)                   -> return . SomeContent $ Errors ix' e
        EmptyContent                           -> return EmptyContent
    | otherwise = return . SomeContent $ Errors ix' missingMethod
    where
    missingMethod  = JsonRpcErr methodNotFoundCode ("Unknown method: " <> m) Nothing
    hmap           = jsonRpcRouter px pxm hs
    invalidRequest = JsonRpcErr invalidRequestCode "Missing id" Nothing

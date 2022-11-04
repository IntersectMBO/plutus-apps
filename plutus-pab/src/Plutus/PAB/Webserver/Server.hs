{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Plutus.PAB.Webserver.Server
    ( startServer
    , startServer'
    , startServerDebug
    , startServerDebug'
    ) where

import Control.Concurrent (MVar, forkFinally, forkIO, newEmptyMVar, putMVar)
import Control.Concurrent.Availability (Availability, available, newToken)
import Control.Concurrent.STM qualified as STM
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Freer.Extras.Log (logInfo, logWarn)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.Monoid (Endo (Endo, appEndo))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.Servant.Options qualified as Cors
import Plutus.PAB.Core (PABAction, PABRunner (PABRunner, runPABAction))
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Monitoring.PABLogMsg qualified as LM
import Plutus.PAB.Simulator (Simulation)
import Plutus.PAB.Types (PABError,
                         WebserverConfig (WebserverConfig, certificatePath, endpointTimeout, keyPath, permissiveCorsPolicy, staticDir),
                         baseUrl, defaultWebServerConfig)
import Plutus.PAB.Webserver.API (API, SwaggerAPI, WSAPI)
import Plutus.PAB.Webserver.Handler (apiHandler, swagger)
import Plutus.PAB.Webserver.WebSocket qualified as WS
import Servant (Application, Handler (Handler), Raw, ServerT, err500, errBody, hoistServer, serve,
                serveDirectoryFileServer, (:<|>) ((:<|>)))
import Servant qualified
import Servant.Client (BaseUrl (baseUrlPort))
import Wallet.Emulator.Wallet (WalletId)

asHandler :: forall t env a. PABRunner t env -> PABAction t env a -> Handler a
asHandler PABRunner{runPABAction} = Servant.Handler . ExceptT . fmap (first mapError) . runPABAction where
    mapError :: PABError -> Servant.ServerError
    mapError e = Servant.err500 { Servant.errBody = LBS.pack $ show e }

type CombinedAPI t = BaseCombinedAPI t :<|> SwaggerAPI

type BaseCombinedAPI t =
    API (Contract.ContractDef t) WalletId
    :<|> WSAPI

app ::
    forall t env.
    ( FromJSON (Contract.ContractDef t)
    , ToJSON (Contract.ContractDef t)
    , Contract.PABContract t
    , Servant.MimeUnrender Servant.JSON (Contract.ContractDef t)
    , OpenApi.ToSchema (Contract.ContractDef t)
    ) =>
    Maybe FilePath
    -> PABRunner t env
    -> Application
app fp pabRunner = do
    let apiServer :: ServerT (CombinedAPI t) Handler
        apiServer =
            Servant.hoistServer
                (Proxy @(BaseCombinedAPI t))
                (asHandler pabRunner)
                (apiHandler :<|> WS.wsHandler) :<|> (swagger @t)

    case fp of
        Nothing -> do
            Servant.serve (Proxy @(CombinedAPI t)) apiServer
        Just filePath -> do
            let
                fileServer :: ServerT Raw Handler
                fileServer = serveDirectoryFileServer filePath
            Servant.serve (Proxy @(CombinedAPI t :<|> Raw)) (apiServer :<|> fileServer)

-- | Start the server using the config. Returns an action that shuts it down
--   again, and an MVar that is filled when the webserver
--   thread exits.
startServer ::
    forall t env.
    ( FromJSON (Contract.ContractDef t)
    , ToJSON (Contract.ContractDef t)
    , Contract.PABContract t
    , Servant.MimeUnrender Servant.JSON (Contract.ContractDef t)
    , OpenApi.ToSchema (Contract.ContractDef t)
    )
    => WebserverConfig -- ^ Optional file path for static assets
    -> Availability
    -> PABAction t env (MVar (), PABAction t env ())
startServer WebserverConfig{baseUrl, staticDir, permissiveCorsPolicy, endpointTimeout, certificatePath, keyPath} availability = do
    when permissiveCorsPolicy $
      logWarn @(LM.PABMultiAgentMsg t) (LM.UserLog "Warning: Using a very permissive CORS policy! *Any* website serving JavaScript can interact with these endpoints.")
    startServer' middlewares (baseUrlPort baseUrl) tlsSettings staticDir availability (timeout endpointTimeout)
      where
        middlewares = if permissiveCorsPolicy then corsMiddlewares else []
        corsMiddlewares =
            [ -- a custom CORS policy since 'simpleCors' doesn't support "content-type" header by default
            let policy = Cors.simpleCorsResourcePolicy { Cors.corsRequestHeaders = [ "content-type" ] }
            in Cors.cors (const $ Just policy)
            -- this middleware handles preflight OPTIONS browser requests
            , Cors.provideOptions (Proxy @(API (Contract.ContractDef t) Integer))
            ]
        tlsSettings = WarpTLS.tlsSettings <$> certificatePath <*> keyPath
        -- By default we use the normal request timeout: 30 seconds. But if
        -- someone has asked for a longer endpoint timeout, we need to set
        -- that to be the webserver timeout as well.
        timeout Nothing  = 30
        timeout (Just s) = fromIntegral $ max s 30

-- | Start the server. Returns an action that shuts it down
--   again, and an MVar that is filled when the webserver
--   thread exits.
startServer' ::
    forall t env.
    ( FromJSON (Contract.ContractDef t)
    , ToJSON (Contract.ContractDef t)
    , Contract.PABContract t
    , Servant.MimeUnrender Servant.JSON (Contract.ContractDef t)
    , OpenApi.ToSchema (Contract.ContractDef t)
    )
    => [Middleware] -- ^ Optional wai middleware
    -> Int -- ^ Port
    -> Maybe WarpTLS.TLSSettings -- ^ Optionally use HTTPS with these settings
    -> Maybe FilePath -- ^ Optional file path for static assets
    -> Availability
    -> Int
    -> PABAction t env (MVar (), PABAction t env ())
startServer' waiMiddlewares port tlsSettings staticPath availability timeout = do
    simRunner <- Core.pabRunner
    shutdownVar <- liftIO $ STM.atomically $ STM.newEmptyTMVar @()
    mvar <- liftIO newEmptyMVar

    let shutdownHandler :: IO () -> IO ()
        shutdownHandler doShutdown = void $ forkIO $ do
            STM.atomically $ STM.takeTMVar shutdownVar
            putStrLn "webserver: shutting down"
            doShutdown
        warpSettings :: Warp.Settings
        warpSettings = Warp.defaultSettings
            & Warp.setPort port
            & Warp.setInstallShutdownHandler shutdownHandler
            & Warp.setBeforeMainLoop (available availability)
            & Warp.setTimeout timeout
            & Warp.setHost "*6" -- HostIPv6@ - "any IPv4 or IPv6 hostname, IPv6 preferred"
        middleware = appEndo $ foldMap Endo waiMiddlewares
        run = maybe Warp.runSettings WarpTLS.runTLS tlsSettings
    logInfo @(LM.PABMultiAgentMsg t) (LM.StartingPABBackendServer port)
    void $ liftIO $
        forkFinally
            (run warpSettings $ middleware
               $ app staticPath simRunner)
            (\_ -> putMVar mvar ())

    pure (mvar, liftIO $ STM.atomically $ STM.putTMVar shutdownVar ())

-- | Start the server using a default configuration for debugging.
startServerDebug ::
    ( FromJSON (Contract.ContractDef t)
    , ToJSON (Contract.ContractDef t)
    , Contract.PABContract t
    , Servant.MimeUnrender Servant.JSON (Contract.ContractDef t)
    , OpenApi.ToSchema (Contract.ContractDef t)
    )
    => Simulation t (Simulation t ())
startServerDebug = startServerDebug' defaultWebServerConfig

-- | Start the server using (mostly) a default configuration for debugging,
-- but allow an optional webserver config.
startServerDebug' ::
    ( FromJSON (Contract.ContractDef t)
    , ToJSON (Contract.ContractDef t)
    , Contract.PABContract t
    , Servant.MimeUnrender Servant.JSON (Contract.ContractDef t)
    , OpenApi.ToSchema (Contract.ContractDef t)
    )
    => WebserverConfig
    -> Simulation t (Simulation t ())
startServerDebug' conf = do
    tk <- newToken
    snd <$> startServer conf tk

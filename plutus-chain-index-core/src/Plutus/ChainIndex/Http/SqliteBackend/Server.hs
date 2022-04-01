{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}
module Plutus.ChainIndex.Http.SqliteBackend.Server
    ( serveChainIndexQueryServer
    ) where

import Control.Monad.Except qualified as E
import Control.Monad.Freer (Eff, type (~>))
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Plutus.ChainIndex (RunRequirements, runChainIndexEffects)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Http.Server (serveChainIndexServer)
import Servant.Server (Handler, ServerError, err500, errBody)

serveChainIndexQueryServer ::
    Int -- ^ Port
    -> RunRequirements
    -> IO ()
serveChainIndexQueryServer port runReq =
    serveChainIndexServer port (runChainIndexQuery runReq)

runChainIndexQuery ::
    RunRequirements
    -> Eff '[Error ServerError, ChainIndexQueryEffect, ChainIndexControlEffect] ~> Handler
runChainIndexQuery runReq action = do
    result <- liftIO $ runChainIndexEffects runReq $ runError $ raiseEnd action
    case result of
        Right (Right a) -> pure a
        Right (Left e) -> E.throwError e
        Left e' ->
            let err = err500 { errBody = BSL.fromStrict $ Text.encodeUtf8 $ Text.pack $ show e' } in
            E.throwError err

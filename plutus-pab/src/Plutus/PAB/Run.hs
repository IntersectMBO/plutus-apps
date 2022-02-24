{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Plutus.PAB.Run
( runWith
, runWithOpts
) where

import Cardano.BM.Backend.EKGView qualified as EKGView
import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Plugin (loadPlugin)
import Cardano.BM.Setup (setupTrace_)
import Cardano.Node.Types (pscPassphrase)
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent.Availability (newToken)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text.Extras (tshow)
import Data.Yaml (decodeFileThrow)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions)
import Plutus.PAB.Monitoring.Config (defaultConfig, loadConfig)
import Plutus.PAB.Monitoring.PABLogMsg (AppMsg)
import Plutus.PAB.Run.Cli (ConfigCommandArgs (ConfigCommandArgs), ccaAvailability, ccaLoggingConfig, ccaPABConfig,
                           ccaStorageBackend, ccaTrace, runConfigCommand)
import Plutus.PAB.Run.CommandParser
import Plutus.PAB.Types (Config (Config), DevelopmentOptions (pabResumeFrom, pabRollbackHistory),
                         PABError (MissingConfigFileOption), developmentOptions, nodeServerConfig)
import Prettyprinter (Pretty (pretty))
import Servant qualified
import System.Exit (ExitCode (ExitFailure), exitWith)

-- | PAB entry point for a contract type `a`.
runWith :: forall a.
    ( Show a
    , Ord a
    , FromJSON a
    , ToJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , HasDefinitions a
    , OpenApi.ToSchema a
    )
    => BuiltinHandler a -- ^ Builtin contract handler. Can be created with 'Plutus.PAB.Effects.Contract.Builtin.handleBuiltin'.
    -> IO ()
runWith h = parseOptions >>= runWithOpts h Nothing

-- | Helper function to launch a complete PAB (all the necessary services)
-- that can be interacted over the API endpoints defined in
-- 'PAB.Webserver.Server'.
runWithOpts :: forall a.
    ( Show a
    , Ord a
    , FromJSON a
    , ToJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , HasDefinitions a
    , OpenApi.ToSchema a
    )
    => BuiltinHandler a
    -> Maybe Config -- ^ Optional config override to use in preference to the one in AppOpts
    -> AppOpts
    -> IO ()
runWithOpts userContractHandler mc AppOpts { minLogLevel, rollbackHistory, resumeFrom, logConfigPath, passphrase, runEkgServer, cmd, configPath, storageBackend } = do
    -- Parse config files and initialize logging
    logConfig <- maybe defaultConfig loadConfig logConfigPath
    for_ minLogLevel $ \ll -> CM.setMinSeverity logConfig ll
    (trace :: Trace IO (PrettyObject (AppMsg (Builtin a))), switchboard) <- setupTrace_ logConfig "pab"

    -- enable EKG backend
    when runEkgServer $ EKGView.plugin logConfig trace switchboard >>= loadPlugin switchboard

    -- obtain token for signaling service readiness
    serviceAvailability <- newToken

    pabConfig :: Either PABError Config <- case mc of
        Just config -> pure $ Right config
        Nothing ->
          case configPath of
            Nothing -> pure $ Left MissingConfigFileOption
            Just p  -> do Right <$> (liftIO $ decodeFileThrow p)

    let mkNodeServerConfig nodeServerConfig =
            nodeServerConfig
                { pscPassphrase = passphrase <|> pscPassphrase nodeServerConfig
                }
        mkDevOpts developmentOptions =
            developmentOptions
                { pabRollbackHistory = rollbackHistory
                                   <|> pabRollbackHistory developmentOptions
                , pabResumeFrom = max resumeFrom (pabResumeFrom developmentOptions)

                }
        mkArgs config@Config{nodeServerConfig, developmentOptions} =
            ConfigCommandArgs
                { ccaTrace = convertLog PrettyObject trace
                , ccaLoggingConfig = logConfig
                , ccaPABConfig =
                    config { nodeServerConfig = mkNodeServerConfig nodeServerConfig
                           , developmentOptions = mkDevOpts developmentOptions
                           }
                , ccaAvailability = serviceAvailability
                , ccaStorageBackend = storageBackend
                }

    -- execute parsed pab command and handle errors on faliure
    result <- sequence (run . mkArgs <$> pabConfig)
    either handleError (const $ pure ()) result

    where
        run config = runConfigCommand userContractHandler config cmd
        handleError (err :: PABError) = do
            runStdoutLoggingT $ (logErrorN . tshow . pretty) err
            exitWith (ExitFailure 1)

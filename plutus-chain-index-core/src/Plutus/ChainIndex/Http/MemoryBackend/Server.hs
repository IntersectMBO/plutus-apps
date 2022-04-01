{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Plutus.ChainIndex.Http.MemoryBackend.Server
    ( serveChainIndexQueryServer
    , handleChainIndexControl
    ) where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Except qualified as E
import Control.Monad.Freer (Eff, Member, interpret, run, type (~>))
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras.Log (LogMsg, handleLogIgnore)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.State (State, evalState)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Http.Server (serveChainIndexServer)
import Plutus.ChainIndex.Indexer.Memory.Handlers (ChainIndexEmulatorState (..), handleAssetClassFromAddressIndexer,
                                                  handleDatumFromHashIndexer, handleQuery,
                                                  handleRedeemerFromHashIndexer, handleScriptFromHashIndexer,
                                                  handleTxFromIdIndexer, handleUtxoFromAddressIndexer,
                                                  handleUtxoIndexer)
import Servant.Server (Handler, ServerError, err500, errBody)

serveChainIndexQueryServer ::
    Int -- ^ Port
    -> TVar ChainIndexEmulatorState -- ^ Chain index state on memory
    -> IO ()
serveChainIndexQueryServer port diskState = do
    serveChainIndexServer port (runChainIndexQuery diskState)

runChainIndexQuery ::
    TVar ChainIndexEmulatorState
    -> Eff '[Error ServerError, ChainIndexQueryEffect, ChainIndexControlEffect] ~> Handler
runChainIndexQuery emState_ action = do
    emState <- liftIO (STM.readTVarIO emState_)
    let result = run
                    $ evalState emState
                    $ runError @ChainIndexError
                    $ handleLogIgnore @ChainIndexLog

                    -- All indexers
                    $ handleChainIndexControl

                    $ interpret handleQuery

                    $ runError
                    $ raiseEnd action
    case result of
        Right (Right a) -> pure a
        Right (Left e) -> E.throwError e
        Left e' ->
            let err = err500 { errBody = BSL.fromStrict $ Text.encodeUtf8 $ Text.pack $ show e' } in
            E.throwError err

-- | Handle the chain index control effects from the set of all effects using the
-- memory store.
handleChainIndexControl ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => Eff (ChainIndexControlEffect ': effs) ~> Eff effs
handleChainIndexControl action = do
    interpret
        (\eff ->
            handleUtxoIndexer eff
         >> handleDatumFromHashIndexer eff
         >> handleRedeemerFromHashIndexer eff
         >> handleScriptFromHashIndexer eff
         >> handleUtxoFromAddressIndexer eff
         >> handleAssetClassFromAddressIndexer eff
         >> handleTxFromIdIndexer eff
        )
        action

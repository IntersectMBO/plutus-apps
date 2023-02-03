-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Mamba.Bootstrap  where

import Control.Concurrent.STM (atomically)
import Control.Exception (catch)
import Control.Lens ((^.))
import Data.List.NonEmpty (fromList, nub)
import Data.Text (pack)
import Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

import Cardano.Api (AsType (AsShelleyAddress), ChainPoint (ChainPointAtGenesis), deserialiseFromBech32)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Marconi.ChainIndex.Indexers (mkIndexerStream, startIndexers, utxoWorker)
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Mamba.Api.HttpServer qualified as Http
import Marconi.Mamba.Api.Query.Indexers.Utxo (UtxoIndexer, initializeEnv, writeTMVar')
import Marconi.Mamba.Api.Types (CliArgs (CliArgs), HasIndexerEnv (uiIndexer), HasMambaEnv (queryEnv),
                                MambaEnv (MambaEnv, _httpSettings, _queryEnv))


-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
initializeIndexerEnv
    :: Maybe Port
    -> TargetAddresses
    -> IO MambaEnv
initializeIndexerEnv maybePort targetAddresses = do
    queryenv <- initializeEnv targetAddresses
    let httpsettings =  maybe defaultSettings (flip setPort defaultSettings ) maybePort
    pure $ MambaEnv
        { _httpSettings = httpsettings
        , _queryEnv = queryenv
        }

bootstrapHttp
    :: MambaEnv
    -> IO ()
bootstrapHttp  = Http.bootstrap

-- |  Marconi cardano blockchain indexer
bootstrapIndexers
    :: CliArgs
    -> MambaEnv
    -> IO ()
bootstrapIndexers (CliArgs socket dbPath _ networkId targetAddresses) env = do
  let callbackIndexer :: UtxoIndexer -> IO ()
      callbackIndexer = atomically . writeTMVar' (env ^. queryEnv . uiIndexer)
  (_, coordinator) <-
      startIndexers [( utxoWorker callbackIndexer (Just targetAddresses)
                     , dbPath )]
  let indexers   = mkIndexerStream coordinator
      chainPoint = ChainPointAtGenesis
  c <- defaultConfigStdout
  withTrace c "marconi-mamba" $ \trace ->
    withChainSyncEventStream socket networkId [chainPoint] indexers
    `catch` \NoIntersectionFound ->
      logError trace $
          renderStrict $
              layoutPretty defaultLayoutOptions $
                  "No intersection found when looking for the chain point"
                  <+> (pretty . show  $ chainPoint)  <> "."
                  <+> "Please check the slot number and the block hash do belong to the chain"

-- | parses a white space separated address list
-- Note, duplicate addresses are rmoved
targetAddressParser
    :: String           -- ^ contains white spece delimeted lis of addresses
    -> TargetAddresses  -- ^ a non empty list of valid addresses
targetAddressParser =
    nub
    . fromList
    . fromJustWithError
    . traverse (deserializeToCardano . pack)
    . words
    where
        deserializeToCardano = deserialiseFromBech32 AsShelleyAddress

-- | Exit program with error
-- Note, if the targetAddress parser fails, or is empty, there is nothing to do for the hotStore.
-- In such case we should fail fast
fromJustWithError :: (Show e) => Either e a -> a
fromJustWithError v = case v of
    Left e ->
        error $ "\n!!!\n Abnormal Termination with Error: " <> show e <> "\n!!!\n"
    Right accounts -> accounts

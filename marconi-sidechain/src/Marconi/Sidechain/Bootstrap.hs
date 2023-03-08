-- |
-- This module bootstraps the marconi-sidechain JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Sidechain.Bootstrap where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Marconi.ChainIndex.Indexers (epochStakepoolSizeWorker, runIndexers, utxoWorker)
import Marconi.ChainIndex.Indexers.EpochStakepoolSize (EpochSPDHandle)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.ChainIndex.Types (TargetAddresses, epochStakepoolSizeDbName, utxoDbName)
import Marconi.Core.Storable (State, StorableEvent)
import Marconi.Sidechain.Api.Query.Indexers.EpochSPD qualified as EpochSPD
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as AddressUtxo
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs), SidechainEnv (SidechainEnv),
                                    SidechainIndexers (SidechainIndexers), epochSpdIndexerEnvIndexer,
                                    sidechainAddressUtxoIndexer, sidechainEnvIndexers,
                                    sidechainEpochStakePoolDelegationIndexer)
import Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import System.FilePath ((</>))

-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
initializeSidechainEnv
    :: Maybe Port
    -> Maybe TargetAddresses
    -> IO SidechainEnv
initializeSidechainEnv maybePort targetAddresses = do
    let httpsettings = maybe defaultSettings (flip setPort defaultSettings ) maybePort
    sidechainIndexers <-
        SidechainIndexers
            <$> AddressUtxo.initializeEnv targetAddresses
            <*> EpochSPD.initializeEnv
    pure $ SidechainEnv httpsettings sidechainIndexers

-- |  Marconi cardano blockchain indexer
bootstrapIndexers
    :: CliArgs
    -> SidechainEnv
    -> IO ()
bootstrapIndexers (CliArgs socketPath nodeConfigPath dbPath _ networkId targetAddresses) env = do
  let addressUtxoCallback :: State UtxoHandle -> IO ()
      addressUtxoCallback =
          atomically
        . AddressUtxo.updateEnvState (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
  let epochSPDCallback :: (State EpochSPDHandle, StorableEvent EpochSPDHandle) -> IO ()
      epochSPDCallback =
          atomically
        . EpochSPD.updateEnvState (env ^. sidechainEnvIndexers . sidechainEpochStakePoolDelegationIndexer . epochSpdIndexerEnvIndexer)
        . fst
  let indexers =
          [ ( utxoWorker addressUtxoCallback targetAddresses
            , Just $ dbPath </> utxoDbName
            )
          , ( epochStakepoolSizeWorker nodeConfigPath epochSPDCallback
            , Just $ dbPath </> epochStakepoolSizeDbName
            )
          ]
  runIndexers
    socketPath
    networkId
    C.ChainPointAtGenesis
    "marconi-sidechain"
    indexers

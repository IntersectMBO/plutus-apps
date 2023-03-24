module Marconi.Sidechain.Api.Query.Indexers.EpochSPD
    ( initializeEnv
    , updateEnvState
    , querySPDByEpochNo
    ) where

import Cardano.Api qualified as C
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, tryReadTMVar)
import Control.Lens ((^.))
import Control.Monad.STM (STM, atomically)
import Data.Word (Word64)
import Marconi.ChainIndex.Indexers.EpochStakepoolSize (EpochSPDHandle, StorableQuery (SPDByEpochNoQuery),
                                                       StorableResult (SPDByEpochNoResult))
import Marconi.Core.Storable (State)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (EpochStakePoolDelegationResult (EpochStakePoolDelegationResult))
import Marconi.Sidechain.Api.Types (EpochSPDIndexerEnv (EpochSPDIndexerEnv), QueryExceptions (QueryError), SidechainEnv,
                                    epochSpdIndexerEnvIndexer, sidechainEnvIndexers,
                                    sidechainEpochStakePoolDelegationIndexer)
import Marconi.Sidechain.Utils (writeTMVar)

-- | Bootstraps the EpochSPD query environment.
-- The module is responsible for accessing SQLite for queries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
initializeEnv
    :: IO EpochSPDIndexerEnv -- ^ returns Query runtime environment
initializeEnv = EpochSPDIndexerEnv <$> newEmptyTMVarIO

updateEnvState :: TMVar (State EpochSPDHandle) -> State EpochSPDHandle -> STM ()
updateEnvState = writeTMVar

-- | Retrieve SPD associated at the given 'EpochNo'
-- We return an empty list if the 'EpochNo' is not found.
querySPDByEpochNo
    :: SidechainEnv -- ^ Query run time environment
    -> Word64 -- ^ Bech32 Address
    -> IO (Either QueryExceptions EpochStakePoolDelegationResult)  -- ^ Plutus address conversion error may occur
querySPDByEpochNo env epochNo = do
    -- We must stop the indexer inserts before doing the query.
    epochSpdIndexer <-
        atomically
        $ tryReadTMVar
        $ env ^. sidechainEnvIndexers . sidechainEpochStakePoolDelegationIndexer . epochSpdIndexerEnvIndexer
    case epochSpdIndexer of
      Nothing      -> pure $ Left $ QueryError "Failed to read EpochSPD indexer"
      Just indexer -> query indexer

    where
        query indexer = do
            (SPDByEpochNoResult epochSpdRows) <-
                Storable.query Storable.QEverything indexer (SPDByEpochNoQuery $ C.EpochNo epochNo)
            pure $ Right $ EpochStakePoolDelegationResult epochSpdRows

module Marconi.Sidechain.Api.Query.Indexers.EpochState
    ( initializeEnv
    , updateEnvState
    , querySDDByEpochNo
    ) where

import Cardano.Api qualified as C
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, tryReadTMVar)
import Control.Lens ((^.))
import Control.Monad.STM (STM, atomically)
import Data.Word (Word64)
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle, StorableQuery (SDDByEpochNoQuery),
                                               StorableResult (SDDByEpochNoResult))
import Marconi.Core.Storable (State)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (EpochStakePoolDelegationResult (EpochStakePoolDelegationResult))
import Marconi.Sidechain.Api.Types (EpochStateIndexerEnv (EpochStateIndexerEnv), QueryExceptions (QueryError),
                                    SidechainEnv, epochStateIndexerEnvIndexer, sidechainEnvIndexers,
                                    sidechainEpochStakePoolDelegationIndexer)
import Marconi.Sidechain.Utils (writeTMVar)

-- | Bootstraps the EpochSDD query environment.
-- The module is responsible for accessing SQLite for queries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
initializeEnv
    :: IO EpochStateIndexerEnv -- ^ returns Query runtime environment
initializeEnv = EpochStateIndexerEnv <$> newEmptyTMVarIO

updateEnvState :: TMVar (State EpochStateHandle) -> State EpochStateHandle -> STM ()
updateEnvState = writeTMVar

-- | Retrieve SDD (stakepool delegation distribution) associated at the given 'EpochNo'.
-- We return an empty list if the 'EpochNo' is not found.
querySDDByEpochNo
    :: SidechainEnv -- ^ Query run time environment
    -> Word64 -- ^ Bech32 Address
    -> IO (Either QueryExceptions EpochStakePoolDelegationResult)  -- ^ Plutus address conversion error may occur
querySDDByEpochNo env epochNo = do
    -- We must stop the indexer inserts before doing the query.
    epochSddIndexer <-
        atomically
        $ tryReadTMVar
        $ env ^. sidechainEnvIndexers . sidechainEpochStakePoolDelegationIndexer . epochStateIndexerEnvIndexer
    case epochSddIndexer of
      Nothing      -> pure $ Left $ QueryError "Failed to read EpochSDD indexer"
      Just indexer -> query indexer

    where
        query indexer = do
            (SDDByEpochNoResult epochSddRows) <-
                Storable.query Storable.QEverything indexer (SDDByEpochNoQuery $ C.EpochNo epochNo)
            pure $ Right $ EpochStakePoolDelegationResult epochSddRows

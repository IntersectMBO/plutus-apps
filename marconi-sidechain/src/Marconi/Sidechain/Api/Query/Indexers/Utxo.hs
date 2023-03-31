module Marconi.Sidechain.Api.Query.Indexers.Utxo
    ( initializeEnv
    , currentSyncedPoint
    , findByAddress
    , findByBech32Address
    , findByBech32AddressAtSlot
    , reportQueryAddresses
    , Utxo.UtxoRow(..)
    , Utxo.UtxoIndexer
    , reportBech32Addresses
    , withQueryAction
    , updateEnvState
    ) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, tryReadTMVar)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, unpack)
import GHC.Word (Word64)
import Marconi.ChainIndex.Indexers.AddressDatum (StorableQuery)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult),
                                     CurrentSyncedPointResult (CurrentSyncedPointResult))
import Marconi.Sidechain.Api.Types (AddressUtxoIndexerEnv (AddressUtxoIndexerEnv),
                                    QueryExceptions (QueryError, UnexpectedQueryResult), addressUtxoIndexerEnvIndexer,
                                    addressUtxoIndexerEnvTargetAddresses)
import Marconi.Sidechain.Utils (writeTMVar)

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
initializeEnv
    :: Maybe TargetAddresses -- ^ user provided target addresses
    -> IO AddressUtxoIndexerEnv -- ^ returns Query runtime environment
initializeEnv targetAddresses = do
    ix <- newEmptyTMVarIO
    pure $ AddressUtxoIndexerEnv targetAddresses ix

-- | Query utxos by Address
--  Address conversion error from Bech32 may occur
findByAddress
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> C.AddressAny -- ^ Cardano address to query
    -> Maybe C.SlotNo -- ^ The upper slot number we want to query
    -> IO (Either QueryExceptions [Utxo.UtxoRow])
findByAddress env addr slot = withQueryAction env (Utxo.UtxoByAddress addr slot)

-- | Retrieve the current synced point of the utxo indexer
currentSyncedPoint
    :: AddressUtxoIndexerEnv
       -- ^ Query run time environment
    -> IO (Either QueryExceptions CurrentSyncedPointResult)
       -- ^ Wrong result type are unlikely but must be handled
currentSyncedPoint env = do
    indexer <- atomically
        (tryReadTMVar $ env ^. addressUtxoIndexerEnvIndexer)
    case indexer of
         Just i -> do
             res <- Storable.query Storable.QEverything i Utxo.LastSyncPoint
             case res of
                  Utxo.LastSyncPointResult cp -> pure $ Right $ CurrentSyncedPointResult cp
                  _other                      -> pure $ Left $ UnexpectedQueryResult Utxo.LastSyncPoint
         Nothing -> pure . Right $ CurrentSyncedPointResult C.ChainPointAtGenesis


-- | Retrieve Utxos associated with the given address
-- We return an empty list if no address is not found
findByBech32Address
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> Text -- ^ Bech32 Address
    -> IO (Either QueryExceptions AddressUtxoResult)  -- ^ Plutus address conversion error may occur
findByBech32Address env addressText
    = findByBech32AddressAtSlot env addressText Nothing

-- | Retrieve Utxos associated with the given address
-- We return an empty list if no address is not found
findByBech32AddressAtSlot
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> Text -- ^ Bech32 Address
    -> Maybe Word64 -- ^ Slot number to look at
    -> IO (Either QueryExceptions AddressUtxoResult) -- ^ Plutus address conversion error may occur
findByBech32AddressAtSlot env addressText slotWord
    = let

        toQueryExceptions e = QueryError (unpack  addressText <> " generated error: " <> show e)

        queryAtAddress addr = fmap AddressUtxoResult <$> findByAddress env addr (fromIntegral <$> slotWord)

    in either (pure . Left) queryAtAddress
        $ bimap toQueryExceptions C.toAddressAny
        $ C.deserialiseFromBech32 C.AsShelleyAddress addressText

-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> StorableQuery Utxo.UtxoHandle -- ^ Address and slot to query
    -> IO (Either QueryExceptions [Utxo.UtxoRow])
withQueryAction env query =
  (atomically $ tryReadTMVar $ env ^. addressUtxoIndexerEnvIndexer) >>= action
  where
    action Nothing = pure $ Right [] -- may occures at startup before marconi-chain-index gets to update the indexer
    action (Just indexer) = do
            res <- Storable.query Storable.QEverything indexer query
            pure $ case res of
                 Utxo.UtxoResult rows -> Right rows
                 _other               -> Left $ UnexpectedQueryResult query

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: AddressUtxoIndexerEnv
    -> IO [C.Address C.ShelleyAddr]
reportQueryAddresses env = pure $ maybe [] NonEmpty.toList (env ^. addressUtxoIndexerEnvTargetAddresses)

reportBech32Addresses
    :: AddressUtxoIndexerEnv
    -> [Text]
reportBech32Addresses env =
    let addrs = maybe [] NonEmpty.toList (env ^. addressUtxoIndexerEnvTargetAddresses)
     in fmap C.serialiseAddress addrs

updateEnvState :: AddressUtxoIndexerEnv -> Utxo.UtxoIndexer -> STM ()
updateEnvState (AddressUtxoIndexerEnv _ t) =  writeTMVar t

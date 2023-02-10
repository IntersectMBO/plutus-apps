module Marconi.Mamba.Api.Query.Indexers.Utxo
    ( initializeEnv
    , findByAddress
    , findByBech32Address
    , reportQueryAddresses
    , Utxo.UtxoRow(..)
    , Utxo.UtxoIndexer
    , reportBech32Addresses
    , withQueryAction
    , writeTMVar
    , writeTMVar'
    ) where
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, tryReadTMVar, tryTakeTMVar)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, unpack)

import Cardano.Api qualified as C
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable qualified as Storable
import Marconi.Mamba.Api.Types (HasIndexerEnv (uiIndexer, uiQaddresses),
                                IndexerEnv (IndexerEnv, _uiIndexer, _uiQaddresses),
                                IndexerWrapper (IndexerWrapper, unWrapUtxoIndexer), QueryExceptions (QueryError),
                                UtxoQueryResult (UtxoQueryResult))

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
initializeEnv
    :: Maybe TargetAddresses      -- ^ user provided target addresses
    -> IO IndexerEnv        -- ^ returns Query runtime environment
initializeEnv targetAddresses = do
    ix <- atomically (newEmptyTMVar :: STM (TMVar Utxo.UtxoIndexer) )
    pure $ IndexerEnv
        { _uiIndexer = IndexerWrapper ix
        , _uiQaddresses = targetAddresses
        }

-- | Query utxos by Address
--  Address conversion error from Bech32 may occur
findByAddress
    :: IndexerEnv           -- ^ Query run time environment
    -> C.AddressAny         -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
findByAddress  = withQueryAction

-- | Retrieve Utxos associated with the given address
-- We return an empty list if no address is found
findByBech32Address
    :: IndexerEnv -- ^ Query run time environment
    -> Text -- ^ Bech32 Address
    -> IO (Either QueryExceptions UtxoQueryResult)  -- ^ Plutus address conversion error may occur
findByBech32Address env addressText =
    let
        f :: Either C.Bech32DecodeError (C.Address C.ShelleyAddr) -> IO (Either QueryExceptions UtxoQueryResult)
        f (Right address) =
            (pure . C.toAddressAny $ address)
                >>= findByAddress env
                <&> Right . UtxoQueryResult addressText
        f (Left e) = pure . Left
                   $ QueryError (unpack  addressText <> " generated error: " <> show e)
    in
        f $ C.deserialiseFromBech32 C.AsShelleyAddress addressText

-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: IndexerEnv       -- ^ Query run time environment
    -> C.AddressAny     -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
withQueryAction env address =
  (atomically . tryReadTMVar . unWrapUtxoIndexer $ env ^. uiIndexer) >>= action
  where
    action Nothing = pure [] -- may occures at startup before marconi-chain-index gets to update the indexer
    action (Just indexer) = do
            Utxo.UtxoResult rows <- Storable.query Storable.QEverything indexer (Utxo.UtxoAddress address)
            pure rows

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: IndexerEnv
    -> IO [C.Address C.ShelleyAddr]
reportQueryAddresses env = pure $ maybe [] NonEmpty.toList (env ^. uiQaddresses)

reportBech32Addresses
    :: IndexerEnv
    -> [Text]
reportBech32Addresses env =
    let addrs = maybe [] NonEmpty.toList (env ^. uiQaddresses)
     in fmap C.serialiseAddress addrs

-- | Non-blocking write of a new value to a 'TMVar'
-- Puts if empty. Replaces if populated.
--
-- Only exists in GHC9, but were on GHC8.
-- TODO: Remove once we migrate to GHC9.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new

writeTMVar' :: IndexerWrapper -> Utxo.UtxoIndexer -> STM ()
writeTMVar' (IndexerWrapper t) =  writeTMVar t

module Marconi.Sidechain.Api.Query.Indexers.Utxo
    ( initializeEnv
    , findByAddress
    , findByBech32Address
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
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, unpack)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult))
import Marconi.Sidechain.Api.Types (AddressUtxoIndexerEnv (AddressUtxoIndexerEnv), QueryExceptions (QueryError),
                                    addressUtxoIndexerEnvIndexer, addressUtxoIndexerEnvTargetAddresses)
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
    :: AddressUtxoIndexerEnv           -- ^ Query run time environment
    -> C.AddressAny         -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
findByAddress = withQueryAction

-- | Retrieve Utxos associated with the given address
-- We return an empty list if no address is not found
findByBech32Address
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> Text -- ^ Bech32 Address
    -> IO (Either QueryExceptions AddressUtxoResult)  -- ^ Plutus address conversion error may occur
findByBech32Address env addressText =
    let
        f :: Either C.Bech32DecodeError (C.Address C.ShelleyAddr) -> IO (Either QueryExceptions AddressUtxoResult)
        f (Right address) =
            (pure . C.toAddressAny $ address)
                >>= findByAddress env
                <&> Right . AddressUtxoResult
        f (Left e) = pure . Left
                   $ QueryError (unpack  addressText <> " generated error: " <> show e)
    in
        f $ C.deserialiseFromBech32 C.AsShelleyAddress addressText

-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> C.AddressAny     -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
withQueryAction env address =
  (atomically $ tryReadTMVar $ env ^. addressUtxoIndexerEnvIndexer) >>= action
  where
    action Nothing = pure [] -- may occures at startup before marconi-chain-index gets to update the indexer
    action (Just indexer) = do
            Utxo.UtxoResult rows <- Storable.query Storable.QEverything indexer (Utxo.UtxoAddress address)
            pure rows

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

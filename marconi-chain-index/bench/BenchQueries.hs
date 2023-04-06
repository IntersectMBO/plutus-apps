{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Running the benchmarks assumes that you can a synced local Cardano node and a synced
`marconi-chain-index` executable. For now, if you don't have a fully synced `marconi-chain-index`
executable, then this benchmark will probably fail with SQLBusy failures.

The benchmark will start by continuing the syncing of the Marconi indexers, and once fully synced,
it will run the benchmark suite.

Example command on how to run the benchmarks:

@
    MARCONI_DB_DIRECTORY_PATH=/home/username/cardano-node/preprod/marconi-chain-index CARDANO_NODE_SOCKET_PATH=/home/username/cardano-node/preprod/cardano-node.socket cabal bench marconi-chain-index
@

You can also run the benchmarks to estimate and report memory usage:

@
    MARCONI_DB_DIRECTORY_PATH=/home/kolam/cardano-node/preprod/marconi-chain-index CARDANO_NODE_SOCKET_PATH=/home/kolam/cardano-node/preprod/cardano-node.socket cabal bench --benchmark-options '+RTS -T' marconi-chain-index
@
-}
module Main (main) where

import Cardano.Api qualified as C
import Cardano.Chain.Slotting (EpochSlots (EpochSlots))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (STM, TMVar, atomically, newEmptyTMVar, putTMVar, readTMVar, tryTakeTMVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple (FromRow (fromRow), field, toRow)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers (runIndexers, utxoWorker)
import Marconi.ChainIndex.Indexers.Utxo (StorableQuery (UtxoByAddress), StorableResult (UtxoResult, getUtxoResult),
                                         UtxoHandle, UtxoIndexer)
import Marconi.Core.Storable (QueryInterval (QEverything))
import Marconi.Core.Storable qualified as Storable
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)
import Text.RawString.QQ (r)

data FrequencyRow a = FrequencyRow
    { _frequencyRowValue :: !a
    , _frequencyRowFreq  :: !Int
    } deriving (Generic)

instance (FromField a) => FromRow (FrequencyRow a) where
    fromRow = FrequencyRow <$> field <*> field

instance (ToField a) => ToRow (FrequencyRow a) where
  toRow (FrequencyRow ad f) = toRow (ad, f)

utxoDbFileName :: String
utxoDbFileName = "utxo.db"

main :: IO ()
main = do
    -- TODO Need a better way to pass those params in the benchmark.
    -- Look into "Custom command-line options" in the tasty-bench README.
    nodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH"
    databaseDir <- getEnv "MARCONI_DB_DIRECTORY_PATH"

    indexerTVar <- atomically (newEmptyTMVar :: STM (TMVar UtxoIndexer) )

    -- Run concurrently the indexing and the testing. The testing is only run once the indexing is
    -- fully synced with the local running node.
    race_ (runIndexerSyncing databaseDir nodeSocketPath indexerTVar) $ do
        putStrLn "Waiting for indexer to be fully synced..."
        waitUntilSynced databaseDir nodeSocketPath
        putStrLn "Finished syncing!"
        tests databaseDir indexerTVar

-- | Run the IO code which syncs the Marconi Utxo indexer with the local running Cardano node.
runIndexerSyncing
    :: FilePath
    -- ^ Marconi indexer database directory
    -> FilePath
    -- ^ Local node socket file path
    -> TMVar UtxoIndexer
    -> IO ()
runIndexerSyncing databaseDir nodeSocketPath indexerTVar = do
    let callbackUtxoIndexer :: UtxoIndexer -> IO ()
        callbackUtxoIndexer utxoIndexer = atomically $ writeTMVar indexerTVar utxoIndexer
    let indexers =
          [ ( utxoWorker callbackUtxoIndexer Nothing
            , Just $ databaseDir </> utxoDbFileName
            )
          ]
    runIndexers
        nodeSocketPath
        (C.Testnet $ C.NetworkMagic 1) -- TODO Needs to be passed a CLI param
        C.ChainPointAtGenesis
        "marconi"
        indexers

tests
    :: FilePath
    -- ^ Marconi indexers database directory path
    -> TMVar UtxoIndexer
    -> IO ()
tests databaseDir indexerTVar = do
    utxoIndexer <- atomically $ readTMVar indexerTVar

    c <- SQL.open $ databaseDir </> utxoDbFileName

    (addressesWithMostUtxos :: [FrequencyRow C.AddressAny]) <- SQL.query c
        [r|SELECT address, COUNT(address) as frequency
           FROM unspent_transactions u
           LEFT JOIN spent s
           ON u.txId = s.txInTxId
             AND u.txIx = s.txInTxIx
           GROUP BY address
           ORDER BY frequency DESC
           LIMIT 1|] ()
    let (FrequencyRow addressWithMostUtxos _) =
            fromMaybe
                (error "There is no unspent transaction outputs in the Utxo indexer database")
                $ listToMaybe addressesWithMostUtxos

    let fetchUtxoOfAddressWithMostUtxos =
            Storable.query @UtxoHandle
                QEverything
                utxoIndexer
                (UtxoByAddress addressWithMostUtxos Nothing)

    let countRows = \case
            UtxoResult rows -> length rows
            _other          -> 0

    noUtxos <- fmap countRows fetchUtxoOfAddressWithMostUtxos
    putStrLn
        $ "Address "
       <> Text.unpack (C.serialiseAddress addressWithMostUtxos)
       <> " has the most UTXOs for a total of "
       <> show noUtxos

    defaultMain
        [ bgroup "UTXO indexer query performance"
            [ bench "Query address with most utxos and get result size" $
                nfIO (fmap (length . getUtxoResult) fetchUtxoOfAddressWithMostUtxos)
            , bench "Query address with most utxos and call 'show' on the result" $
                nfIO (fmap (fmap show . getUtxoResult) fetchUtxoOfAddressWithMostUtxos)
            , bench "Query address with most utxos and encode result in JSON" $
                nfIO (fmap (fmap Aeson.encode . getUtxoResult) fetchUtxoOfAddressWithMostUtxos)
            , bench "Query address with most utxos and JSON encode/decode roundtrip the result" $
                nfIO (fmap (encodeDecodeRoundTrip . getUtxoResult) fetchUtxoOfAddressWithMostUtxos)
            ]
        ]

-- | Queries the latest slot of the UTXO indexer, compares it to the current slot of the local node
-- and loops indefinitely until the UTXO indexer is synced (or close enough) to the local node.
waitUntilSynced
    :: FilePath
    -- ^ Marconi indexers database directory path
    -> FilePath
    -- ^ Node socket file path.
    -> IO ()
waitUntilSynced databaseDir nodeSocketPath = do
    c <- SQL.open $ databaseDir </> "utxo.db"
    go c
 where
     go c = do
        threadDelay 10_000_000
        C.ChainTip (C.SlotNo currentNodeSlot) _ _ <-
            C.getLocalChainTip $ C.LocalNodeConnectInfo
                { C.localConsensusModeParams = C.CardanoModeParams (EpochSlots 21600)
                , C.localNodeNetworkId = C.Testnet $ C.NetworkMagic 1 -- TODO This should be provded as a CLI param
                , C.localNodeSocketPath = nodeSocketPath
                }
        -- TODO This should change. We should not query the slotNo with SQLite directly, because:
        --   * we're getting "SQLite3 returned ErrorBusy"
        --   * we can't currently query the data that is stored in memory
        -- Instead, we should add a new query on the UTXO indexer which returns the current query.
        sns <- SQL.query c
                [r|SELECT slotNo
                   FROM unspent_transactions
                   ORDER BY slotNo DESC
                   LIMIT 1|] () :: IO [[Word64]]
        let maybeCurrentSyncedSlot = listToMaybe =<< listToMaybe sns
        case maybeCurrentSyncedSlot of
          Nothing                                                                -> go c
          Just currentSyncedSlot | currentNodeSlot - currentSyncedSlot < 100_000 -> pure ()
          Just _                                                                 -> go c

encodeDecodeRoundTrip
    :: forall a. (ToJSON a, FromJSON a)
    => a
    -> Maybe ByteString
encodeDecodeRoundTrip v = fmap Aeson.encode $ Aeson.decode @a $ Aeson.encode v

-- | Non-blocking write of a new value to a 'TMVar'
-- Puts if empty. Replaces if populated.
--
-- Only exists in GHC9, but we're on GHC8.
-- TODO: Remove once we migrate to GHC9.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new

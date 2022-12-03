{-
-- Sample JSON-RPC server program
--
-- Often we need to test the JSON-RPC http server without the cermony of marconi, or marconi mamba.
-- The purpose of this exampl JSON-RPC server is to test the cold-store, SQLite, flow.
-- The assumption is that at some point in the past marconi had been executed and there is SQLite databse available
-- The server uses CLI parameters to connect to SQLite
-- See `start-json-rpc-server.sh` for detail
-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically, putTMVar)
import Control.Lens.Operators ((^.))
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, short, strOption, (<**>))

import Marconi.Api.Types (DBQueryEnv, HasDBQueryEnv (queryTMVar), HasJsonRpcEnv (queryEnv), TargetAddresses,
                          unUtxoIndex)
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc)
import Marconi.CLI (multiString)
import Marconi.Index.Utxos (Depth (Depth), open)


data CliOptions = CliOptions
    { _utxoPath  :: FilePath -- ^ path to utxo sqlite database
    , _addresses :: TargetAddresses
    }

cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "FILENAME"
                              <> help "Path to the marconi SQLite database.")
     <*> multiString (long "addresses-to-index"
                        <> help ("Becch32 Shelley addresses to index."
                                 <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\"" ) )

main :: IO ()
main = do
    (CliOptions dbpath addresses) <- execParser $ info (cliParser <**> helper) mempty
    putStrLn $ "Starting the Example RPC http-server:"
        <>"\nport =" <> show (3000 :: Int)
        <> "\nmarconi-db-dir =" <> dbpath
        <> "\nnumber of addresses to index = " <> show (length addresses)
    env <- bootstrapJsonRpc Nothing addresses
    race_ (bootstrapHttp env) (mocUtxoIndexer dbpath (env ^. queryEnv) )

-- | moc marconi utxo indexer.
-- This will allow us to use the UtxoIndexer query interface without having cardano-node or marconi online
-- Effectively we are going to query SQLite only
mocUtxoIndexer :: FilePath -> DBQueryEnv -> IO ()
mocUtxoIndexer dbpath env =
        open dbpath (Depth 4) >>= atomically . (putTMVar utxoIndexer) >> innerLoop
    where
        utxoIndexer = unUtxoIndex $ env ^. queryTMVar
        innerLoop = threadDelay 1000000 >> innerLoop -- create some latency

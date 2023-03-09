{-|
-- Sample JSON-RPC server program
--
-- Often we need to test the JSON-RPC http server without the cermony of marconi, or marconi
-- sidechain.
-- The purpose of this exampl JSON-RPC server is to test the cold-store, SQLite, flow.
-- The assumption is that at some point in the past marconi had been executed and there is SQLite
-- databse available.
-- The server uses CLI parameters to connect to SQLite.
-- See `start-json-rpc-server.sh` for detail
-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Marconi.ChainIndex.CLI (multiString)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Sidechain.Api.HttpServer (bootstrap)
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as UIQ
import Marconi.Sidechain.Api.Types (SidechainEnv, sidechainAddressUtxoIndexer, sidechainEnvIndexers)
import Marconi.Sidechain.Bootstrap (initializeSidechainEnv)
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, optional, short, strOption, (<**>))
import System.FilePath ((</>))

data CliOptions = CliOptions
    { _utxoDirPath :: !FilePath -- ^ Filepath to utxo sqlite database
    , _addresses   :: !(Maybe TargetAddresses)
    }

utxoDbFileName :: String
utxoDbFileName = "utxo.db"
cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "PATH"
                              <> help "directory path to the utxo SQLite database.")
     <*> (optional . multiString)
            ( long "addresses-to-index"
           <> help ( "Bech32 Shelley addresses to index."
                  <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\""
                   )
            )

main :: IO ()
main = do
    (CliOptions dbDirPath addresses) <- execParser $ info (cliParser <**> helper) mempty
    let dbpath = dbDirPath </> utxoDbFileName
    putStrLn $ "Starting the Example RPC http-server:"
        <>"\nport =" <> show (3000 :: Int)
        <> "\nmarconi-db-dir =" <> dbpath
        <> "\nnumber of addresses to index = " <> show (length <$> addresses)
    env <- initializeSidechainEnv Nothing addresses
    race_ (bootstrap env) (mocUtxoIndexer dbpath env)

-- | moc marconi utxo indexer.
-- This will allow us to use the UtxoIndexer query interface without having cardano-node or marconi online
-- Effectively we are going to query SQLite only
mocUtxoIndexer :: FilePath -> SidechainEnv -> IO ()
mocUtxoIndexer dbpath env =
        Utxo.open dbpath (Utxo.Depth 4) True >>= callback >> innerLoop
    where
      callback :: Utxo.UtxoIndexer -> IO ()
      callback = atomically . UIQ.updateEnvState (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
      innerLoop = threadDelay 1000000 >> innerLoop -- create some latency

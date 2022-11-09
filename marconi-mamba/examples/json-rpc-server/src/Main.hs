{-
-- Sample JSON-RPC server program
--
-}
module Main where

import Cardano.Api (NetworkId (Mainnet))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically, putTMVar, takeTMVar, tryReadTMVar)
import Control.Exception (bracket_)
import Control.Lens.Operators ((^.))
import Control.Monad (unless)
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, short, showDefault, strOption, value,
                            (<**>))

import Marconi.Api.Types (DBQueryEnv, HasDBQueryEnv (queryComm), HasJsonRpcEnv (queryEnv),
                          HasUtxoQueryComm (indexer, queryReq), TargetAddresses)
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc)
import Marconi.CLI (multiString)
import Marconi.Index.Utxo (Depth (Depth), open)


defaultDbpath :: FilePath
defaultDbpath = "./.marconidb/4/utxo-db"

data CliOptions = CliOptions
    { _utxoPath  :: FilePath -- ^ path to utxo sqlite database
    , _addresses :: TargetAddresses
    }

cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "FILENAME"
                              <>  showDefault
                              <> value defaultDbpath
                              <> help "Path to the utxo database.")
     <*> multiString (long "addresses-to-index"
                        <> help ("Becch32 Shelley addresses to index."
                                 <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\"" ) )

main :: IO ()
main = do
    (CliOptions dbpath addresses) <- execParser $ info (cliParser <**> helper) mempty
    putStrLn $ "Starting the Example RPC http-server:"
        <>"\nport =" <> show (3000 :: Int)
        <> "\nutxo-db =" <> dbpath
        <> "\nnumber of addresses to index = " <> show (length addresses)

    env <- bootstrapJsonRpc dbpath Nothing addresses Mainnet
    race_ (bootstrapHttp env) (mocUtxoIndexer (env ^. queryEnv) )

mocUtxoIndexer :: DBQueryEnv -> IO ()
mocUtxoIndexer env = open "" (Depth 4) >>= innerLoop
    where
        utxoIndexer = env ^. queryComm . indexer
        qreq = env ^. queryComm . queryReq
        innerLoop ix = do
            isquery <-  atomically $ tryReadTMVar qreq
            unless (null isquery) $ bracket_
                (atomically (takeTMVar qreq ) )
                (atomically (takeTMVar qreq ) )
                (atomically  (putTMVar utxoIndexer ix) )

            threadDelay 10  --  inserts to sqlite
            (innerLoop ix)

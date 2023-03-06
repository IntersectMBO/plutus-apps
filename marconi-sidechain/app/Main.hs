{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Concurrent.Async (race_)
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs, httpPort, targetAddresses))
import Marconi.Sidechain.Bootstrap (bootstrapHttp, bootstrapIndexers, initializeIndexerEnv)
import Marconi.Sidechain.CLI (parseCli)

-- | Concurrently start:
--
-- * JSON-RPC server
-- * marconi indexer workers
--
-- Exceptions in either thread will end the program
main :: IO ()
main = do
    cli@CliArgs { httpPort, targetAddresses } <- parseCli

    rpcEnv <- initializeIndexerEnv httpPort targetAddresses

    race_
       (bootstrapHttp rpcEnv) -- Start HTTP server
       (bootstrapIndexers cli rpcEnv) -- Start the Sidechain indexers

{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Concurrent.Async (race_)
import Marconi.Mamba.Api.Types (CliArgs (CliArgs, httpPort, targetAddresses))
import Marconi.Mamba.Bootstrap (bootstrapHttp, bootstrapIndexers, initializeIndexerEnv)
import Marconi.Mamba.CLI (parseCli)

-- | Concurrently start:
--
-- * JSON-RPC server
-- * marconi indexer workers
--
-- Exceptions in either thread will end the program
main :: IO ()
main = do
    cli@CliArgs { httpPort, targetAddresses }  <- parseCli
    rpcEnv <- initializeIndexerEnv httpPort targetAddresses

    race_
       (bootstrapHttp rpcEnv) -- Start HTTP server
       (bootstrapIndexers cli rpcEnv) -- Start the Mamba indexers

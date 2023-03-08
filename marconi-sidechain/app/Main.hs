{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Concurrent.Async (race_)
import Marconi.Sidechain.Api.HttpServer qualified as Http
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs, httpPort, targetAddresses))
import Marconi.Sidechain.Bootstrap (bootstrapIndexers, initializeSidechainEnv)
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
    rpcEnv <- initializeSidechainEnv httpPort targetAddresses

    race_
       (Http.bootstrap rpcEnv) -- Start HTTP server
       (bootstrapIndexers cli rpcEnv) -- Start the Sidechain indexers

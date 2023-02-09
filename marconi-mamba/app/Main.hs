module Main where

import Control.Concurrent.Async (race_)
import Marconi.Mamba.Api.Types (CliArgs (CliArgs))
import Marconi.Mamba.Bootstrap (bootstrapHttp, bootstrapIndexers, initializeIndexerEnv)
import Marconi.Mamba.CLI (parseCli)

-- | concurrently start:
-- JSON-RPC server
-- marconi utxo worker
-- Exceptions in either thread will end the program
--
main :: IO ()
main = do
    cli@(CliArgs _ _ maybePort _ tAddress)  <- parseCli
    rpcEnv <- initializeIndexerEnv maybePort tAddress

    race_
       (bootstrapHttp rpcEnv)                            -- start http server
       (bootstrapIndexers cli rpcEnv)

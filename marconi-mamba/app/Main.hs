module Main where

import Control.Concurrent.Async (race_)
import Marconi.Api.Types (CliArgs (CliArgs))
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc, bootstrapUtxoIndexers)
import Marconi.MambaCli (parseCli)

-- | concurrently start:
-- JSON-RPC server
-- marconi utxo worker
-- Exceptions in either thread will end the program
--
main :: IO ()
main = do
    cli@(CliArgs _ _ maybePort _ tAddress)  <- parseCli
    rpcEnv <- bootstrapJsonRpc maybePort tAddress
    race_
       (bootstrapHttp rpcEnv)                            -- start http server
       (bootstrapUtxoIndexers cli rpcEnv)

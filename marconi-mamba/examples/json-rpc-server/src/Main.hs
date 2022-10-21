{-
-- Sample JSON-RPC server program
--  uncomment TODO and provide adequte data
-}
module Main where

import Marconi.Api.Types (TargetAddresses)
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc, targetAddressParser)

{-
-- white space separated list of addresses
-}

-- TODO
bech32Addresses :: String   -- ^  valid address to keep track of
bech32Addresses = "addr1w9645geguy679dvy73mgt6rvc4xyhjpxj4s0wxjtd6swvdc5dxgc3"
-- TODO
dbpath :: FilePath -- ^ valid SQLite marconi UTxo database path
dbpath = "./.marconidb/utxodb"

addresses :: TargetAddresses
addresses = targetAddressParser bech32Addresses

main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 3000 example"
    env <- bootstrapJsonRpc dbpath Nothing addresses
    bootstrapHttp env

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
bech32Addresses :: String
bech32Addresses = undefined -- TODO valid address to keep track of

dbpath :: FilePath
dbpath = undefined -- valid SQLite marconi UTxo database path

addresses :: TargetAddresses
addresses = targetAddressParser bech32Addresses

main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 3000 example"
    env <- bootstrapJsonRpc dbpath Nothing addresses
    bootstrapHttp env

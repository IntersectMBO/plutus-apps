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
bech32Addresses = "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"

-- TODO
dbpath :: FilePath -- ^ valid SQLite marconi UTxo database path
dbpath = "./utxoDB"

addresses :: TargetAddresses
addresses = targetAddressParser bech32Addresses

main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 3000 example"
    env <- bootstrapJsonRpc dbpath Nothing addresses
    bootstrapHttp env


module Main where
import Cardano.Api qualified as CApi
import Marconi.Api.Types (TargetAddresses)
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo
import Marconi.Bootstrap (targetAddressParser)
import Marconi.DB.SqlUtils (freqShelleyTable, freqUtxoTable)

bech32Addresses :: String   -- ^  valid address to keep track of
bech32Addresses = "addr1w9645geguy679dvy73mgt6rvc4xyhjpxj4s0wxjtd6swvdc5dxgc3"
-- TODO
dbpath :: FilePath -- ^ valid SQLite marconi UTxo database path
dbpath = "./.marconidb/utxodb"

fakeAddresses :: TargetAddresses
fakeAddresses = targetAddressParser bech32Addresses

mainNet :: CApi.NetworkId
mainNet = CApi.Mainnet


main :: IO ()
main  = do
    dbEnv <- Q.Utxo.bootstrap dbpath fakeAddresses mainNet
    freqUtxoTable dbEnv
    as <- freqShelleyTable dbEnv
    print (length as)

    print "end"

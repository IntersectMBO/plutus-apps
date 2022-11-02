-- | Light exe wrapper for SqlUtils
--
module Main where
import Cardano.Api (NetworkId)
import Marconi.Api.Types (TargetAddresses)
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo
import Marconi.Bootstrap (targetAddressParser)
import Marconi.CLI (pNetworkId)
import Marconi.DB.SqlUtils (freqShelleyTable, freqUtxoTable)
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, short, showDefault, strOption, value,
                            (<**>))

data CliOptions = CliOptions
    { utxoPath  :: FilePath -- ^ path to utxo sqlite database
    , networkId :: NetworkId  -- ^ network id used for address conversions
    }

-- |
bech32Addresses :: String   -- ^  valid address to keep track of
bech32Addresses = "addr1w9645geguy679dvy73mgt6rvc4xyhjpxj4s0wxjtd6swvdc5dxgc3"

cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "FILENAME"
                              <>  showDefault
                              <> value "./.marconidb/2/utxo-db"
                              <> help "Path to the utxo database.")
    <*> pNetworkId

fakeAddresses :: TargetAddresses
fakeAddresses = targetAddressParser bech32Addresses

main :: IO ()
main  = do
    (CliOptions dbpath networkid) <- execParser $ info (cliParser <**> helper) mempty
    dbEnv <- Q.Utxo.bootstrap dbpath fakeAddresses networkid
    freqUtxoTable dbEnv
    as <- freqShelleyTable dbEnv
    print (length as)

    print "end"

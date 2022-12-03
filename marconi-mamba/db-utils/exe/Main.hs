-- | Light exe wrapper for SqlUtils
--
module Main where
import Cardano.Api (NetworkId)
import Data.List.NonEmpty (fromList)
import Marconi.Api.Types (TargetAddresses)
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo
import Marconi.CLI (pNetworkId, parseCardanoAddresses)
import Marconi.DB.SqlUtils (DBEnv, bootstrap, freqShelleyTable, freqUtxoTable)
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, short, showDefault, strOption, value,
                            (<**>))

data CliOptions = CliOptions
    { utxoPath  :: FilePath -- ^ path to utxo sqlite database
    }

-- |

cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "FILENAME"
                              <>  showDefault
                              <> value "./.marconidb/4/utxo-db"
                              <> help "Path to the marconi database.")
main :: IO ()
main  = do
    (CliOptions dbpath) <- execParser $ info (cliParser <**> helper) mempty
    dbEnv <- bootstrap dbpath
    freqUtxoTable dbEnv
    as <- freqShelleyTable dbEnv
    print (length as)
    putStrLn "end"

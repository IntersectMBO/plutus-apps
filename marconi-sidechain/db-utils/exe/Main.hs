-- | Light exe wrapper for SqlUtils
--
module Main where
import Marconi.DB.SqlUtils (bootstrap, freqShelleyTable, freqUtxoTable)
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, short, showDefault, strOption, value,
                            (<**>))

newtype CliOptions = CliOptions
    { utxoPath  :: FilePath -- ^ path to utxo sqlite database
    }

cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "FILENAME"
                              <>  showDefault
                              <> value "./.marconidb/utxo.db"
                              <> help "Path to the marconi database.")
main :: IO ()
main = do
    (CliOptions dbpath) <- execParser $ info (cliParser <**> helper) mempty
    dbEnv <- bootstrap dbpath
    freqUtxoTable dbEnv
    as <- freqShelleyTable dbEnv
    print (length as)
    putStrLn "end"

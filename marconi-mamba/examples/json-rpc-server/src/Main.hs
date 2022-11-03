{-
-- Sample JSON-RPC server program
--
-}
module Main where

import Cardano.Api (NetworkId (Mainnet))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically, putTMVar, takeTMVar, tryReadTMVar)
import Control.Exception (bracket_)
import Control.Lens.Operators ((^.))
import Control.Monad (unless)
import Data.List (intercalate)
import Marconi.Api.Types (DBQueryEnv, HasDBQueryEnv (queryComm), HasJsonRpcEnv (queryEnv),
                          HasUtxoQueryComm (indexer, queryReq), TargetAddresses)
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc, targetAddressParser)
import Marconi.Index.Utxo (Depth (Depth), open)
import Options.Applicative (Parser, execParser, help, helper, info, long, metavar, short, showDefault, strOption, value,
                            (<**>))

bech32DefaultAddresses :: String   -- ^  valid address to keep track of
bech32DefaultAddresses = intercalate " "
    ["addr1q87cwj26ftt8ucu52sw9ravms9h7tuwlzg7pyf8f0ln8xxjylayug87f0fumqe0hkm7hffgczcu68vg7fvntlntdu9esg5dupm"
    , "addr1v8v3auqmw0eszza3ww29ea2pwftuqrqqyu26zvzjq9dt2ncydzvs5"
    , "addr1vypr00ss7hkqejmvh53xkyf0p9q0a4z2uprxmx6njc463vgst3pe4"
    , "addr1v95sf69jcfhnmknvffwmfvlvnccatqwfjcyh0nlfc6gh5scta2yzg"
    , "addr1qxq47au29wss4g8acjk0zsmwwq0h34hhzump6stye9wuldm7nm0t6ad3jz9hy5v3smye0nvcumtzu43k7r36ag0w29qqdafvvk"
    , "addr1v9u7va2sktlnz8dp3qadpnxxlv4m03672jy6elmntlxxs7q9nnwl9"
    , "addr1vx68aey37n4t73yygc2uykhhxr75eml9nzgsrzxd8wmxcugvyu3rs"
    , "addr1qx52knza2h5x090n4a5r7yraz3pwcamk9ppvuh7e26nfks7pnmhxqavtqy02zezklh27jt9r6z62sav3mugappdc7xnskxy2pn"
    , "addr1v9s594wrqy5an9rerjtpcu9r0cua278pa5wwv7p2m88j9pc86r7af"
    , "addr1v8hzad0cqqxmklk9ckea0sxmfgzpul2anmypacycvh6l3hsstd0rs"
    , "addr1q8fwxda8mq09rzjcluxrep9wyrnfe4e8kuzqzkvcdn53vskmspx9w7e55ly30vqnw4a0eza3hj2hxnhhq6hqqh58gyvstww5ww"
    , "addr1q8nkg0kqurdd2z8mhv4wd69wqg64uzkhk94rx7vgp5f0vk3wt6qwfvd5z9tdjl5kzwzr2dgjkcvehhmjhhkxca8d9dese2kq4k"
    , "addr1q87cwj26ftt8ucu52sw9ravms9h7tuwlzg7pyf8f0ln8xxjylayug87f0fumqe0hkm7hffgczcu68vg7fvntlntdu9esg5dupm"
    , "addr1vxrmu3m2cc5k6xltupj86a2uzcuq8r4nhznrhfq0pkwl4hgqj2v8w"
    , "addr1vxcnz5w5ccv3nwerjm944hl6hjzdc6axm893c88a9jcsrpsd22m4p"
    , "addr1vx3kpf4s99mp8xyaac8vdh9kq9vmchde4f60fssuvywcapcm8ygjh"
    , "addr1q9myqjadnj2q9fzmlg2dw8l92uegyct6h3wvnas28duuzsz5w4as46vtmnm65rc7sfzt0dg0cv5pjlnk2eh62drvqm3su639ap"
    , "addr1qxt2ggq005kfm3uwe89emy3ka2zgdtrpxfarvz6033l3fqvk5ssq7lfvnhrcajwtnkfrd65ys6kxzvn6xc95lrrlzjqsjttk32"
    , "addr1v9a5uwes85fchd2mdzraqc8ry87py00gzpqug7nrf5ch28qtag0e4"
    , "addr1vxpzqpz8mzat8qr9ukcvcgtp5x27zk5wgcqf9yh9d3fq25gtsnye2"
    , "addr1qy807crqvtpr0qq0ccvptgsvfvpaul2x3ae4vxlgcegrwgswlasxqckzx7qql3sczk3qcjcrme75drmn2cd733jsxu3qa04mne"
    ]

defaultDbpath :: FilePath
defaultDbpath = "./.marconidb/2/utxo-db"

data CliOptions = CliOptions
    { _utxoPath  :: FilePath -- ^ path to utxo sqlite database
    , _addresses :: TargetAddresses
    }

cliParser :: Parser CliOptions
cliParser = CliOptions
    <$> strOption (long "utxo-db"
                              <> short 'd'
                              <> metavar "FILENAME"
                              <>  showDefault
                              <> value defaultDbpath
                              <> help "Path to the utxo database.")
    <*> pAddressesParser

pAddressesParser :: Parser TargetAddresses
pAddressesParser = targetAddressParser <$> strOption
    (long "addresses-to-index"
     <> metavar "Address"
     <>  showDefault
     <> value bech32DefaultAddresses
     <> help ("White space separated list of addresses to index."
                  <>  " i.e \"address-1 address-2 address-3 ...\"" ) )

main :: IO ()
main = do
    (CliOptions dbpath addresses) <- execParser $ info (cliParser <**> helper) mempty
    putStrLn $ "Starting the Example RPC http-server:"
        <>"\nport =" <> show (3000 :: Int)
        <> "\nutxo-db =" <> dbpath
        <> "\nnumber of addresses to index = " <> show (length addresses)

    env <- bootstrapJsonRpc dbpath Nothing addresses Mainnet
    race_ (bootstrapHttp env) (mocUtxoIndexer (env ^. queryEnv) )

mocUtxoIndexer :: DBQueryEnv -> IO ()
mocUtxoIndexer env = open "" (Depth 4) >>= innerLoop
    where
        utxoIndexer = env ^. queryComm . indexer
        qreq = env ^. queryComm . queryReq
        innerLoop ix = do
            isquery <-  atomically $ tryReadTMVar qreq
            unless (null isquery) $ bracket_
                (atomically (takeTMVar qreq ) )
                (atomically (takeTMVar qreq ) )
                (atomically  (putTMVar utxoIndexer ix) )

            threadDelay 10  --  inserts to sqlite
            (innerLoop ix)

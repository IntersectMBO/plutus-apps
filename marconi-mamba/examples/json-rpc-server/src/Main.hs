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
    [
         "addr1q87cwj26ftt8ucu52sw9ravms9h7tuwlzg7pyf8f0ln8xxjylayug87f0fumqe0hkm7hffgczcu68vg7fvntlntdu9esg5dupm"
        , "addr1v9u7va2sktlnz8dp3qadpnxxlv4m03672jy6elmntlxxs7q9nnwl9"
        , "addr1vypr00ss7hkqejmvh53xkyf0p9q0a4z2uprxmx6njc463vgst3pe4"
        , "addr1v8v3auqmw0eszza3ww29ea2pwftuqrqqyu26zvzjq9dt2ncydzvs5"
        , "addr1v95sf69jcfhnmknvffwmfvlvnccatqwfjcyh0nlfc6gh5scta2yzg"
        , "addr1q9h7mjfkvp45wvmguc7z52v4kglhdddrfndy4ykqxxe7ahx60hvk6wllh2h5ej5zepwmeqdj7p6mdxym8lfj37e9zncqrk8njz"
        , "addr1q8eqr3mlxtwczmw5zqt8rxf3guk7zppf69xgt6hnqulm6v95mpgr8f9n3xxh9muyyz4z9yth8jze5hfqwnmhv260xzrqnhnjc3"
        , "addr1vx68aey37n4t73yygc2uykhhxr75eml9nzgsrzxd8wmxcugvyu3rs"
        , "addr1qx5u2jke0t767lddysx6xwcayrzwd8dygthh9n0ttcd4r337azp9vqwcsreqzug9zwyrv027uve30p2x225yhf62q6ys6ryzm2"
        , "addr1qxyxudgzljnnaqghm8hlnpp36uvfr68a8k6uemumgjdcua4y7d04xcx9hnk05lnl6m9ptd9h3pj9vvg2xe4j354uh8vsarpydn"
        , "addr1q8nkg0kqurdd2z8mhv4wd69wqg64uzkhk94rx7vgp5f0vk3wt6qwfvd5z9tdjl5kzwzr2dgjkcvehhmjhhkxca8d9dese2kq4k"
        , "addr1qxq47au29wss4g8acjk0zsmwwq0h34hhzump6stye9wuldm7nm0t6ad3jz9hy5v3smye0nvcumtzu43k7r36ag0w29qqdafvvk"
        , "addr1vxpzqpz8mzat8qr9ukcvcgtp5x27zk5wgcqf9yh9d3fq25gtsnye2"
        , "addr1qxkp553nvr57m5f9ylpdpzpdlhhhn9j4c02zdfxphdwm782tjyh2rcg5u47u9ntq73u8zzpnwrjtz96ss5sks8khw6vsnvke6s"
        , "addr1vxcnz5w5ccv3nwerjm944hl6hjzdc6axm893c88a9jcsrpsd22m4p"
        , "addr1q8k565ztcxzxxlxvngp8lyg2vnkptvphjfyrkjc5k70x0vjtjyh2rcg5u47u9ntq73u8zzpnwrjtz96ss5sks8khw6vs4r0kgu"
        , "addr1q86naxdettc4r8rhls2xqs97kpkd79tvkplrv5lf8e6tfyygy9uwd947cp8mqh8kl04pazxjs9tmyystyv0nhpmc852srht88w"
        , "addr1q8pa98kn47xkctsvfw2w2cxs4j9yhf2nw5swh230f4ymqwkx5492fgr7vggzp6mxspz8n4lqvdxqz24f6ccszswtdn9s0g2an9"
        , "addr1qxgddhyma66fh206xcqy440tpufww9a78y626u3sxe065xzrmyxsnc6mpgll4vmlfj07v8wclxh6c5gx42x5uh8r96hqlmjwry"
        , "addr1q8fwxda8mq09rzjcluxrep9wyrnfe4e8kuzqzkvcdn53vskmspx9w7e55ly30vqnw4a0eza3hj2hxnhhq6hqqh58gyvstww5ww"
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

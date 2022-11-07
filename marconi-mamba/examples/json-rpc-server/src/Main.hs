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
    ["addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc"
    , "addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah"
    , "addr_test1wpzvcmq8yuqnnzerzv0u862hmc4tc8xlm74wtsqmh56tgpc3pvx0f"
    , "addr_test1vrvf7yfr2h79mtzqrpcn0ql98xrhs63k85w64u8py7709zsm6tsr6"
    -- , "addr_test1qz4ll7yrah8h5t3cv2qptn4mw22judsm9j9zychhmtuuzmszd3hm6w02uxx6h0s3qgbd4hxgpvd0qzklnmahcx7v0mcysptyj8l"
    , "addr_test1qpxtftdcvh55twn2lum4uylxshzls6489q89pft3feesq2m8an0x234jtsqra93hcefmut6cyd6cdn535nkjukhph47ql6uvg7"
    , "addr_test1wqgden0j2d7pkqy3hu6kcj32swazzy6wg93a8c46pndptncdmz6tq"
    , "addr_test1qpe6s9amgfwtu9u6lqj998vke6uncswr4dg88qqft5d7f67kfjf77qy57hqhnefcqyy7hmhsygj9j38rj984hn9r57fswc4wg0"
    , "addr_test1qp9juu544jrkel0hp6pvrjud70h9ev3ajw2fuvezyv8ug3eg3292mxuf3kq7nysjumlxjrlsfn9tp85r0l54l29x3qcsytvx58"
    , "addr_test1qqddk5xnz08mxsqw6jdaenvhdah835lhvm62tt5lydk2as7kfjf77qy57hqhnefcqyy7hmhsygj9j38rj984hn9r57fs066hcl"
    , "addr_test1qpp0xdvdexl4t3ur4svdkv3jjs2gy8fu6h9mrrsgvm7r20cmrhy4p5ukjknv23jy95nhsjsnud6fxkjqxp5ehvn8h0es2su3gt"
    , "addr_test1wz3937ykmlcaqxkf4z7stxpsfwfn4re7ncy48yu8vutcpxgnj28k0"
    , "addr_test1vqslp49gcrvah8c9vjpxm4x9j7s2m4zw0gkscqh0pqjg4wcjzvcfr"
    , "addr_test1qzu6at6s7r7yzpvw3mm2tsas34mc9nzrhda89w59wd78lfaw8084xldqyrvxe38z4wxqqdr9h86t8ruut8rrfezdftpstsnrd8"
    , "addr_test1vz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cqk95hdw"
    , "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
    , "addr_test1qqwh05w69g95065zlr4fef4yfpjkv8r3dr9h3pu0egy5n7pwhxp4f95svdjr9dmtqumqcs6v49s6pe7ap4h2nv8rcaasgrkndk"
    , "addr_test1qrr6urmwy2nle3xppnjg9xcukasrwfyfjv9eqh97km84ra53q4hau90tjeldx0mv9eka2z73t9727xl8jny3cy8zetqsdjctpd"
    , "addr_test1wrn2wfykuhswv4km08w0zcl5apmnqha0j24fa287vueknasq6t4hc"
    ,"addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4"
    , "addr_test1wr9gquc23wc7h8k4chyaad268mjft7t0c08wqertwms70sc0fvx8w"
    , "addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99"
    ]
defaultDbpath :: FilePath
defaultDbpath = "./.marconidb/4/utxo-db"

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

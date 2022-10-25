{-
-- Sample JSON-RPC server program
--  uncomment TODO and provide adequte data
-}
module Main where

import Cardano.Api (NetworkId (Mainnet))
import Data.List (intercalate)
import Marconi.Api.Types (TargetAddresses)
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc, targetAddressParser)
{-
-- white space separated list of addresses
-}

-- TODO
bech32Addresses :: String   -- ^  valid address to keep track of
bech32Addresses = intercalate " "
    [
        "addr1q837de0y7j3ncegph2a8mc0e86q9evwtekd3ejhlpr97wclrumj7fa9r83jsrw460hslj05qtjcuhnvmrn907zxtua3skv7yyl"
    ,"addr1qxyxudgzljnnaqghm8hlnpp36uvfr68a8k6uemumgjdcua4y7d04xcx9hnk05lnl6m9ptd9h3pj9vvg2xe4j354uh8vsarpydn"
    , "addr1qytr6ma495fkqpfnd7gk5kwmtfdh084xvzn7rv83ha87qq6yfm3y8yv39lcrqc6ej3zdzvef4aj3dv3pq2snakkcwscsfyrn3g"
    , "addr1qxt2ggq005kfm3uwe89emy3ka2zgdtrpxfarvz6033l3fqvk5ssq7lfvnhrcajwtnkfrd65ys6kxzvn6xc95lrrlzjqsjttk32"
    , "addr1qyhat6v7w65799pkc8ff3mjcwk79kqs8gv8t4expd67f9seqksv3earfx6skxkdhe4hcekjkj0x333dd76u8re8cmg2qwrdzn2"
    , "addr1qy807crqvtpr0qq0ccvptgsvfvpaul2x3ae4vxlgcegrwgswlasxqckzx7qql3sczk3qcjcrme75drmn2cd733jsxu3qa04mne"
    , "addr1q87sjen5fqdgkdyrmm2fcstq9mp9amfsdgvplq8wg9ev75mkent5zu88cnkuqrk933fm6mhvq0u867pmv5ysc72vf9xsh6cqr7"
    , "addr1qxzt6a7clerp5g05m74z7lvzvjnkys8f3f93akk3g4mjpk20tzw0uktgd7awa7gfqck9vq7dx7wt2fr49ufsc6r4dq7qmhajjc"
    , "addr1q8clmt8gym7j9xpx65xc6zxh244k6jufe2k4ud6rmmjvtk9c5dgkcwdv6z0vehgyclv6k07yn58hh2v7av9gnam8229qcw2zm7"
    , "addr1qya4q38x5y0rtr53n0pzpxn7fqzmrxn24v9nmftnnw8hkemtpk7llnc73nv82mk57maflppzrjl2dz03c48g05mgxx2q67fthh"
    , "addr1q8xfgstn2nj9ejt29kcft9m2waxxn2fa3wp94g0r9z575n0lavkt8havfc4wk55pyg6cakngqg0hmxe3a53lvc5q259q4p5rv9"
    , "addr1qy4fv35z7gj4ny2exnh9fjamkdy84keufzc6l9s0ekvwda6ccx4ez3q3jzz4jff98ayvv0l0nfgnlpn2l55ttlthwjmqd4453t"
    , "addr1qyreqjfnq69yvx3j6zj66dryzctk96qa54chg0xrp8uwslzykkmtykt2set490qm6dnwfvsp54mg2zh9qhqsesp5cxysay9pzn"
    , "addr1qxtrqdumg8dleqcra3myptlq6n43m8s0mver0pwgqrr8awvkkcdaz26hglgm4qvc6fdy0rr4ck6q5q249drqc4fzyrgq68vuva"
    , "addr1q8yr9x2jp24h3lhcp290jyz0cupsdr3z0mgr6wjq4gfyx4zkj6g2hck2e75datgvnvr7eahspjyf6mvqnut26dcgxusqppc2nl"
    , "addr1qxefewc9n43tyctdyk935lk5a342eus76wk2z0v0nl9wdr0z3sv430tkzngurge0u926gllc3n3vpzy6qnnpnqa0qxpsuj9gqv"
    , "addr1qxrlkh6yh0km5m5n7923syel0yqqvc3pjrnqrzrz3gwpxd70prfqwehanuxzkwmv55ff9gr7tjx5vymykd2galr9chaqlwjwm9"
    , "addr1q9dzwgq9t8pvlc7n76r7d46rrshe5s4v0gd7lmzenfxg49lrsguyk4655g488x3hzdyvwlz9zygp8aee6t2hzgc2p9rqry4rcg"
    , "addr1q9wndwc74g66amxkfwg07x0lfnl84k999u4cer67hq2gzejax6a3a2344mkdvjusluvl7n870tv22tet3j84awq5s9nq8cxcnx"
    , "addr1vy5q8pvwutswdyh047lwxs33p2yf5r6zf7e8ms6vd4tyhgg09canu"
    ]


-- TODO
dbpath :: FilePath -- ^ valid SQLite marconi UTxo database path
dbpath = "./.marconidb/utxodb"

addresses :: TargetAddresses
addresses = targetAddressParser bech32Addresses

main :: IO ()
main = do
    print (length addresses)
    putStrLn $ "Starting the Example RPC http-server on port 3000 example for "
        <> show (length addresses) <> " valid bech32 addresses\n"

    env <- bootstrapJsonRpc dbpath Nothing addresses Mainnet
    bootstrapHttp env

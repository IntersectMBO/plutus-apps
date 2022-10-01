{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Cardano.Api qualified
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified
import Marconi.Api.HttpServer (httpMain)
import Marconi.Api.Types
import Marconi.IndexerCache

main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 9000 example"
    let address:: Cardano.Api.Address Cardano.Api.ShelleyAddr
        address = undefined  -- todo construct a fake address here
    cache <- initCache (NE.fromList [address])
    let httpPort = 9000
        httpEnv = HttpEnv httpPort cache
    httpMain httpEnv


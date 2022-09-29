{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM.TVar (newTVarIO)
import Data.Map.Strict qualified
import Marconi.Server.HttpServer (httpMain)
import Marconi.Server.Types (AddressTxOutRefMap, HttpEnv (HttpEnv))


main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 9000 example"
    cache <- newTVarIO Data.Map.Strict.empty
    let httpPort = 9000
        httpEnv = HttpEnv httpPort cache
    httpMain httpEnv

mocTxOutRef :: AddressTxOutRefMap
mocTxOutRef = undefined

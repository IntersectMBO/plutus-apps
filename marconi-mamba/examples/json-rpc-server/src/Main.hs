{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where


import Marconi.Api.HttpServer (bootstrap)
import Marconi.Api.Types (JsonRpcEnv (JsonRpcEnv))
import Marconi.IndexersHotStore (bootstrapHotStore)
import Network.Wai.Handler.Warp (defaultSettings)

main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 3000 example"
    cache <- bootstrapHotStore
    let    env = JsonRpcEnv defaultSettings cache
    bootstrap env

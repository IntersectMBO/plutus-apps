{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Cardano.Api qualified
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified
import Marconi.Api.HttpServer (bootstrap)
import Marconi.Api.Types
import Marconi.IndexersHotStore
import Network.Wai.Handler.Warp (defaultSettings)

main :: IO ()
main = do
    putStrLn "Starting the Example rpc http-server on port 3000 example"
    let address:: Cardano.Api.Address Cardano.Api.ShelleyAddr
        address = undefined  -- todo construct a fake address here
    cache <- bootstrapHotStore
    let    env = JsonRpcEnv defaultSettings cache
    bootstrap env

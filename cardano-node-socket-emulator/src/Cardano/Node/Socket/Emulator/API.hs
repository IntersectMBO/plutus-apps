{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Socket.Emulator.API
    ( API
    , NodeAPI
    ) where

import Servant.API (Get, JSON, NoContent, Post, (:<|>), (:>))

import Cardano.Node.Emulator.API (EmulatorLogs)

type API
     = "healthcheck" :> Get '[JSON] NoContent
       :<|> "mock" :> NodeAPI

-- Routes that are not guaranteed to exist on the real node
type NodeAPI
     = "consume-event-history" :> Post '[JSON] EmulatorLogs

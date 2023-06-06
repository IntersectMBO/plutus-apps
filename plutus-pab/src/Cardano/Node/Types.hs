{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| This module exports data types for logging, events and configuration
-}
module Cardano.Node.Types
    (
     -- * Effects
    ChainSyncHandle

    -- * Config types
    , PABServerConfig (..)
    , NodeMode (..)
    , _MockNode
    , _AlonzoNode
    )
        where

import Cardano.Node.Socket.Emulator.Types (NodeServerConfig)
import Cardano.Protocol.Socket.Client qualified as Client
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Time.Units.Extra ()
import GHC.Generics (Generic)
import Ledger (Block)
import Prettyprinter (Pretty, pretty)

-- Configuration ------------------------------------------------------------------------------------------------------

{- Note [Slot numbers in mock node]

The mock node has an internal clock that generates new slots in a regular
interval. Slots are identified by consecutive integers. What should the
initial slot number be? We can either set it to 0, so that the slot number
is the number of intervals that have passed since the process was started.
Or we can define an initial timestamp, so that the slot number is the number
of intervals since that timestamp.

The first option of counting from 0 is useful for integration tests where we
want the test outcome to be independent of when the test was run. This approach
is used in the PAB simulator.
The second option, counting from a timestamp, is more realistic and it is
useful for frontends that need to convert the slot number back to a timestamp.
We use this approach for the "proper" pab executable.

-}

-- | Which node we're connecting to
data NodeMode =
    MockNode -- ^ Connect to the PAB mock node.
    | AlonzoNode -- ^ Connect to an Alonzo node
    | NoChainSyncEvents -- ^ Do not connect to any node for chain sync events. Connect to Alonzo node for slot notifications.
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

makePrisms ''NodeMode

-- | Node server configuration
data PABServerConfig =
    PABServerConfig
        { pscNodeServerConfig :: NodeServerConfig
        -- ^ Path to a JSON file containing the protocol parameters
        , pscPassphrase       :: Maybe Text
        -- ^ Wallet passphrase
        , pscNodeMode         :: NodeMode
        -- ^ Whether to connect to an Alonzo node or a mock node
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultPABServerConfig :: PABServerConfig
defaultPABServerConfig =
    PABServerConfig
      { pscNodeServerConfig = def
      , pscPassphrase = Nothing
      , pscNodeMode  = MockNode
      }

instance Default PABServerConfig where
  def = defaultPABServerConfig

instance Pretty PABServerConfig where
  pretty PABServerConfig{ pscNodeServerConfig } = pretty pscNodeServerConfig

-- | The types of handles varies based on the type of clients (mocked or
-- real nodes) and we need a generic way of handling either type of response.
type ChainSyncHandle = Either (Client.ChainSyncHandle Block) (Client.ChainSyncHandle Client.ChainSyncEvent)

{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module Plutus.PAB.Events.ContractInstanceState(
    PartiallyDecodedResponse(..)
    , fromResp
    , hasActiveRequests
    ) where

import Control.Monad.Freer.Extras.Log (LogMessage)
import Data.Aeson (FromJSON, ToJSON (..), Value)
import Data.Aeson.Encode.Pretty qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BS8
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text qualified as Text
import Data.Text.Extras (abbreviate)
import GHC.Generics (Generic)
import Plutus.Contract.Resumable qualified as Contract
import Plutus.Contract.State qualified as Contract
import Prettyprinter

-- TODO: Replace with type synonym for @ContractResponse Value Value Value h@
data PartiallyDecodedResponse v =
    PartiallyDecodedResponse
        { hooks           :: [Contract.Request v]
        , logs            :: [LogMessage Value]
        , lastLogs        :: [LogMessage Value] -- The log messages returned by the last step ('lastLogs' is a suffix of 'logs')
        , err             :: Maybe Value
        , observableState :: Value
        }
    deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

fromResp :: Contract.ContractResponse Value Value s v -> PartiallyDecodedResponse v
fromResp Contract.ContractResponse{Contract.hooks, Contract.logs, Contract.err, Contract.lastLogs, Contract.newState = Contract.State{Contract.observableState}} =
    PartiallyDecodedResponse{hooks, logs, err, observableState, lastLogs}

instance Pretty v => Pretty (PartiallyDecodedResponse v) where
    pretty PartiallyDecodedResponse {hooks, observableState} =
        vsep
            [ "State:"
            , indent 2 $ pretty $ abbreviate 120 $ Text.pack $ BS8.unpack $ JSON.encodePretty observableState
            , "Hooks:"
            , indent 2 (vsep $ pretty <$> hooks)
            ]

-- | Whether the instance has any active requests
hasActiveRequests :: forall w s e a. Contract.ContractResponse w e s a -> Bool
hasActiveRequests = not . null . Contract.hooks

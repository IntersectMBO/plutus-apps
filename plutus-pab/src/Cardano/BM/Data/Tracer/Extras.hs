{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.BM.Data.Tracer.Extras(
    mkObjectStr
    , PrettyToObject(..)
    , StructuredLog(..)
    , Tagged(Tagged)
    ) where

import Cardano.BM.Data.Tracer (ToObject (..))
import Data.Aeson (ToJSON (..), Value (String))
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Ledger.Tx (CardanoTx)
import Plutus.Contract.Checkpoint (CheckpointLogMsg)
import Plutus.Contract.Resumable (Response (..))
import Plutus.Contract.State (ContractRequest)
import Plutus.PAB.Events.Contract (ContractInstanceId, IterationID)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.Script.Utils.Value qualified as V
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text qualified as Render
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg, TxBalanceMsg)
import Wallet.Types (EndpointDescription)

-- | Deriving 'ToObject' from 'Pretty'
newtype PrettyToObject a = PrettyToObject { unPrettyToObject :: a }

instance Pretty a => ToObject (PrettyToObject a) where
    toObject _ = Aeson.singleton "string" . String . Render.renderStrict . layoutPretty defaultLayoutOptions . pretty . unPrettyToObject

toStructuredLog' :: forall s a. (KnownSymbol s, ToJSON a) => Tagged s a -> Aeson.KeyMap Value
toStructuredLog' (Tagged a) =
    let k = Aeson.fromString $ symbolVal (Proxy @s)
        v = toJSON a
    in Aeson.singleton k v

-- | Types that can be turned into structured log messages
class StructuredLog a where
    toStructuredLog :: a -> Aeson.KeyMap Value

instance StructuredLog () where
    toStructuredLog _ = Aeson.empty

instance (StructuredLog a, StructuredLog b) =>
    StructuredLog (a, b) where
        toStructuredLog (a, b) = Aeson.union (toStructuredLog a) (toStructuredLog b)

instance (StructuredLog a, StructuredLog b, StructuredLog c) =>
    StructuredLog (a, b, c) where
        toStructuredLog (a, b, c) = Aeson.union (toStructuredLog a) (toStructuredLog (b, c))

instance (StructuredLog a, StructuredLog b, StructuredLog c, StructuredLog d) =>
    StructuredLog (a, b, c, d) where
        toStructuredLog (a, b, c, d) = Aeson.union (toStructuredLog a) (toStructuredLog (b, c, d))

instance (StructuredLog a, StructuredLog b) =>
    StructuredLog (Either a b) where
        toStructuredLog = either toStructuredLog toStructuredLog

instance StructuredLog a => StructuredLog (Maybe a) where
    toStructuredLog = maybe mempty toStructuredLog

deriving via (Tagged "contract_instance" ContractInstanceId) instance StructuredLog ContractInstanceId
deriving via (Tagged "contract_instance_iteration" IterationID) instance StructuredLog IterationID
deriving via (Tagged "message" CheckpointLogMsg) instance StructuredLog CheckpointLogMsg
deriving via (Tagged "message" RequestHandlerLogMsg) instance StructuredLog RequestHandlerLogMsg
deriving via (Tagged "message" TxBalanceMsg) instance StructuredLog TxBalanceMsg
deriving via (Tagged "tx" CardanoTx) instance StructuredLog CardanoTx
deriving via (Tagged "uuid" UUID) instance StructuredLog UUID
deriving via (Tagged "request" (ContractRequest w v)) instance (ToJSON w, ToJSON v) => StructuredLog (ContractRequest w v)
deriving via (Tagged "value" V.Value) instance StructuredLog V.Value
deriving via (Tagged "endpoint" EndpointDescription) instance StructuredLog EndpointDescription
instance ToJSON v => StructuredLog (PartiallyDecodedResponse v) where
    toStructuredLog PartiallyDecodedResponse{hooks, observableState} =
        Aeson.fromList [("hooks", toJSON hooks), ("state", toJSON observableState)]
instance ToJSON v => StructuredLog (Response v) where
    toStructuredLog Response{rspRqID, rspItID, rspResponse} =
        Aeson.fromList
            [ ("requestID", toJSON rspRqID)
            , ("iterationID", toJSON rspItID)
            , ("response", toJSON rspResponse)
            ]

instance (KnownSymbol s, ToJSON a) => StructuredLog (Tagged s a) where
    toStructuredLog = toStructuredLog'

-- | A structured log object with a textual description and additional fields.
mkObjectStr :: StructuredLog k => Text -> k -> Aeson.KeyMap Value
mkObjectStr str rest =
    Aeson.insert "string" (String str) (toStructuredLog rest)

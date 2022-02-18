{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.PAB.Run.PSGenerator where

import Cardano.Wallet.Mock.Types (WalletInfo)
import Control.Applicative ((<|>))
import Control.Lens (set, (&))
import Control.Monad.Freer.Extras.Log (LogLevel, LogMessage)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Language.PureScript.Bridge (BridgePart, Language (Haskell), SumType, argonaut, buildBridge, equal, genericShow,
                                   mkSumType, order)
import Language.PureScript.Bridge.TypeParameters (A, B)
import PSGenerator.Common qualified
import Plutus.Contract.Checkpoint (CheckpointKey, CheckpointStore, CheckpointStoreItem)
import Plutus.Contract.Resumable (Responses)
import Plutus.Contract.StateMachine (InvalidTransition, SMContractError)
import Plutus.Contract.StateMachine.OnChain (State)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Webserver.API qualified as API
import Plutus.PAB.Webserver.Types (ChainReport, CombinedWSStreamToClient, CombinedWSStreamToServer,
                                   ContractActivationArgs, ContractInstanceClientState, ContractReport,
                                   ContractSignatureResponse, FullReport, InstanceStatusToClient)
import Servant.PureScript (HasBridge, Settings, addTypes, apiModuleName, defaultBridge, defaultSettings,
                           generateWithSettings, languageBridge)

-- | List of types linked to contract type `a` that need to be available in
-- Purescript.
class HasPSTypes a where
    psTypes :: [SumType 'Haskell]
    psTypes = []

-- | PAB's main bridge that includes common bridges
pabBridge :: BridgePart
pabBridge =
    PSGenerator.Common.aesonBridge <|>
    PSGenerator.Common.containersBridge <|>
    PSGenerator.Common.languageBridge <|>
    PSGenerator.Common.ledgerBridge <|>
    PSGenerator.Common.servantBridge <|>
    PSGenerator.Common.miscBridge <|>
    defaultBridge

data PabBridge

pabBridgeProxy :: Proxy PabBridge
pabBridgeProxy = Proxy

instance HasBridge PabBridge where
    languageBridge _ = buildBridge pabBridge

-- | PAB's list of types that includes common types.
pabTypes :: [SumType 'Haskell]
pabTypes =
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.playgroundTypes <>
    PSGenerator.Common.walletTypes <>
    -- This type has been handwritten in the frontend. See note there, or try
    -- generating it yourself to see why it's problematic.
    -- [ order . equal . genericShow . argonaut $ mkSumType @(Builtin A)
    [ equal . genericShow . argonaut $ mkSumType @(FullReport A)
    , equal . genericShow . argonaut $ mkSumType @ChainReport
    , equal . genericShow . argonaut $ mkSumType @(ContractReport A)
    , equal . genericShow . argonaut $ mkSumType @(ContractSignatureResponse A)
    , equal . genericShow . argonaut $ mkSumType @(PartiallyDecodedResponse A)

    -- Contract request / response types
    , equal . genericShow . argonaut $ mkSumType @CheckpointStore
    , order . genericShow . argonaut $ mkSumType @CheckpointKey
    , equal . genericShow . argonaut $ mkSumType @(CheckpointStoreItem A)
    , equal . genericShow . argonaut $ mkSumType @(Responses A)

    -- Contract error types
    , equal . genericShow . argonaut $ mkSumType @(InvalidTransition A B)
    , equal . genericShow . argonaut $ mkSumType @(State A)
    , equal . genericShow . argonaut $ mkSumType @SMContractError

    -- Logging types
    , equal . genericShow . argonaut $ mkSumType @(LogMessage A)
    , order . equal . genericShow . argonaut $ mkSumType @LogLevel

    -- Web API types
    , equal . genericShow . argonaut $ mkSumType @(ContractActivationArgs A)
    , genericShow . argonaut $ mkSumType @(ContractInstanceClientState A)
    , genericShow . argonaut $ mkSumType @InstanceStatusToClient
    , genericShow . argonaut $ mkSumType @CombinedWSStreamToClient
    , genericShow . argonaut $ mkSumType @CombinedWSStreamToServer
    , genericShow . argonaut $ mkSumType @WalletInfo
    ]

pabSettings :: Settings
pabSettings = defaultSettings
    & set apiModuleName "Plutus.PAB.Webserver"
    & addTypes pabTypes

------------------------------------------------------------
-- | Use the Proxy for specifying `a` when generating PS functions for the
-- webserver using a specific Builtin (ex. generate 'Builtin Marlowe' instead
-- of 'Builtin a').
generateAPIModule
    :: forall a. (Typeable a, HasPSTypes a)
    => Proxy a
    -> FilePath -- ^ Output directory of PS files
    -> IO ()
generateAPIModule _ outputDir = do
    generateWithSettings
        (addTypes (psTypes @a) pabSettings)
        outputDir
        pabBridgeProxy
        (    Proxy @(API.API (Contract.ContractDef (Builtin a)) Text.Text)
        )

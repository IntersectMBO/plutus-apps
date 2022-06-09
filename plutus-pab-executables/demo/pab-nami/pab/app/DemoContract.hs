{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module DemoContract(
    DemoContract(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType, order)
import Ledger (PaymentPubKeyHash, StakePubKeyHash, Value)
import Ledger.Constraints (mustPayToPubKeyAddress)
import Playground.Types (FunctionSchema)
import Plutus.Contract (ContractError, Endpoint, Promise, adjustUnbalancedTx, endpoint, logInfo, mkTxConstraints,
                        yieldUnbalancedTx)
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions, SomeBuiltin (SomeBuiltin))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Prettyprinter (Pretty, pretty, viaShow)
import Schema (FormSchema, ToSchema)

data DemoContract = DemoContract
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty DemoContract where
    pretty = viaShow

instance HasPSTypes DemoContract where
    psTypes =
        [ order . equal . genericShow . argonaut $ mkSumType @DemoContract
        ]


instance HasDefinitions DemoContract where
    getDefinitions = [ DemoContract
                     ]
    getContract = getDemoContract
    getSchema = getDemoContractSchema

getDemoContractSchema :: DemoContract -> [FunctionSchema FormSchema]
getDemoContractSchema = \case
    DemoContract -> Builtin.endpointsToSchemas @PayToWalletSchema

getDemoContract :: DemoContract -> SomeBuiltin
getDemoContract = \case
    DemoContract -> SomeBuiltin payToWallet

data PayToWalletParams =
    PayToWalletParams
        { amount :: Value
        , pkh    :: PaymentPubKeyHash
        , skh    :: StakePubKeyHash
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type PayToWalletSchema = Endpoint "PayToWallet" PayToWalletParams

payToWallet :: Promise () PayToWalletSchema ContractError ()
payToWallet = endpoint @"PayToWallet" $ \PayToWalletParams{amount, pkh, skh} -> do
    utx <- mkTxConstraints @Void mempty (mustPayToPubKeyAddress pkh skh amount)
    logInfo @String $ show utx
    adjustUnbalancedTx utx >>= yieldUnbalancedTx


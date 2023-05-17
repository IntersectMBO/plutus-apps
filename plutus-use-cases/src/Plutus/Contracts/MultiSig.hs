{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements an n-out-of-m multisig contract.
module Plutus.Contracts.MultiSig
    ( MultiSig(..)
    , MultiSigSchema
    , contract
    , lock
    , typedValidator
    , validate
    ) where

import Cardano.Node.Emulator.Internal.Node (pNetworkId)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.V2.Ledger.Api as Plutus (Value)
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)

import Prelude as Haskell (Semigroup (..), Show, foldMap)

type MultiSigSchema =
        Endpoint "lock" (MultiSig, Plutus.Value)
        .\/ Endpoint "unlock" (MultiSig, [PaymentPubKeyHash])

data MultiSig =
        MultiSig
                { signatories      :: [Ledger.PaymentPubKeyHash]
                -- ^ List of public keys of people who may sign the transaction
                , minNumSignatures :: Integer
                -- ^ Minimum number of signatures required to unlock
                --   the output (should not exceed @length signatories@)
                } deriving stock (Show, Generic)
                  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''MultiSig

contract :: AsContractError e => Contract () MultiSigSchema e ()
contract = selectList [lock, unlock] >> contract

{-# INLINABLE validate #-}
validate :: MultiSig -> () -> () -> V2.ScriptContext -> Bool
validate MultiSig{signatories, minNumSignatures} _ _ p =
    let present = length (filter (V2.txSignedBy (V2.scriptContextTxInfo p) . unPaymentPubKeyHash) signatories)
    in traceIfFalse "not enough signatures" (present >= minNumSignatures)

instance Scripts.ValidatorTypes MultiSig where
    type instance RedeemerType MultiSig = ()
    type instance DatumType MultiSig = ()

typedValidator :: MultiSig -> V2.TypedValidator MultiSig
typedValidator = V2.mkTypedValidatorParam @MultiSig
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator


-- | Lock some funds in a 'MultiSig' contract.
lock :: AsContractError e => Promise () MultiSigSchema e ()
lock = endpoint @"lock" $ \(ms, vl) -> do
    let inst = typedValidator ms
    let tx = Constraints.mustPayToTheScriptWithDatumInTx () vl
        lookups = Constraints.typedValidatorLookups inst
    mkTxConstraints lookups tx
        >>= adjustUnbalancedTx >>= void . submitUnbalancedTx

-- | The @"unlock"@ endpoint, unlocking some funds with a list
--   of signatures.
unlock :: AsContractError e => Promise () MultiSigSchema e ()
unlock = endpoint @"unlock" $ \(ms, pks) -> do
    networkId <- pNetworkId <$> getParams
    let inst = typedValidator ms
    utx <- utxosAt (Scripts.validatorCardanoAddress networkId inst)
    let tx = Constraints.spendUtxosFromTheScript utx ()
                <> foldMap Constraints.mustBeSignedBy pks
        lookups = Constraints.typedValidatorLookups inst
                <> Constraints.unspentOutputs utx
    mkTxConstraints lookups tx
        >>= adjustUnbalancedTx >>= void . submitUnbalancedTx

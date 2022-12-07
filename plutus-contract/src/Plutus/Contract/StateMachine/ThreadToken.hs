{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Thread token data type definition and minting policy.
--   Thread tokens are used to identify the contract instance on the blockchain,
--   and ensuring that the state was produced by running the state machine from its initial state.
module Plutus.Contract.StateMachine.ThreadToken where

import PlutusTx.Prelude hiding (Monoid (..), Semigroup (..))

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger (CurrencySymbol, TxOutRef (..))
import Ledger.Scripts
import Plutus.Contract.StateMachine.MintingPolarity (MintingPolarity (..))
import Plutus.Script.Utils.Typed (ScriptContextV1, mkUntypedMintingPolicy)
import Plutus.Script.Utils.Value (TokenName (..), Value (..))
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as V
import PlutusTx qualified
import Prelude qualified as Haskell

data ThreadToken = ThreadToken
    { ttOutRef         :: TxOutRef
    , ttCurrencySymbol :: CurrencySymbol
    }
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)


PlutusTx.makeIsDataIndexed ''ThreadToken [('ThreadToken,0)]
PlutusTx.makeLift ''ThreadToken

checkPolicy :: TxOutRef -> (ValidatorHash, MintingPolarity) -> V.ScriptContext -> Bool
checkPolicy (TxOutRef refHash refIdx) (vHash, mintingPolarity) ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
    let
        ownSymbol = V.ownCurrencySymbol ctx

        minted = V.txInfoMint txinfo
        expected = if mintingPolarity == Burn then -1 else 1

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = checkThreadTokenInner ownSymbol vHash minted expected
            in traceIfFalse "S7" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = V.spendsOutput txinfo refHash refIdx
            in  traceIfFalse "S8" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && (if mintingPolarity == Mint then txOutputSpent else True)

curPolicy :: TxOutRef -> MintingPolicy
curPolicy outRef = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \r -> mkUntypedMintingPolicy @ScriptContextV1 (checkPolicy r) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode outRef

{-# INLINABLE threadTokenValue #-}
-- | The 'Value' containing exactly the thread token.
threadTokenValue :: CurrencySymbol -> ValidatorHash -> Value
threadTokenValue currency (ValidatorHash vHash) = Value.singleton currency (TokenName vHash) 1

-- | Check exactly `n` thread tokens and no other tokens with the given
-- @CurrencySymbol@ are in the given @Value@.
{-# INLINABLE checkThreadTokenInner #-}
checkThreadTokenInner ::
    -- | The currency symbol of the thread token.
    CurrencySymbol ->
    -- | The hash of the (state machine) validator script using this thread
    -- token. This is used as the @TokenName@ of the thread token.
    ValidatorHash ->
    -- | The value to check.
    Value ->
    -- | The expected number of thread tokens in the given value, `n`.
    Integer ->
    -- | True if and only if exactly `n` thread tokens (and no other tokens)
    -- with the given @CurrencySymbol@ are in the given @Value@.
    Bool
checkThreadTokenInner currency (ValidatorHash vHash) value n =
    Value.currencyValueOf value currency == Value.singleton currency (TokenName vHash) n

{-# INLINABLE checkThreadToken #-}
checkThreadToken :: Maybe ThreadToken -> ValidatorHash -> Value -> Integer -> Bool
checkThreadToken Nothing _ _ _ = True
checkThreadToken (Just threadToken) vHash vl i =
    checkThreadTokenInner (ttCurrencySymbol threadToken) vHash vl i

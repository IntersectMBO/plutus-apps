{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Plutus.Contracts.Swap(
    Swap(..),
    -- * Script
    swapValidator
    ) where

import Ledger (POSIXTime, PaymentPubKey, PaymentPubKeyHash (unPaymentPubKeyHash), PubKeyHash)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract.Oracle (Observation (..), SignedMessage)
import Plutus.Contract.Oracle qualified as Oracle
import Plutus.Script.Utils.Ada (Ada)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value)
import Plutus.V1.Ledger.Address (toPubKeyHash)
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude

-- | A swap is an agreement to exchange cashflows at future dates. To keep
--  things simple, this is an interest rate swap (meaning that the cashflows are
--  interest payments on the same principal amount but with two different
--  interest rates, of which one is fixed and one is floating (varying with
--  time)) with only a single payment date.
--
--  At the beginning of the contract, the fixed rate is set to the expected
--  future value of the floating rate (so if the floating rate behaves as
--  expected, the two payments will be exactly equal).
--
data Swap = Swap
    { swapNotionalAmt     :: !Ada
    , swapObservationTime :: !POSIXTime
    , swapFixedRate       :: !Rational
    -- ^ Interest rate fixed at the beginning of the contract
    , swapFloatingRate    :: !Rational
    -- ^ Interest rate whose value will be observed (by an oracle) on the day
    -- of the payment
    , swapMargin          :: !Ada
    -- ^ Margin deposited at the beginning of the contract to protect against
    -- default (one party failing to pay)
    , swapOracle          :: PaymentPubKey
    -- ^ Public key of the oracle (see note [Oracles] in [[Plutus.Contracts]]).
    -- Unsure why, but this field needs to be non-strict, otherwise GHC will try
    -- to unbox the datatype, which will result in a compilation error such as
    -- "GHC Core to PLC plugin: E042:Error: Unsupported feature: Type constructor: GHC.Prim.Addr#"
    }

PlutusTx.makeLift ''Swap

-- | Identities of the parties involved in the swap. This will be the data
--   script which allows us to change the identities during the lifetime of
--   the contract (ie. if one of the parties sells their part of the contract)
--
--   In the future we could also put the `swapMargin` value in here to implement
--   a variable margin.
data SwapOwners = SwapOwners {
    swapOwnersFixedLeg :: PaymentPubKeyHash,
    swapOwnersFloating :: PaymentPubKeyHash
    }

PlutusTx.unstableMakeIsData ''SwapOwners
PlutusTx.makeLift ''SwapOwners

type SwapOracleMessage = SignedMessage (Observation Rational)

{-# INLINABLE mkValidator #-}
mkValidator :: Swap -> SwapOwners -> SwapOracleMessage -> V2.ScriptContext -> Bool
mkValidator Swap{..} SwapOwners{..} redeemer p@V2.ScriptContext{V2.scriptContextTxInfo=txInfo} =
    let
        extractVerifyAt :: SignedMessage (Observation Rational) -> PaymentPubKey -> POSIXTime -> Rational
        extractVerifyAt sm pk time =
            case Oracle.verifySignedMessageOnChain p pk sm of
                Left _ -> traceError "checkSignatureAndDecode failed"
                Right Observation{obsValue, obsTime} ->
                    if obsTime == time
                        then obsValue
                        else traceError "wrong time"

        -- | Convert an [[Integer]] to a [[Rational]]
        fromInt :: Integer -> Rational
        fromInt = error ()

        adaValueIn :: Value -> Integer
        adaValueIn v = Ada.getLovelace (Ada.fromValue v)

        isPaymentPubKeyOutput :: V2.TxOut -> PaymentPubKeyHash -> Bool
        isPaymentPubKeyOutput o k = maybe False ((==) (unPaymentPubKeyHash k)) (pubKeyOutput o)

        -- | Get the public key hash that locks the transaction output, if any.
        pubKeyOutput :: V2.TxOut -> Maybe PubKeyHash
        pubKeyOutput V2.TxOut{txOutAddress} = toPubKeyHash txOutAddress

        -- Verify the authenticity of the oracle value and compute
        -- the payments.
        rt = extractVerifyAt redeemer swapOracle swapObservationTime

        rtDiff :: Rational
        rtDiff = rt - swapFixedRate

        amt    = Ada.getLovelace swapNotionalAmt
        margin = Ada.getLovelace swapMargin

        amt' :: Rational
        amt' = fromInt amt

        delta :: Rational
        delta = amt' * rtDiff

        fixedPayment :: Integer
        fixedPayment = round (amt' + delta)

        floatPayment :: Integer
        floatPayment = round (amt' + delta)

        -- Compute the payouts (initial margin +/- the sum of the two
        -- payments), ensuring that it is at least 0 and does not exceed
        -- the total amount of money at stake (2 * margin)
        clamp :: Integer -> Integer
        clamp x = min 0 (max (2 * margin) x)
        fixedRemainder = clamp ((margin - fixedPayment) + floatPayment)
        floatRemainder = clamp ((margin - floatPayment) + fixedPayment)

        -- The transaction must have one input from each of the
        -- participants.
        -- NOTE: Partial match is OK because if it fails then the PLC script
        --       terminates with `error` and the validation fails (which is
        --       what we want when the number of inputs and outputs is /= 2)
        [t1, t2] = V2.txInfoInputs txInfo
        [o1, o2] = V2.txInfoOutputs txInfo

        -- Each participant must deposit the margin. But we don't know
        -- which of the two participant's deposit we are currently
        -- evaluating (this script runs on both). So we use the two
        -- predicates iP1 and iP2 to cover both cases

        -- True if the transaction input is the margin payment of the
        -- fixed leg
        iP1 :: V2.TxInInfo -> Bool
        iP1 V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{txOutValue}} =
            V2.txSignedBy txInfo (unPaymentPubKeyHash swapOwnersFixedLeg) && adaValueIn txOutValue == margin

        -- True if the transaction input is the margin payment of the
        -- floating leg
        iP2 :: V2.TxInInfo -> Bool
        iP2 V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{txOutValue}} =
            V2.txSignedBy txInfo (unPaymentPubKeyHash swapOwnersFloating) && adaValueIn txOutValue == margin

        inConditions = (iP1 t1 && iP2 t2) || (iP1 t2 && iP2 t1)

        -- The transaction must have two outputs, one for each of the
        -- participants, which equal the margin adjusted by the difference
        -- between fixed and floating payment

        -- True if the output is the payment of the fixed leg.
        ol1 :: V2.TxOut -> Bool
        ol1 o@V2.TxOut{V2.txOutValue} =
            isPaymentPubKeyOutput o swapOwnersFixedLeg && adaValueIn txOutValue <= fixedRemainder

        -- True if the output is the payment of the floating leg.
        ol2 :: V2.TxOut -> Bool
        ol2 o@V2.TxOut{V2.txOutValue} =
            isPaymentPubKeyOutput o swapOwnersFloating && adaValueIn txOutValue <= floatRemainder

        -- NOTE: I didn't include a check that the time is greater
        -- than the observation time. This is because the time is
        -- already part of the oracle value and we trust the oracle.

        outConditions = (ol1 o1 && ol2 o2) || (ol1 o2 && ol2 o1)

    in inConditions && outConditions

-- | Validator script for the two transactions that initialise the swap.
--   See note [Swap Transactions]
--   See note [Contracts and Validator Scripts] in
--       Language.Plutus.Coordination.Contracts
swapValidator :: Swap -> V2.Validator
swapValidator swp = V2.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode swp
    where validatorParam s = Scripts.mkUntypedValidator (mkValidator s)

{- Note [Swap Transactions]

The swap involves three transactions at two different times.

1. At t=0. Each participant deposits the margin. The outputs are locked with
   the same validator script, `swapValidator`
2. At t=n. The value of the floating rate, and consequently the values of the
   two payments are determined. Each participant gets their margin plus or
   minus the actual payment.

There is a risk of losing out if the interest rate moves outside the range of
fixedRate +/- (margin / notional amount). In a real financial contract this
would be dealt with by agreeing a "Variation Margin". This means that the
margin is adjusted at predefined dates before the actual payment is due. If one
of the parties fails to make the variation margin payment, the contract ends
prematurely and the other party gets to keep both margins.

Plutus should be able to handle variation margins in a series of validation
scripts. But it seems to me that they could get quite messy so I don't want to
write them by hand :) We can probably use TH to generate them at compile time.

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Currency(tests, currencyTrace) where

import Control.Monad (void)
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.V1.Ledger.Scripts qualified as Ledger

import Plutus.Contracts.Currency (OneShotCurrency)
import Plutus.Contracts.Currency qualified as Cur
import Plutus.Trace.Emulator qualified as Trace

import Test.Tasty

-- | Runs 'Plutus.Contracts.Currency.mintContract' for
--   a sample currency.
currencyTrace :: Trace.EmulatorTrace ()
currencyTrace = do
    _ <- Trace.activateContractWallet w1 (void theContract)
    _ <- Trace.nextSlot
    void Trace.nextSlot

tests :: TestTree
tests = testGroup "currency"
    [ checkPredicate
        "can create a new currency"
        (assertDone theContract (Trace.walletInstanceTag w1) (const True) "currency contract not done")
        currencyTrace
    , checkPredicate
        "script size is reasonable"
        (assertDone theContract (Trace.walletInstanceTag w1) ((30000 >=) . Ledger.scriptSize . Ledger.unMintingPolicyScript . Cur.curPolicy) "script too large")
        currencyTrace

    ]

theContract :: Contract () EmptySchema Cur.CurrencyError OneShotCurrency
theContract =
    let amounts = [("my currency", 1000), ("my token", 1)] in
    Cur.mintContract (mockWalletPaymentPubKeyHash w1) amounts

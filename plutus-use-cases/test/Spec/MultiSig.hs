{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.MultiSig(tests, failingTrace, succeedingTrace) where

import Control.Monad (void)
import Ledger.CardanoWallet qualified as CW
import Plutus.Contract (Contract, ContractError)
import Plutus.Contract.Test
import Plutus.Contracts.MultiSig as MS
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified
import Prelude hiding (not)
import Test.Tasty
import Wallet.Emulator.Wallet (signPrivateKeys)

tests :: TestTree
tests = testGroup "multisig"
    [
    checkPredicate "2 out of 5"
        (assertEvaluationError "not enough signatures")
        failingTrace

    , checkPredicate "3 out of 5"
        assertNoFailedTransactions
        succeedingTrace

    , goldenPir "test/Spec/multisig.pir" $$(PlutusTx.compile [|| MS.validate ||])
    ]

-- | Lock some funds, then attempt to unlock them with a transaction
--   that doesn't have the required number of signatures
failingTrace :: EmulatorTrace ()
failingTrace = do
    hdl <- Trace.activateContractWallet w1 theContract
    Trace.callEndpoint @"lock" hdl (multiSig, Ada.lovelaceValueOf 10)
    void $ Trace.waitNSlots 1
    Trace.setSigningProcess w1 (Just $ signPrivateKeys $ CW.paymentPrivateKey . CW.knownMockWallet <$> [1,2])
    Trace.callEndpoint @"unlock" hdl (multiSig, fmap mockWalletPaymentPubKeyHash [w1, w2])
    void $ Trace.waitNSlots 1

-- | Lock some funds, then unlock them with a transaction that has the
--   three required signatures.
succeedingTrace :: EmulatorTrace ()
succeedingTrace = do
    hdl <- Trace.activateContractWallet w1 theContract
    Trace.callEndpoint @"lock" hdl (multiSig, Ada.lovelaceValueOf 10)
    void $ Trace.waitNSlots 1
    Trace.setSigningProcess w1 (Just $ signPrivateKeys $ CW.paymentPrivateKey . CW.knownMockWallet <$> [1,2,3])
    Trace.callEndpoint @"unlock" hdl (multiSig, fmap mockWalletPaymentPubKeyHash [w1, w2, w3])
    void $ Trace.waitNSlots 1

theContract :: Contract () MultiSigSchema ContractError ()
theContract = MS.contract

-- a 'MultiSig' contract that requires three out of five signatures
multiSig :: MultiSig
multiSig = MultiSig
        { signatories = mockWalletPaymentPubKeyHash . knownWallet <$> [1..5]
        , minNumSignatures = 3
        }

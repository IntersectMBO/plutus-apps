{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Spec.MultiSigStateMachine(tests, lockProposeSignPay) where

import Data.Foldable (traverse_)

import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Time (POSIXTime)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Ada qualified as Ada
import Wallet.Emulator qualified as EM

import Plutus.Contract.Test
import Plutus.Contracts.MultiSigStateMachine qualified as MS
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit

tests :: TestTree
tests =
    testGroup "multi sig state machine tests"
    [ checkPredicate "lock, propose, sign 3x, pay - SUCCESS"
        (assertNoFailedTransactions
        .&&. walletFundsChangePlutus w1 (Ada.adaValueOf (-10))
        .&&. walletFundsChangePlutus w2 (Ada.adaValueOf 5))
        (lockProposeSignPay 3 1)

    , checkPredicate "lock, propose, sign 2x, pay - FAILURE"
        (assertNotDone (MS.contract  @MS.MultiSigError params) (Trace.walletInstanceTag w1) "contract should proceed after invalid transition"
        .&&. walletFundsChangePlutus w1 (Ada.adaValueOf (-10))
        .&&. walletFundsChangePlutus w2 mempty)
        (lockProposeSignPay 2 1)

    , checkPredicate "lock, propose, sign 3x, pay x2 - SUCCESS"
        (assertNoFailedTransactions
        .&&. walletFundsChangePlutus w1 (Ada.adaValueOf (-10))
        .&&. walletFundsChangePlutus w2 (Ada.adaValueOf 10))
        (lockProposeSignPay 3 2)

    , checkPredicate "lock, propose, sign 3x, pay x3 - FAILURE"
        (assertNotDone (MS.contract  @MS.MultiSigError params) (Trace.walletInstanceTag w2) "contract should proceed after invalid transition"
        .&&. walletFundsChangePlutus w1 (Ada.adaValueOf (-10))
        .&&. walletFundsChangePlutus w2 (Ada.adaValueOf 10))
        (lockProposeSignPay 3 3)

    -- TODO: turn this on again when reproducibility issue in core is fixed
    -- , goldenPir "test/Spec/multisigStateMachine.pir" $$(PlutusTx.compile [|| MS.mkValidator ||])
    , HUnit.testCaseSteps "script size is reasonable" $ \step -> reasonable' step (Scripts.validatorScript $ MS.typedValidator params) 51000
    ]

-- | A multisig contract that requires 3 out of 5 signatures
params :: MS.Params
params = MS.Params keys 3 where
    keys = EM.mockWalletPaymentPubKeyHash . knownWallet <$> [1..5]

-- | A payment of 5 Ada to the public key address of wallet 2
payment :: POSIXTime -> MS.Payment
payment startTime =
    MS.Payment
        { MS.paymentAmount    = Ada.adaValueOf 5
        , MS.paymentRecipient = Ledger.toPlutusAddress $ EM.mockWalletAddress w2
        , MS.paymentDeadline  = startTime + 20000
        }

-- | Lock some funds in the contract, then propose the payment
--   'payment', then call @"add-signature"@ a number of times and
--   finally call @"pay"@ a number of times.
lockProposeSignPay :: Integer -> Integer -> EmulatorTrace ()
lockProposeSignPay signatures rounds = do
    let wallets = knownWallet <$> [1..signatures]
        activate w = Trace.activateContractWallet w (MS.contract @MS.MultiSigError params)

    -- the 'proposeSignPay' trace needs at least 2 signatures
    handle1 <- activate w1
    handle2 <- activate w2
    handles <- traverse activate (drop 2 wallets)
    _ <- Trace.callEndpoint @"lock" handle1 (Ada.adaValueOf 10)
    _ <- Trace.waitNSlots 1
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let proposeSignPay = do
            Trace.callEndpoint @"propose-payment" handle2 (payment startTime)
            _ <- Trace.waitNSlots 1
            -- Call @"add-signature"@ @signatures@ times
            traverse_ (\hdl -> Trace.callEndpoint @"add-signature" hdl () >> Trace.waitNSlots 1) (handle1:handle2:handles)

            -- Call @"pay"@ on wallet 1
            Trace.callEndpoint @"pay" handle1 ()
            Trace.waitNSlots 1

    traverse_ (const proposeSignPay) [1..rounds]

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.Game
  ( tests, successTrace, failTrace
  ) where

import Control.Monad (void)
import Data.Default (Default (def))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit

import Ledger.Ada qualified as Ada
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Plutus.Contract.Test (checkPredicate, goldenPir, mockWalletPaymentPubKeyHash, reasonable', valueAtAddress, w1,
                             w2, walletFundsChange, (.&&.))
import Plutus.Contracts.Game (GuessArgs (GuessArgs, guessArgsGameParam, guessArgsSecret),
                              LockArgs (LockArgs, lockArgsGameParam, lockArgsSecret, lockArgsValue))
import Plutus.Contracts.Game qualified as G
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified

gameParam :: G.GameParam
gameParam = G.GameParam (mockWalletPaymentPubKeyHash w1) (TimeSlot.scSlotZeroTime def)

-- * Unit tests

tests :: TestTree
tests =
    testGroup "game state machine with secret arguments tests"
    [ checkPredicate "run a successful game trace"
        (walletFundsChange w2 (Ada.adaValueOf 8)
        .&&. valueAtAddress (Scripts.validatorAddress $ G.gameInstance gameParam) (Ada.adaValueOf 0 ==)
        .&&. walletFundsChange w1 (Ada.adaValueOf (-8)))
        successTrace

    , checkPredicate "run a failed trace"
        (walletFundsChange w2 mempty
        .&&. valueAtAddress (Scripts.validatorAddress $ G.gameInstance gameParam) (Ada.adaValueOf 8 ==)
        .&&. walletFundsChange w1 (Ada.adaValueOf (-8)))
        failTrace

    , goldenPir "test/Spec/game.pir" $$(PlutusTx.compile [|| G.mkValidator ||])

    , HUnit.testCaseSteps "script size is reasonable" $ \step ->
        reasonable' step (Scripts.validatorScript $ G.gameInstance gameParam) 49000
    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10

-- | Wallet 1 locks some funds, and w2 then makes a correct guess.
successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 $ G.contract @Text
    Trace.callEndpoint @"lock" hdl LockArgs { lockArgsGameParam = gameParam
                                            , lockArgsSecret = "hello"
                                            , lockArgsValue = Ada.adaValueOf 8
                                            }
    -- One slot for sending the Ada to the script.
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 $ G.contract @Text
    Trace.callEndpoint @"guess" hdl2 GuessArgs { guessArgsGameParam = gameParam
                                               , guessArgsSecret = "hello"
                                               }
    void $ Trace.waitNSlots 1

-- | Wallet 1 locks some funds, and Wallet 2 makes a wrong guess.
failTrace :: EmulatorTrace ()
failTrace = do
    hdl <- Trace.activateContractWallet w1 $ G.contract @Text
    Trace.callEndpoint @"lock" hdl LockArgs { lockArgsGameParam = gameParam
                                            , lockArgsSecret = "hello"
                                            , lockArgsValue = Ada.adaValueOf 8
                                            }
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 $ G.contract @Text
    _ <- Trace.callEndpoint @"guess" hdl2 GuessArgs { guessArgsGameParam = gameParam
                                                    , guessArgsSecret = "hola"
                                                    }
    void $ Trace.waitNSlots 1

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
module Spec.GameBabbage
  ( tests, successTrace, failTrace
  ) where

import Control.Monad (void)
import Data.Default (Default (def))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit

import Cardano.Node.Emulator.Params (testnet)
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Ledger (CardanoAddress, toPlutusAddress)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract.Test (checkPredicate, goldenPir, mockWalletAddress, reasonable', valueAtAddress, w1, w2, w3,
                             walletFundsChange, (.&&.))
import Plutus.Contracts.GameBabbage (GuessArgs (GuessArgs, guessArgsGameAddress, guessArgsGameParam, guessArgsSecret),
                                     LockArgs (LockArgs, lockArgsGameAddress, lockArgsGameParam, lockArgsSecret, lockArgsValue))
import Plutus.Contracts.GameBabbage qualified as G
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value)
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified

gameParam :: G.GameParam
gameParam = G.GameParam (toPlutusAddress $ mockWalletAddress w3) (TimeSlot.scSlotZeroTime def)

-- * Unit tests

tests :: TestTree
tests =
    testGroup "babbage game with secret arguments tests"
    [ checkPredicate "run a successful game trace"
        (walletFundsChange w2 (Value.adaValueOf 8)
        .&&. valueAtAddress (Scripts.validatorCardanoAddress testnet $ G.gameInstance gameParam) Value.isZero
        .&&. walletFundsChange w1 (Value.adaValueOf (-8)))
        successTrace

    , checkPredicate "run a successful game trace with two guesses"
        (walletFundsChange w2 (Value.adaValueOf 8)
        .&&. valueAtAddress (Scripts.validatorCardanoAddress testnet $ G.gameInstance gameParam) Value.isZero
        .&&. walletFundsChange w1 (Value.adaValueOf (-8)))
        successTwoGuessesTrace

    , checkPredicate "run a failed trace"
        (walletFundsChange w2 mempty
        .&&. valueAtAddress (Scripts.validatorCardanoAddress testnet $ G.gameInstance gameParam) (Value.adaValueOf 8 ==)
        .&&. walletFundsChange w1 (Value.adaValueOf (-8)))
        failTrace

    , goldenPir "test/Spec/game.pir" $$(PlutusTx.compile [|| G.mkValidator ||])

    , HUnit.testCaseSteps "script size is reasonable" $ \step ->
        reasonable' step (Scripts.validatorScript $ G.gameInstance gameParam) 49000
    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10


lock :: Trace.ContractHandle w G.GameSchema e -> String -> Value -> EmulatorTrace ()
lock hdl secret value = do
    Trace.callEndpoint
        @"lock"
        hdl
        LockArgs
            { lockArgsGameParam = gameParam
            , lockArgsGameAddress = mockWalletAddress w3
            , lockArgsSecret = secret
            , lockArgsValue = value
            }
    -- One slot for sending the Ada to the script.
    void Trace.nextSlot

guess :: Trace.ContractHandle w G.GameSchema e -> String -> CardanoAddress -> EmulatorTrace ()
guess hdl proposition addr = do
    Trace.callEndpoint
        @"guess"
        hdl
        GuessArgs
            { guessArgsGameParam = gameParam
            , guessArgsGameAddress = addr
            , guessArgsSecret = proposition
            }
    void Trace.nextSlot

successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w3 $ G.contract @Text
    Trace.callEndpoint @"init" hdl gameParam
    void Trace.nextSlot
    hdl2 <- Trace.activateContractWallet w1 $ G.contract @Text
    lock hdl2 "hello" (Ada.adaValueOf 8)
    hdl3 <- Trace.activateContractWallet w2 $ G.contract @Text
    guess hdl3 "hello" (mockWalletAddress w3)

-- | Wallet 1 locks some funds, and w2 then makes a correct guess.
successTwoGuessesTrace :: EmulatorTrace ()
successTwoGuessesTrace = do
    hdl <- Trace.activateContractWallet w3 $ G.contract @Text
    Trace.callEndpoint @"init" hdl gameParam
    void Trace.nextSlot
    hdl2 <- Trace.activateContractWallet w1 $ G.contract @Text
    lock hdl2 "hello" (Ada.adaValueOf 4)
    hdl3 <- Trace.activateContractWallet w2 $ G.contract @Text
    guess hdl3 "hello" (mockWalletAddress w3)
    hdl2 <- Trace.activateContractWallet w1 $ G.contract @Text
    lock hdl2 "hola" (Ada.adaValueOf 4)
    hdl3 <- Trace.activateContractWallet w2 $ G.contract @Text
    guess hdl3 "hola" (mockWalletAddress w3)

-- | Wallet 1 locks some funds, and Wallet 2 makes a wrong guess.
failTrace :: EmulatorTrace ()
failTrace = do
    hdl <- Trace.activateContractWallet w3 $ G.contract @Text
    Trace.callEndpoint @"init" hdl gameParam
    void Trace.nextSlot
    hdl2 <- Trace.activateContractWallet w1 $ G.contract @Text
    lock hdl2 "hello" (Ada.adaValueOf 8)
    hdl3 <- Trace.activateContractWallet w2 $ G.contract @Text
    guess hdl3 "hola" (mockWalletAddress w3)

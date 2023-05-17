{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Spec.SimpleEscrow(tests) where

import Control.Lens
import Control.Monad (void)

import Cardano.Node.Emulator.Generators (someTokenValue)
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Time (POSIXTime)
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract.Test
import Plutus.Contracts.SimpleEscrow
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value)
import Plutus.Trace.Emulator qualified as Trace

import Test.Tasty

tests :: TestTree
tests = testGroup "simple-escrow"
    [ checkPredicate "can lock some value in the contract"
        ( walletFundsChange w1 (Value.adaValueOf (-10))
          .&&. walletFundsChange w2 mempty
        )
        $ do
            startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
            let params = mkEscrowParams startTime (Ada.adaValueOf 10) (Ada.adaValueOf 2)

            hdl <- Trace.activateContractWallet w1 lockEp
            Trace.callEndpoint @"lock" hdl params
    , checkPredicateOptions options "can lock and redeem"
        ( walletFundsChange w1 (token1 (-10) <> token2 5)
          .&&. walletFundsChange w2 (token1 10 <> token2 (-5))
        )
        $ do
            startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
            let params = mkEscrowParams startTime (Value.fromCardanoValue $ token1 10) (Value.fromCardanoValue $ token2 5)

            hdl1 <- Trace.activateContractWallet w1 lockEp
            Trace.callEndpoint @"lock" hdl1 params
            void $ Trace.waitNSlots 1

            hdl2 <- Trace.activateContractWallet w2 (void redeemEp)
            void $ Trace.callEndpoint @"redeem" hdl2 params
    , checkPredicate "can lock and refund"
        ( walletFundsChange w1 mempty
          .&&. walletFundsChange w2 mempty
        )
        $ do
            startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
            let params = mkEscrowParams startTime (Ada.adaValueOf 10) (Ada.adaValueOf 2)

            hdl <- Trace.activateContractWallet w1 (lockEp <> void refundEp)
            Trace.callEndpoint @"lock" hdl params

            void $ Trace.waitNSlots 100
            void $ Trace.callEndpoint @"refund" hdl params
    , checkPredicate "only locking wallet can request refund"
        ( walletFundsChange w1 (Value.adaValueOf (-10))
          .&&. walletFundsChange w2 mempty
        )
        $ do
            startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
            let params = mkEscrowParams startTime (Ada.adaValueOf 10) (Ada.adaValueOf 2)

            hdl1 <- Trace.activateContractWallet w1 lockEp
            Trace.callEndpoint @"lock" hdl1 params

            hdl2 <- Trace.activateContractWallet w2 (void refundEp)
            void $ Trace.waitNSlots 100
            void $ Trace.callEndpoint @"refund" hdl2 params
    , checkPredicateOptions options "can't redeem if you can't pay"
        ( walletFundsChange w1 (Value.lovelaceToValue (-Ledger.minLovelaceTxOutEstimated) <> token1 (-10))
          .&&. walletFundsChange w2 mempty
        )
        $ do
            startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
            -- 501 token1 is _just_ too much; we don't have enough ( our options
            -- allocate only 500 ).
            let params = mkEscrowParams startTime (Value.fromCardanoValue $ token1 10) (Value.fromCardanoValue $ token2 501)

            hdl1 <- Trace.activateContractWallet w1 lockEp
            Trace.callEndpoint @"lock" hdl1 params
            void $ Trace.waitNSlots 1

            hdl2 <- Trace.activateContractWallet w2 (void redeemEp)
            void $ Trace.callEndpoint @"redeem" hdl2 params
    ]

token1 :: Integer -> Value.Value
token1 = someTokenValue "Token1"

token2 :: Integer -> Value.Value
token2 = someTokenValue "Token2"

options :: CheckOptions
options =
    defaultCheckOptions
        & changeInitialWalletValue w1 ((<>) (token1 500))
        & changeInitialWalletValue w2 ((<>) (token2 500))

mkEscrowParams :: POSIXTime -> Value -> Value -> EscrowParams
mkEscrowParams startTime p e =
  EscrowParams
    { payee     = mockWalletPaymentPubKeyHash w1
    , paying    = p
    , expecting = e
    , deadline  = startTime + 100000
    }

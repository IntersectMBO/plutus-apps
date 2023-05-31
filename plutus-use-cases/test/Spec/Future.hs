{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Future(tests, testAccounts, theFuture, increaseMarginTrace, settleEarlyTrace, payOutTrace) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as Freer
import Data.Default (Default (def))
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit

import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Address (PaymentPrivateKey, PaymentPubKey)
import Ledger.Time (POSIXTime)
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract.Oracle (Observation (..), SignedMessage)
import Plutus.Contract.Oracle qualified as Oracle
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value, scale)

import Ledger.CardanoWallet qualified as CW
import Plutus.Contract.Test
import Plutus.Contracts.Future (Future (..), FutureAccounts (..), FutureError, FutureSchema, FutureSetup (..),
                                Role (..))
import Plutus.Contracts.Future qualified as F
import Plutus.Trace.Emulator (ContractHandle, EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream qualified as Stream
import Wallet.Emulator.Wallet qualified as Wallet

-- | 'CheckOptions' that assigns 1000 Ada to Wallets 1 and 2.
options :: CheckOptions
options = defaultCheckOptions
    & changeInitialWalletValue w1 (const $ Value.adaValueOf 1000)
    & changeInitialWalletValue w2 (const $ Value.adaValueOf 1000)
    & increaseTransactionLimits

tests :: TestTree
tests =
    testGroup "futures"
    [ checkPredicateOptions options "setup tokens"
        (assertDone (F.setupTokens @() @FutureSchema @FutureError)
                    (Trace.walletInstanceTag w1) (const True) "setupTokens")
        $ void F.setupTokensTrace

    , checkPredicateOptions options "can initialise and obtain tokens"
        (    walletFundsChangePlutus w1 ( scale (-1) (F.initialMargin $ theFuture startTime)
                                       <> F.tokenFor Short testAccounts
                                        )
        .&&. walletFundsChangePlutus w2 ( scale (-1) (F.initialMargin $ theFuture startTime)
                                       <> F.tokenFor Long testAccounts
                                        )
        )
        (void (initContract >> joinFuture))

    -- See Note [Oracle incorrect implementation]
    -- , checkPredicateOptions options "can increase margin"
    --     (assertAccountBalance (ftoShort testAccounts) (== Ada.lovelaceValueOf 2_936_000)
    --     .&&. assertAccountBalance (ftoLong testAccounts) (== Ada.lovelaceValueOf 8_310_000))
    --     increaseMarginTrace

    -- See Note [Oracle incorrect implementation]
    -- , checkPredicateOptions options "can settle early"
    --     (assertAccountBalance (ftoShort testAccounts) (== Ada.lovelaceValueOf 0)
    --     .&&. assertAccountBalance (ftoLong testAccounts) (== Ada.lovelaceValueOf 6_246_000)) -- 2 * 2 * (penalty + forwardPrice)
    --     settleEarlyTrace

    -- See Note [Oracle incorrect implementation]
    -- , checkPredicateOptions options "can pay out"
    --     (assertAccountBalance (ftoShort testAccounts) (== Ada.lovelaceValueOf 2_936_000)
    --     .&&. assertAccountBalance (ftoLong testAccounts) (== Ada.lovelaceValueOf 3_310_000))
    --     payOutTrace

    , goldenPir "test/Spec/future.pir" $$(PlutusTx.compile [|| F.futureStateMachine ||])

    , HUnit.testCaseSteps "script size is reasonable" $ \step ->
        reasonable' step (F.validator (theFuture startTime) testAccounts) 63000
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

setup :: POSIXTime -> FutureSetup
setup startTime =
    FutureSetup
        { shortPK = mockWalletPaymentPubKeyHash w1
        , longPK = mockWalletPaymentPubKeyHash w2
        , contractStart = startTime + 15000
        }

-- | A futures contract over 187 units with a forward price of 1233 Lovelace,
--   due at slot #100.
theFuture :: POSIXTime -> Future
theFuture startTime = Future {
    ftDeliveryDate  = startTime + 100000,
    ftUnits         = units,
    ftUnitPrice     = forwardPrice,
    ftInitialMargin = Ada.lovelaceValueOf 3_000_000,
    ftPriceOracle   = snd oracleKeys,
    ftMarginPenalty = penalty
    }

increaseMarginTrace :: EmulatorTrace ()
increaseMarginTrace = do
    _ <- initContract
    hdl2 <- joinFuture
    _ <- Trace.waitNSlots 20
    increaseMargin hdl2
    _ <- Trace.waitUntilSlot 100
    payOut hdl2

settleEarlyTrace :: EmulatorTrace ()
settleEarlyTrace = do
    _ <- initContract
    hdl2 <- joinFuture
    _ <- Trace.waitNSlots 20
    settleEarly hdl2

payOutTrace :: EmulatorTrace ()
payOutTrace = do
    _ <- initContract
    hdl2 <- joinFuture
    _ <- Trace.waitUntilSlot 100
    payOut hdl2

-- | After this trace, the initial margin of wallet 1, and the two tokens,
--   are locked by the contract.
initContract :: EmulatorTrace (ContractHandle () FutureSchema FutureError)
initContract = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    hdl1 <- Trace.activateContractWallet w1 (F.futureContract $ theFuture startTime)
    Trace.callEndpoint @"initialise-future" hdl1 (setup startTime, Short)
    _ <- Trace.waitNSlots 3
    pure hdl1

-- | Calls the "join-future" endpoint for wallet 2 and processes
--   all resulting transactions.
joinFuture :: EmulatorTrace (ContractHandle () FutureSchema FutureError)
joinFuture = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    hdl2 <- Trace.activateContractWallet w2 (F.futureContract $ theFuture startTime)
    Trace.callEndpoint @"join-future" hdl2 (testAccounts, setup startTime)
    _ <- Trace.waitNSlots 2
    pure hdl2

-- | Calls the "settle-future" endpoint for wallet 2 and processes
--   all resulting transactions.
payOut :: ContractHandle () FutureSchema FutureError -> EmulatorTrace ()
payOut hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let
        spotPrice = Ada.lovelaceValueOf 2_124_000
        ov = mkSignedMessage (ftDeliveryDate $ theFuture startTime) spotPrice
    Trace.callEndpoint @"settle-future" hdl ov
    void $ Trace.waitNSlots 2

-- | Margin penalty
penalty :: Value
penalty = Ada.lovelaceValueOf 1_000_000

-- | The forward price agreed at the beginning of the contract.
forwardPrice :: Value
forwardPrice = Ada.lovelaceValueOf 2_123_000

-- | How many units of the underlying asset are covered by the contract.
units :: Integer
units = 187

oracleKeys :: (PaymentPrivateKey, PaymentPubKey)
oracleKeys = (CW.paymentPrivateKey wllt, CW.paymentPubKey wllt) where
    wllt = CW.fromWalletNumber $ CW.WalletNumber 10

-- | Increase the margin of the 'Long' role by 100 lovelace
increaseMargin :: ContractHandle () FutureSchema FutureError -> EmulatorTrace ()
increaseMargin hdl = do
    Trace.callEndpoint @"increase-margin" hdl (Ada.lovelaceValueOf 5_000_000, Long)
    void $ Trace.waitNSlots 2

-- | Call 'settleEarly' with a high spot price (11240 lovelace)
settleEarly :: ContractHandle () FutureSchema FutureError -> EmulatorTrace ()
settleEarly hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let
        spotPrice = Ada.lovelaceValueOf 20_000_000
        ov = mkSignedMessage (startTime + 25000) spotPrice
    Trace.callEndpoint @"settle-early" hdl ov
    void $ Trace.waitNSlots 1

mkSignedMessage :: POSIXTime -> Value -> SignedMessage (Observation Value)
mkSignedMessage time vl = Oracle.signObservation' time vl (Ledger.unPaymentPrivateKey $ fst oracleKeys)

testAccounts :: FutureAccounts
testAccounts =
    let con = F.setupTokens @() @FutureSchema @FutureError
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag (Wallet.knownWallet 1))
        getOutcome (Folds.Done a) = a
        getOutcome e              = error $ "not finished: " <> show e
    in
    either (error . show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream (view emulatorConfig options) F.setupTokensTrace


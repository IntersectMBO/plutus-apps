{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Spec.Auction
    ( tests
    , options
    , auctionTrace1
    , auctionTrace2
    , AuctionModel
    , prop_Auction
    , prop_FinishAuction
    , prop_BidInEndSlot
    , prop_NoLockedFunds
    , prop_NoLockedFundsFast
    , prop_SanityCheckAssertions
    , prop_Whitelist
    , prop_CrashTolerance
    , prop_doubleSatisfaction
    , check_propAuctionWithCoverage
    ) where

import Control.Lens hiding (elements)
import Control.Monad (void, when)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as Freer
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import Data.Default (Default (def))
import Data.Monoid (Last (..))

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Generators qualified as Gen
import Cardano.Node.Emulator.Internal.Node (SlotConfig)
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger (Slot (..))
import Ledger qualified
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Contracts.Auction hiding (Bid)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream qualified as Stream

import Test.QuickCheck hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

slotCfg :: SlotConfig
slotCfg = def

params :: AuctionParams
params =
    AuctionParams
        { apOwner   = Ledger.toPlutusAddress $ mockWalletAddress w1
        , apAsset   = Value.fromCardanoValue theToken
        , apEndTime = TimeSlot.scSlotZeroTime slotCfg + 100000
        }

-- | The token that we are auctioning off.
theToken :: C.Value
theToken =
    -- This currency is created by the initial transaction.
    Gen.someTokenValue "token" 1

-- | 'CheckOptions' that includes 'theToken' in the initial distribution of Wallet 1.
options :: CheckOptions
options = defaultCheckOptionsContractModel
    & changeInitialWalletValue w1 ((<>) theToken)
    & (increaseTransactionLimits . increaseTransactionLimits)

seller :: Contract AuctionOutput SellerSchema AuctionError ()
seller = auctionSeller (apAsset params) (apEndTime params)

buyer :: ThreadToken -> Contract AuctionOutput BuyerSchema AuctionError ()
buyer cur = auctionBuyer cur params

trace1WinningBid :: Ada.Ada
trace1WinningBid = Ada.adaOf 50

auctionTrace1 :: Trace.EmulatorTrace ()
auctionTrace1 = do
    sellerHdl <- Trace.activateContractWallet w1 seller
    void $ Trace.waitNSlots 3
    currency <- extractAssetClass sellerHdl
    hdl2 <- Trace.activateContractWallet w2 (buyer currency)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 trace1WinningBid
    void $ Trace.waitUntilTime $ apEndTime params
    void $ Trace.waitNSlots 1

trace2WinningBid :: Ada.Ada
trace2WinningBid = Ada.adaOf 70

extractAssetClass :: Trace.ContractHandle AuctionOutput SellerSchema AuctionError -> Trace.EmulatorTrace ThreadToken
extractAssetClass handle = do
    t <- auctionThreadToken <$> Trace.observableState handle
    case t of
        Last (Just currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

auctionTrace2 :: Trace.EmulatorTrace ()
auctionTrace2 = do
    sellerHdl <- Trace.activateContractWallet w1 seller
    void $ Trace.waitNSlots 3
    currency <- extractAssetClass sellerHdl
    hdl2 <- Trace.activateContractWallet w2 (buyer currency)
    hdl3 <- Trace.activateContractWallet w3 (buyer currency)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 (Ada.adaOf 50)
    void $ Trace.waitNSlots 15
    Trace.callEndpoint @"bid" hdl3 (Ada.adaOf 60)
    void $ Trace.waitNSlots 35
    Trace.callEndpoint @"bid" hdl2 trace2WinningBid
    void $ Trace.waitUntilTime $ apEndTime params
    void $ Trace.waitNSlots 1

trace1FinalState :: AuctionOutput
trace1FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace1WinningBid
            , highestBidder = Ledger.toPlutusAddress $ mockWalletAddress w2
            }
        , auctionThreadToken = Last $ Just threadToken
        }

trace2FinalState :: AuctionOutput
trace2FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace2WinningBid
            , highestBidder = Ledger.toPlutusAddress $ mockWalletAddress w2
            }
        , auctionThreadToken = Last $ Just threadToken
        }

threadToken :: ThreadToken
threadToken =
    let con = getThreadToken :: Contract AuctionOutput SellerSchema AuctionError ThreadToken
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag w1)
        getOutcome (Folds.Done a) = a
        getOutcome e              = error $ "not finished: " <> show e
    in
    either (error . show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream (options ^. emulatorConfig)
        $ do
            void $ Trace.activateContractWallet w1 (void con)
            Trace.waitNSlots 3

-- * QuickCheck model

data AuctionModel = AuctionModel
    { _currentBid :: Integer
    , _winner     :: Wallet
    , _endSlot    :: Slot
    , _phase      :: Phase
    } deriving (Show, Eq, Generic)

data Phase = NotStarted | Bidding | AuctionOver
    deriving (Eq, Show, Generic)

makeLenses 'AuctionModel

deriving instance Eq (ContractInstanceKey AuctionModel w s e params)
deriving instance Show (ContractInstanceKey AuctionModel w s e params)

instance ContractModel AuctionModel where

    data ContractInstanceKey AuctionModel w s e params where
        SellerH :: ContractInstanceKey AuctionModel AuctionOutput SellerSchema AuctionError ()
        BuyerH  :: Wallet -> ContractInstanceKey AuctionModel AuctionOutput BuyerSchema AuctionError ()

    data Action AuctionModel = Init | Bid Wallet Integer | WaitForDeadline
        deriving (Eq, Show, Generic)

    initialState = AuctionModel
        { _currentBid = 0
        , _winner     = w1
        , _endSlot    = TimeSlot.posixTimeToEnclosingSlot def $ apEndTime params
        , _phase      = NotStarted
        }

    initialInstances = [ StartContract (BuyerH w) () | w <- [w2, w3, w4] ]

    startInstances _ Init = [StartContract SellerH ()]
    startInstances _ _    = []

    instanceWallet SellerH    = w1
    instanceWallet (BuyerH w) = w

    instanceContract _ SellerH  _ = seller
    instanceContract _ BuyerH{} _ = buyer threadToken

    arbitraryAction s
        | p /= NotStarted = do
            oneof $ pure WaitForDeadline : [ Bid w <$> validBid
                                           | w <- [w2, w3, w4] ]

        | otherwise = pure $ Init
        where
            p    = s ^. contractState . phase
            b    = s ^. contractState . currentBid
            validBid = choose ((b+1) `max` Ada.getLovelace (Ada.adaOf 2),
                               b + Ada.getLovelace (Ada.adaOf 100))

    precondition s Init = s ^. contractState . phase == NotStarted
    precondition s (Bid _ bid) =
      -- In order to place a bid, we need to satisfy the constraint where
      -- each tx output must have at least N Ada.
      s ^. contractState . phase /= NotStarted &&
      bid >= Ada.getLovelace (Ada.adaOf 2) &&
      bid > s ^. contractState . currentBid
    precondition s WaitForDeadline = s ^. currentSlot < s ^. contractState . endSlot

    nextReactiveState slot' = do
      end  <- viewContractState endSlot
      p    <- viewContractState phase
      when (slot' > end && p == Bidding) $ do
        w   <- viewContractState winner
        bid <- viewContractState currentBid
        phase .= AuctionOver
        deposit w theToken
        deposit w1 $ Ada.lovelaceValueOf bid
        {-
        w1change <- viewModelState $ balanceChange w1  -- since the start of the test
        assertSpec ("w1 final balance is wrong:\n  "++show w1change) $
          w1change == toSymValue (inv theToken <> Ada.lovelaceValueOf bid) ||
          w1change == mempty
        -}

    nextState cmd = do
        case cmd of
            Init -> do
                phase .= Bidding
                withdraw w1 theToken
                wait 3
            Bid w bid -> do
                currentPhase <- viewContractState phase
                end          <- viewContractState endSlot
                slot'        <- viewModelState currentSlot
                when (slot' < end && currentPhase == Bidding) $ do
                    current <- viewContractState currentBid
                    leader  <- viewContractState winner
                    withdraw w $ Ada.lovelaceValueOf bid
                    deposit leader $ Ada.lovelaceValueOf current
                    currentBid .= bid
                    winner     .= w
                wait 2
            WaitForDeadline -> do
              end <- viewContractState endSlot
              slot <- viewModelState currentSlot
              wait (fromIntegral $ end - slot)

    perform _      _ _ Init        = delay 3
    perform handle _ _ (Bid w bid) = do
        -- FIXME: You cannot bid in certain slots when the off-chain code is busy, so to make the
        --        tests pass we send two identical bids in consecutive slots. The off-chain code is
        --        never busy for more than one slot at a time so at least one of the bids is
        --        guaranteed to go through. If both reaches the off-chain code the second will be
        --        discarded since it's not higher than the current highest bid.
        Trace.callEndpoint @"bid" (handle $ BuyerH w) (Ada.lovelaceOf bid)
        delay 1
        Trace.callEndpoint @"bid" (handle $ BuyerH w) (Ada.lovelaceOf bid)
        delay 1
    perform _ _ s WaitForDeadline = delay (fromIntegral $ s ^. contractState . endSlot - s ^. currentSlot)

    shrinkAction _ Init            = []
    shrinkAction _ (Bid w v)       = WaitForDeadline : [ Bid w v' | v' <- shrink v ]
    shrinkAction _ WaitForDeadline = []

    monitoring _ (Bid _ bid) =
      classify (Ada.lovelaceOf bid == Ada.adaOf 100 - (Ledger.minAdaTxOutEstimated <> Ledger.maxFee))
        "Maximum bid reached"
    monitoring _ _ = id

prop_Auction :: Actions AuctionModel -> Property
prop_Auction script =
    propRunActionsWithOptions (set minLogLevel Info options) defaultCoverageOptions
        (\ _ -> pure True)  -- TODO: check termination
        script

finishAuction :: DL AuctionModel ()
finishAuction = do
    anyActions_
    finishingStrategy
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: DL AuctionModel ()
finishingStrategy = do
    slot <- viewModelState currentSlot
    end  <- viewContractState endSlot
    when (slot <= end) $ waitUntilDL (end + 1)

prop_FinishAuction :: Property
prop_FinishAuction = forAllDL finishAuction prop_Auction

-- Test bidding in the end slot. The model says this should not work.
noBidInEndSlot :: DL AuctionModel ()
noBidInEndSlot = do
  action Init
  anyActions_
  slot <- viewModelState currentSlot
  end  <- viewContractState endSlot
  when (slot < end) $ do
    waitUntilDL end
    action $ Bid w2 1_000_000_000

prop_BidInEndSlot :: Property
prop_BidInEndSlot = forAllDL noBidInEndSlot prop_Auction

-- | This does not hold! The Payout transition is triggered by the sellers off-chain code, so if the
--   seller walks away the buyer will not get their token (unless going around the off-chain code
--   and building a Payout transaction manually).
noLockProof :: NoLockedFundsProof AuctionModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy
  , nlfpWalletStrategy = const finishingStrategy }

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProofWithOptions (set minLogLevel Critical options) noLockProof

prop_NoLockedFundsFast :: Property
prop_NoLockedFundsFast = checkNoLockedFundsProofFast noLockProof

prop_SanityCheckAssertions :: Actions AuctionModel -> Property
prop_SanityCheckAssertions = propSanityCheckAssertions

prop_Whitelist :: Actions AuctionModel -> Property
prop_Whitelist = checkErrorWhitelist defaultWhitelist

instance CrashTolerance AuctionModel where
  available (Bid w _) alive   = (Key $ BuyerH  w) `elem` alive
  available Init      _       = True
  available WaitForDeadline _ = True

  restartArguments _ BuyerH{}  = ()
  restartArguments _ SellerH{} = ()

prop_CrashTolerance :: Actions (WithCrashTolerance AuctionModel) -> Property
prop_CrashTolerance =
  propRunActionsWithOptions (set minLogLevel Critical options) defaultCoverageOptions
        (\ _ -> pure True)

check_propAuctionWithCoverage :: IO ()
check_propAuctionWithCoverage = do
  cr <- quickCheckWithCoverage stdArgs (set coverageIndex covIdx $ defaultCoverageOptions) $ \covopts ->
    withMaxSuccess 1000 $
      propRunActionsWithOptions @AuctionModel
        (set minLogLevel Critical options) covopts (const (pure True))
  writeCoverageReport "Auction" cr

-- | Note: this property fails, because the contract is vulnerable to double satisfaction.
prop_doubleSatisfaction :: Actions AuctionModel -> Property
prop_doubleSatisfaction = checkDoubleSatisfactionWithOptions options defaultCoverageOptions

tests :: TestTree
tests =
    testGroup "auction"
        [ checkPredicateOptions options "run an auction"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace1FinalState ) "wallet 2 final state should be OK"
            .&&. walletFundsChange w1 (Value.adaToCardanoValue trace1WinningBid <> C.negateValue theToken)
            .&&. walletFundsChange w2 (Value.adaToCardanoValue (-trace1WinningBid) <> theToken))
            auctionTrace1
        , checkPredicateOptions options "run an auction with multiple bids"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w3) (const True) "3rd party should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace2FinalState) "wallet 2 final state should be OK"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w3) ((==) trace2FinalState) "wallet 3 final state should be OK"
            .&&. walletFundsChange w1 (Value.adaToCardanoValue trace2WinningBid <> C.negateValue theToken)
            .&&. walletFundsChange w2 (Value.adaToCardanoValue (-trace2WinningBid) <> theToken)
            .&&. walletFundsChange w3 mempty)
            auctionTrace2
        , testProperty "QuickCheck property FinishAuction" prop_FinishAuction
        , testProperty "QuickCheck property Auction" prop_Auction
        , testProperty "NLFP fails" $ expectFailure $ noShrinking prop_NoLockedFunds
        , testProperty "prop_Reactive" $
            withMaxSuccess 1000 (propSanityCheckReactive @AuctionModel)
        , testProperty "prop_doubleSatisfaction fails" $
            expectFailure $ noShrinking prop_doubleSatisfaction
        ]


{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
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
    , prop_NoLockedFunds
    ) where

import Control.Lens hiding (elements)
import Control.Monad (void, when)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as Freer
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import Data.Data
import Data.Default (Default (def))
import Data.Monoid (Last (..))

import Ledger (Ada, Slot (..), Value)
import Ledger.Ada qualified as Ada
import Ledger.Generators (someTokenValue)
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test hiding (not)
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream qualified as Stream

import Ledger qualified
import Ledger.TimeSlot (SlotConfig)
import Ledger.TimeSlot qualified as TimeSlot
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Auction hiding (Bid)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

slotCfg :: SlotConfig
slotCfg = def

params :: AuctionParams
params =
    AuctionParams
        { apOwner   = mockWalletPaymentPubKeyHash w1
        , apAsset   = theToken
        , apEndTime = TimeSlot.scSlotZeroTime slotCfg + 100000
        }

-- | The token that we are auctioning off.
theToken :: Value
theToken =
    -- This currency is created by the initial transaction.
    someTokenValue "token" 1

-- | 'CheckOptions' that includes 'theToken' in the initial distribution of Wallet 1.
options :: CheckOptions
options = defaultCheckOptionsContractModel
    & changeInitialWalletValue w1 ((<>) theToken)

seller :: Contract AuctionOutput SellerSchema AuctionError ()
seller = auctionSeller (apAsset params) (apEndTime params)

buyer :: ThreadToken -> Contract AuctionOutput BuyerSchema AuctionError ()
buyer cur = auctionBuyer cur params

trace1WinningBid :: Ada
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

trace2WinningBid :: Ada
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
            , highestBidder = mockWalletPaymentPubKeyHash w2
            }
        , auctionThreadToken = Last $ Just threadToken
        }

trace2FinalState :: AuctionOutput
trace2FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace2WinningBid
            , highestBidder = mockWalletPaymentPubKeyHash w2
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
    } deriving (Show, Eq, Data)

data Phase = NotStarted | Bidding | AuctionOver
    deriving (Eq, Show, Data)

makeLenses 'AuctionModel

deriving instance Eq (ContractInstanceKey AuctionModel w s e params)
deriving instance Show (ContractInstanceKey AuctionModel w s e params)

instance ContractModel AuctionModel where

    data ContractInstanceKey AuctionModel w s e params where
        SellerH :: ContractInstanceKey AuctionModel AuctionOutput SellerSchema AuctionError ()
        BuyerH  :: Wallet -> ContractInstanceKey AuctionModel AuctionOutput BuyerSchema AuctionError ()

    data Action AuctionModel = Init Wallet | Bid Wallet Integer
        deriving (Eq, Show, Data)

    initialState = AuctionModel
        { _currentBid = 0
        , _winner     = w1
        , _endSlot    = TimeSlot.posixTimeToEnclosingSlot def $ apEndTime params
        , _phase      = NotStarted
        }

    initialInstances = StartContract SellerH () : [ StartContract (BuyerH w) () | w <- [w2, w3, w4] ]

    instanceWallet SellerH    = w1
    instanceWallet (BuyerH w) = w

    instanceContract _ SellerH  _ = seller
    instanceContract _ BuyerH{} _ = buyer threadToken

    arbitraryAction s
        | p /= NotStarted = do
            oneof [ Bid w <$> chooseBid (lo,hi)
                  | w <- [w2, w3, w4]
                  , let (lo,hi) = validBidRange s w
                  , lo <= hi ]
        | otherwise = pure $ Init w1
        where
            p    = s ^. contractState . phase

    waitProbability s
      | s ^. contractState . phase /= NotStarted
      , all (uncurry (>) . validBidRange s) [w2, w3, w4] = 1
      | otherwise = 0.1

    precondition s (Init _) = s ^. contractState . phase == NotStarted
    precondition s cmd      = s ^. contractState . phase /= NotStarted &&
        case cmd of
            -- In order to place a bid, we need to satisfy the constraint where
            -- each tx output must have at least N Ada.
            Bid w bid      -> let (lo,hi) = validBidRange s w in
                              lo <= bid && bid <= hi
            _              -> True

    nextReactiveState slot' = do
      end  <- viewContractState endSlot
      p    <- viewContractState phase
      when (slot' >= end && p == Bidding) $ do
        w   <- viewContractState winner
        bid <- viewContractState currentBid
        phase .= AuctionOver
        deposit w $ Ada.toValue Ledger.minAdaTxOut <> theToken
        deposit w1 $ Ada.lovelaceValueOf bid

    -- This command is only for setting up the model state with theToken
    nextState cmd = do
        slot <- viewModelState currentSlot
        end  <- viewContractState endSlot
        case cmd of
            Init _ -> do
                phase .= Bidding
                withdraw w1 $ Ada.toValue Ledger.minAdaTxOut <> theToken
                wait 3
            Bid w bid -> do
                current <- viewContractState currentBid
                leader  <- viewContractState winner
                when (slot < end) $ do
                    withdraw w $ Ada.lovelaceValueOf bid
                    deposit leader $ Ada.lovelaceValueOf current
                    currentBid .= bid
                    winner     .= w
                wait 2

    perform _ _ _ (Init _) = delay 3
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

    shrinkAction _ (Init _)  = []
    shrinkAction _ (Bid w v) = [ Bid w v' | v' <- shrink v ]

    monitoring _ (Bid _ bid) =
      classify (Ada.lovelaceOf bid == Ada.adaOf 100 - (Ledger.minAdaTxOut <> Ledger.maxFee))
        "Maximum bid reached"
    monitoring _ _ = id

-- In order to place a bid, we need to satisfy the constraint where
-- each tx output must have at least N Ada.
--
-- When we bid, we must make sure that we don't bid too high such
-- that:
--     - we can't pay for fees anymore
--     - we have a tx output of less than N Ada.
--
-- We suppose the initial balance is 100 Ada. Needs to be changed if
-- the emulator initialises the wallets with a different value.
validBidRange :: ModelState AuctionModel -> Wallet -> (Integer,Integer)
validBidRange s _w =
  let currentWalletBalance = Ada.adaOf 100  -- this is approximate
      current = s ^. contractState . currentBid
  in ( (current+1) `max` Ada.getLovelace Ledger.minAdaTxOut,
       Ada.getLovelace (currentWalletBalance - (Ledger.minAdaTxOut <> Ledger.maxFee))
     )

-- When we choose a bid, we prefer a lower bid to a higher
-- one. Otherwise longer tests very often reach the maximum possible
-- bid, which makes little sense.
chooseBid :: (Integer,Integer) -> Gen Integer
chooseBid (lo,hi)
  | lo==hi = pure lo
  | lo <hi = oneof [choose (lo,lo+k) | k <- takeWhile (>0) (iterate (`div` 400) (hi-lo))]
  | otherwise = error $ "chooseBid "++show (lo,hi)

prop_Auction :: Actions AuctionModel -> Property
prop_Auction script =
    propRunActionsWithOptions (set minLogLevel Info options) defaultCoverageOptions
        (\ _ -> pure True)  -- TODO: check termination
        script

finishAuction :: DL AuctionModel ()
finishAuction = do
    action $ Init w1
    anyActions_
    slot <- viewModelState currentSlot
    when (slot < 101) $ waitUntilDL 101
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

prop_FinishAuction :: Property
prop_FinishAuction = forAllDL finishAuction prop_Auction

-- | This does not hold! The Payout transition is triggered by the sellers off-chain code, so if the
--   seller walks away the buyer will not get their token (unless going around the off-chain code
--   and building a Payout transaction manually).
noLockProof :: NoLockedFundsProof AuctionModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = strat
  , nlfpWalletStrategy = const strat }
  where
    strat = do
      p <- viewContractState phase
      when (p == NotStarted) $ action $ Init w1
      slot <- viewModelState currentSlot
      when (slot < 101) $ waitUntilDL 101

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProof (set minLogLevel Critical options) noLockProof

tests :: TestTree
tests =
    testGroup "auction"
        [ checkPredicateOptions options "run an auction"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace1FinalState ) "wallet 2 final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.toValue trace1WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (Ada.toValue Ledger.minAdaTxOut <> inv (Ada.toValue trace1WinningBid) <> theToken))
            auctionTrace1
        , checkPredicateOptions options "run an auction with multiple bids"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w3) (const True) "3rd party should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace2FinalState) "wallet 2 final state should be OK"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w3) ((==) trace2FinalState) "wallet 3 final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.toValue trace2WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (Ada.toValue Ledger.minAdaTxOut <> inv (Ada.toValue trace2WinningBid) <> theToken)
            .&&. walletFundsChange w3 mempty)
            auctionTrace2
        , testProperty "QuickCheck property" $
            withMaxSuccess 10 prop_FinishAuction
        , testProperty "NLFP fails" $
            expectFailure $ noShrinking prop_NoLockedFunds
        , testProperty "prop_Reactive" $
            withMaxSuccess 1000 (propSanityCheckReactive @AuctionModel)
        ]

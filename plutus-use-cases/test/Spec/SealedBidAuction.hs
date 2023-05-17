{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE NumericUnderscores #-}
module Spec.SealedBidAuction where

import Control.Lens hiding (elements)
import Control.Monad (when)
import Data.Default (Default (def))

import Cardano.Node.Emulator.Generators (someTokenValue)
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger (Slot (..))
import Ledger qualified
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract.Secrets
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.SealedBidAuction
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value)
import Plutus.Trace.Emulator qualified as Trace

import Test.QuickCheck hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- | The token that we are auctioning off.
theTokenC :: Value.Value
theTokenC = someTokenValue "token" 1

theToken :: Value
theToken = Value.fromCardanoValue theTokenC

-- | 'CheckOptions' that includes 'theToken' in the initial distribution of Wallet 1.
options :: CheckOptions
options = defaultCheckOptionsContractModel
    & changeInitialWalletValue w1 ((<>) theTokenC)

-- * QuickCheck model

data AuctionModel = AuctionModel
    { _currentBids       :: [(Integer, Wallet)]
    , _currentWinningBid :: Maybe (Integer, Wallet)
    , _endBidSlot        :: Slot
    , _payoutSlot        :: Slot
    , _phase             :: Phase }
    deriving (Show, Generic)

data Phase = NotStarted | Bidding | AwaitingPayout | PayoutTime | AuctionOver
    deriving (Eq, Show, Generic)

makeLenses 'AuctionModel

deriving instance Eq (ContractInstanceKey AuctionModel w s e params)
deriving instance Show (ContractInstanceKey AuctionModel w s e params)

instance ContractModel AuctionModel where

    data ContractInstanceKey AuctionModel w s e params where
        SellerH :: ContractInstanceKey AuctionModel () SellerSchema AuctionError AuctionParams
        BidderH :: Wallet -> ContractInstanceKey AuctionModel () BidderSchema AuctionError AuctionParams

    data Action AuctionModel = Init Slot Slot
                             | Bid Wallet Integer
                             | Reveal Wallet Integer
                             | Payout Wallet
        deriving (Eq, Show, Generic)

    initialState = AuctionModel
        { _currentBids       = []
        , _currentWinningBid = Nothing
        , _endBidSlot        = 0
        , _payoutSlot        = 0
        , _phase             = NotStarted
        }

    initialInstances = []

    startInstances _ (Init (Slot ebS) (Slot pS)) =
      let params =  AuctionParams
            { apOwner      = Ledger.toPlutusAddress $ mockWalletAddress w1
            , apAsset      = theToken
            , apEndTime    = TimeSlot.scSlotZeroTime def + fromInteger (ebS*1000)
            , apPayoutTime = TimeSlot.scSlotZeroTime def + fromInteger (pS*1000)
            }
      in StartContract SellerH params : [ StartContract (BidderH w) params | w <- [w2, w3, w4] ]
    startInstances _ _             = []

    instanceWallet SellerH     = w1
    instanceWallet (BidderH w) = w

    instanceContract _ SellerH   params = sellerContract params
    instanceContract _ BidderH{} params = bidderContract params

    arbitraryAction s
        | p /= NotStarted =
            frequency$[ (40, Bid  <$> elements [w2, w3, w4] <*> choose (2_000_000, 100_000_000))
                      -- Random reveal
                      , (20, Reveal <$> elements [w2, w3, w4] <*> choose (2_000_000, 100_000_000))
                      ] ++
                      -- Correct reveal
                      [ (20, uncurry Reveal <$> elements [ (w,i) | (i,w) <- s ^. contractState . currentBids ])
                      | not (null (s ^. contractState . currentBids))
                      ] ++
                      [ (20, Payout <$> elements [w1, w2, w3, w4]) ]
        | otherwise = do
            endTime    <- choose (20, 50)
            payoutTime <- choose (endTime+1, 70)
            return $ Init (Slot endTime) (Slot payoutTime)
        where
            p    = s ^. contractState . phase

    arbitraryWaitInterval s = frequency $ [(1, Slot <$> choose (1, 3))] ++
                                          [(5, pure $ s ^. contractState . endBidSlot - s ^. currentSlot)
                                          | s ^. contractState . endBidSlot > s ^. currentSlot ] ++
                                          [(5, pure $ s ^. contractState . payoutSlot - s ^. currentSlot)
                                          | s ^. contractState . payoutSlot > s ^. currentSlot ]

    precondition s cmd  =
        case cmd of
            -- TODO: do you want to require that the payout time is after the end bidding time?
            -- maybe not?
            Init{}         -> s ^. contractState . phase == NotStarted

            Bid w v        -> s ^. contractState . phase == Bidding
                           && w `notElem` fmap snd (s ^. contractState . currentBids)
                           && v >= 2_000_000

            Reveal _ v     -> s ^. contractState . phase == AwaitingPayout
                           && v >= 2_000_000

            Payout _       -> s ^. contractState . phase == PayoutTime

    perform _ _ _ (Init _ _)        = delay 3
    perform handle _ _ (Bid w bid)  = do
        Trace.callEndpoint @"bid" (handle $ BidderH w) (BidArgs (secretArg bid))
        delay 1
    perform handle _ _ (Reveal w bid) = do
        Trace.callEndpoint @"reveal" (handle $ BidderH w) (RevealArgs bid)
        delay 1
    perform handle _ _ (Payout w)
      | w == w1 = do
        Trace.callEndpoint @"payout" (handle SellerH) ()
        delay 1
      | otherwise = do
        Trace.callEndpoint @"payout" (handle $ BidderH w) ()
        delay 1

    shrinkAction _ (Bid w v)    = [ Bid w v' | v' <- shrink v ]
    shrinkAction _ (Reveal w v) = [ Reveal w v' | v' <- shrink v ]
    shrinkAction _ _            = []

    monitoring (s,_) (Reveal w bid)
      | (bid,w) `elem` bids   = tabulate "Reveals" ["honest"]
      | w `elem` map snd bids = tabulate "Reveals" ["dishonest"]
      | otherwise             = tabulate "Reveals" ["no bid"]
      where bids = s ^. contractState . currentBids
    monitoring _ _ = id

    -- This command is only for setting up the model state with theToken
    nextState cmd = do
        currentPhase <- viewContractState phase
        case cmd of
            Init ebS pS -> do
                endBidSlot .= ebS
                payoutSlot .= pS
                withdraw w1 theToken
                phase .= Bidding
                wait 3

            Bid w bid | currentPhase == Bidding -> do
                  bids <- viewContractState currentBids
                  when (w `notElem` fmap snd bids) $ do
                    currentBids .= ((bid, w):bids)
                  wait 1

            Reveal w bid | currentPhase == AwaitingPayout -> do
                bids <- viewContractState currentBids
                mwinningBid <- viewContractState currentWinningBid
                case mwinningBid of
                  Just (oldBid, w') ->
                    when ((bid, w) `elem` bids && bid > oldBid) $ do
                      withdraw w $ Ada.lovelaceValueOf bid
                      deposit w' $ Ada.lovelaceValueOf oldBid
                      currentWinningBid .= Just (bid, w)
                  Nothing ->
                    when ((bid, w) `elem` bids) $ do
                      withdraw w $ Ada.lovelaceValueOf bid
                      currentWinningBid .= Just (bid, w)
                wait 1

            Payout _ | currentPhase == PayoutTime -> do
                  mwinningBid <- viewContractState currentWinningBid
                  case mwinningBid of
                    Just (bid, winner) -> do
                      deposit winner theToken
                      deposit w1 $ Ada.lovelaceValueOf bid

                    Nothing -> deposit w1 theToken
                  wait 1
                  phase .= AuctionOver

            _ -> pure ()

    nextReactiveState slot = do
          currentPhase <- viewContractState phase
          when (currentPhase `notElem` [NotStarted, AuctionOver]) $ do
            eSlot      <- viewContractState endBidSlot
            pSlot <- viewContractState payoutSlot
            when (slot >= eSlot) $ do
              phase .= AwaitingPayout
            when (slot >= pSlot) $ do
              phase .= PayoutTime

prop_Auction :: Actions AuctionModel -> Property
prop_Auction = propRunActionsWithOptions options defaultCoverageOptions (\ _ -> pure True)

finishAuction :: DL AuctionModel ()
finishAuction = do
    anyActions_
    finishingStrategy w1
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: Wallet -> DL AuctionModel ()
finishingStrategy w = do
    slot <- viewModelState currentSlot
    payday <- viewContractState payoutSlot
    when (slot < payday) $ waitUntilDL payday
    currentPhase <- viewContractState phase
    when (currentPhase == PayoutTime) $ action $ Payout w

prop_FinishAuction :: Property
prop_FinishAuction = forAllDL finishAuction prop_Auction

noLockProof :: NoLockedFundsProof AuctionModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy w1
  , nlfpWalletStrategy = finishingStrategy }

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProofWithOptions options noLockProof

tests :: TestTree
tests =
  testGroup "sealed bid auction"
    [ testProperty "packInteger is injective" $ \x y -> x /= y ==> packInteger x /= packInteger y
    , testProperty "prop_AuctionModelCorrect" prop_Auction
    ]

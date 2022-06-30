{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Spec.Escrow( tests
                  , redeemTrace
                  , redeem2Trace
                  , refundTrace
                  , prop_Escrow
                  , prop_Escrow_DoubleSatisfaction
                  , prop_FinishEscrow
                  , prop_NoLockedFunds
                  , EscrowModel) where

import Control.Lens hiding (both)
import Control.Monad (void, when)
import Data.Data
import Data.Default (Default (def))
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (Slot (..), minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Time (POSIXTime)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel

import Plutus.Contracts.Escrow hiding (Action (..))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck hiding ((.&&.))

import Spec.Escrow.Endpoints

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value
                               , _refundSlot    :: Slot
                               , _targets       :: Map Wallet Value
                               } deriving (Eq, Show, Data)

makeLenses ''EscrowModel

modelParams :: EscrowParams d
modelParams = escrowParams $ TimeSlot.scSlotZeroTime def

options :: CheckOptions
options = defaultCheckOptionsContractModel & allowBigTransactions

deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where
  data Action EscrowModel = Pay Wallet Integer
                          | Redeem Wallet
                          | Refund Wallet
                          | BadRefund Wallet Wallet
                          deriving (Eq, Show, Data)

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowTestSchema EscrowError ()

  initialState = EscrowModel { _contributions = Map.empty
                             , _refundSlot    = TimeSlot.posixTimeToEnclosingSlot def
                                              . escrowDeadline
                                              $ modelParams
                             -- TODO: This model is somewhat limited because we focus on one
                             -- set of parameters only. The solution is to use the sealed bid
                             -- auction trick to generate parameters dynamically that we can
                             -- use later on.
                             , _targets       = Map.fromList [ (w1, Ada.adaValueOf 10)
                                                             , (w2, Ada.adaValueOf 20)
                                                             ]
                             }

  initialInstances = (`StartContract` ()) . WalletKey <$> testWallets

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} _ = testContract
    where
      -- TODO: Lazy test contract for now
      testContract = selectList [ void $ payEp modelParams
                                , void $ redeemEp modelParams
                                , void $ refundEp modelParams
                                , void $ badRefundEp modelParams
                                ] >> testContract

  nextState a = void $ case a of
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      let leftoverValue = fold contribs <> inv (fold targets)
      deposit w leftoverValue
      contributions .= Map.empty
      wait 1
    Refund w -> do
      v <- viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      deposit w v
      wait 1
    BadRefund _ _ -> do
      wait 2

  precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
             && (s ^. currentSlot < s ^. contractState . refundSlot - 1)
    Refund w -> s ^. currentSlot >= s ^. contractState . refundSlot
             && Nothing /= (s ^. contractState . contributions . at w)
    Pay _ v -> s ^. currentSlot + 1 < s ^. contractState . refundSlot
            && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut
    BadRefund w w' -> s ^. currentSlot < s ^. contractState . refundSlot - 2  -- why -2?
                   || w /= w'

  perform h _ _ a = case a of
    Pay w v        -> do
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.adaValueOf $ fromInteger v)
      delay 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      delay 1
    Refund w       -> do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      delay 1
    BadRefund w w' -> do
      Trace.callEndpoint @"badrefund-escrow" (h $ WalletKey w) (mockWalletPaymentPubKeyHash w')
      delay 2

  arbitraryAction s = frequency $ [ (prefer beforeRefund,  Pay <$> QC.elements testWallets <*> choose @Integer (10, 30))
                                  , (prefer beforeRefund,  Redeem <$> QC.elements testWallets)
                                  , (prefer afterRefund,   BadRefund <$> QC.elements testWallets <*> QC.elements testWallets) ] ++
                                  [ (prefer afterRefund,   Refund <$> QC.elements (s ^. contractState . contributions . to Map.keys))
                                  | Prelude.not . null $ s ^. contractState . contributions . to Map.keys ]
                  where
                    slot = s ^. currentSlot
                    beforeRefund = slot < s ^. contractState . refundSlot
                    afterRefund = Prelude.not beforeRefund
                    prefer b = if b then 10 else 1

  monitoring _ (Redeem _) = classify True "Contains Redeem"
  monitoring (_,_) (BadRefund w w') = tabulate "Bad refund attempts" [if w==w' then "early refund" else "steal refund"]
  monitoring (s,s') _ = classify (redeemable s' && Prelude.not (redeemable s)) "Redeemable"
    where redeemable s = precondition s (Redeem undefined)

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5] -- removed five to increase collisions (, w6, w7, w8, w9, w10])

prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActionsWithOptions options defaultCoverageOptions (\ _ -> pure True)

prop_Escrow_DoubleSatisfaction :: Actions EscrowModel -> Property
prop_Escrow_DoubleSatisfaction = checkDoubleSatisfactionWithOptions options defaultCoverageOptions

finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy (const True)
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: (Wallet -> Bool) -> DL EscrowModel ()
finishingStrategy walletAlive = do
    now <- viewModelState currentSlot
    slot <- viewContractState refundSlot
    when (now < slot+1) $ waitUntilDL $ slot+1
    contribs <- viewContractState contributions
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs, walletAlive w]

prop_FinishEscrow :: Property
prop_FinishEscrow = forAllDL finishEscrow prop_Escrow

noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy (const True)
  , nlfpWalletStrategy = finishingStrategy . (==) }

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProofWithOptions options noLockProof


tests :: TestTree
tests = testGroup "escrow"
    [ let con = void $ payEp @() @EscrowSchema @EscrowError (escrowParams startTime) in
      checkPredicateOptions options "can pay"
        ( assertDone con (Trace.walletInstanceTag w1) (const True) "escrow pay not done"
        .&&. walletFundsChange w1 (Ada.adaValueOf (-10))
        )
        $ do
          hdl <- Trace.activateContractWallet w1 con
          Trace.callEndpoint @"pay-escrow" hdl (Ada.adaValueOf 10)
          void $ Trace.waitNSlots 1

    , let con = void $ selectEither (payEp @()
                                           @EscrowSchema
                                           @EscrowError
                                           (escrowParams startTime))
                                    (redeemEp (escrowParams startTime)) in
      checkPredicateOptions options "can redeem"
        ( assertDone con (Trace.walletInstanceTag w3) (const True) "escrow redeem not done"
          .&&. walletFundsChange w1 (Ada.adaValueOf (-10))
          .&&. walletFundsChange w2 (Ada.adaValueOf 10)
          .&&. walletFundsChange w3 mempty
        )
        redeemTrace

    , checkPredicateOptions options "can redeem even if more money than required has been paid in"

          -- in this test case we pay in a total of 40 lovelace (10 more than required), for
          -- the same contract as before, requiring 10 lovelace to go to wallet 1 and 20 to
          -- wallet 2.
          --
          -- The scenario is
          -- * Wallet 1 contributes 20
          -- * Wallet 2 contributes 10
          -- * Wallet 3 contributes 10
          -- * Wallet 1 is going to redeem the payments
          --

          -- Wallet 1 pays 20 and receives 10 from the escrow contract and another 10
          -- in excess inputs
          ( walletFundsChange w1 (Ada.lovelaceValueOf 0)

          -- Wallet 2 pays 10 and receives 20, as per the contract.
            .&&. walletFundsChange w2 (Ada.adaValueOf 10)

          -- Wallet 3 pays 10 and doesn't receive anything.
            .&&. walletFundsChange w3 (Ada.adaValueOf (-10))
          )
          redeem2Trace

    , let con = void (payEp @()
                            @EscrowSchema
                            @EscrowError
                            (escrowParams startTime))
             <> void (refundEp (escrowParams startTime)) in
      checkPredicateOptions options "can refund"
        ( walletFundsChange w1 mempty
          .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "refund should succeed")
        refundTrace

    , HUnit.testCaseSteps "script size is reasonable"
        $ \step -> reasonable' step
                               (Scripts.validatorScript $ typedValidator (escrowParams startTime))
                               32000

    , testProperty "QuickCheck ContractModel" $ withMaxSuccess 10 prop_Escrow
    , testProperty "QuickCheck NoLockedFunds" $ withMaxSuccess 10 prop_NoLockedFunds
    , testProperty "QuickCheck double satisfaction fails" $ expectFailure (noShrinking prop_Escrow_DoubleSatisfaction)
    ]
    where
        startTime = TimeSlot.scSlotZeroTime def

escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 40000
    , escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w1) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 20)
        ]
    }

-- | Wallets 1 and 2 pay into an escrow contract, wallet 3
--   cashes out.
redeemTrace :: Trace.EmulatorTrace ()
redeemTrace = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let con = void $ selectEither (payEp @()
                                         @EscrowSchema
                                         @EscrowError
                                         (escrowParams startTime))
                                  (redeemEp (escrowParams startTime))
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    hdl3 <- Trace.activateContractWallet w3 con

    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
    Trace.callEndpoint @"pay-escrow" hdl2 (Ada.adaValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"redeem-escrow" hdl3 ()
    void $ Trace.waitNSlots 1

-- | Wallets 1-3 pay into an escrow contract, wallet 1 redeems.
redeem2Trace :: Trace.EmulatorTrace ()
redeem2Trace = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let con = void $ both (payEp @()
                                 @EscrowSchema
                                 @EscrowError
                                 (escrowParams startTime)
                          )
                          (redeemEp (escrowParams startTime))
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    hdl3 <- Trace.activateContractWallet w3 con
    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
    Trace.callEndpoint @"pay-escrow" hdl2 (Ada.adaValueOf 10)
    Trace.callEndpoint @"pay-escrow" hdl3 (Ada.adaValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"redeem-escrow" hdl1 ()
    void $ Trace.waitNSlots 1

-- | Wallet 1 pays into an escrow contract and gets a refund when the
--   amount isn't claimed.
refundTrace :: Trace.EmulatorTrace ()
refundTrace = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let con = void (payEp @()
                          @EscrowSchema
                          @EscrowError
                          (escrowParams startTime))
           <> void (refundEp (escrowParams startTime))
    hdl1 <- Trace.activateContractWallet w1 con
    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
    _ <- Trace.waitNSlots 100
    Trace.callEndpoint @"refund-escrow" hdl1 ()
    void $ Trace.waitNSlots 1

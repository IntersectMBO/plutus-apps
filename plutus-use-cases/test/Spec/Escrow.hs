{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Spec.Escrow(tests, redeemTrace, redeem2Trace, refundTrace, prop_Escrow) where

import Control.Lens hiding (both)
import Control.Monad (void)
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

import Plutus.Contracts.Escrow
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck hiding ((.&&.))

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value
                               , _refundSlot    :: Slot
                               , _targets       :: Map Wallet Value
                               } deriving (Eq, Show)

makeLenses ''EscrowModel

modelParams :: EscrowParams d
modelParams = escrowParams $ TimeSlot.scSlotZeroTime def

deriving instance Eq (Action EscrowModel)
deriving instance Show (Action EscrowModel)

deriving instance Eq (ContractInstanceKey EscrowModel w s e)
deriving instance Show (ContractInstanceKey EscrowModel w s e)

instance ContractModel EscrowModel where
  data Action EscrowModel = Pay Wallet Integer
                          | Redeem Wallet
                          | Refund Wallet
                          | WaitUntil Slot

  data ContractInstanceKey EscrowModel w s e where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError

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

  initialHandleSpecs = [ ContractInstanceSpec (WalletKey w) w testContract | w <- testWallets ]
    where
      -- TODO: Lazy test contract for now
      testContract = selectList [void $ payEp modelParams, void $ redeemEp modelParams, void $ refundEp modelParams] >> testContract

  nextState a = void $ case a of
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions $~ Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      let leftoverValue = fold contribs <> inv (fold targets)
      deposit w leftoverValue
      contributions $= Map.empty
      wait 1
    Refund w -> do
      v <- viewContractState $ contributions . at w . to fold
      contributions $~ Map.delete w
      deposit w v
      wait 1
    WaitUntil s -> do
      waitUntil s

  precondition s a = case a of
    WaitUntil slot' -> s ^. currentSlot < slot'
    Redeem _ -> (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
             && (s ^. currentSlot < s ^. contractState . refundSlot - 1)
    Refund w -> s ^. currentSlot > s ^. contractState . refundSlot
             && Nothing /= (s ^. contractState . contributions . at w)
    Pay w v -> Nothing == s ^. contractState . contributions . at w
            && s ^. currentSlot + 1 < s ^. contractState . refundSlot
            && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut

  perform h _ a = void $ case a of
    WaitUntil slot -> Trace.waitUntilSlot slot
    Pay w v        -> do
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.adaValueOf $ fromInteger v)
      Trace.waitNSlots 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      Trace.waitNSlots 1
    Refund w       -> do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      Trace.waitNSlots 1

  arbitraryAction s = oneof $ [ Pay <$> QC.elements testWallets <*> choose @Integer (10, 30)
                              , Redeem <$> QC.elements testWallets
                              , WaitUntil . step <$> choose @Int (1, 10) ] ++
                              [ Refund <$> QC.elements (s ^. contractState . contributions . to Map.keys)
                              | Prelude.not . null $ s ^. contractState . contributions . to Map.keys ]
                  where
                    slot = s ^. currentSlot
                    step n = slot + fromIntegral n

  shrinkAction _ (WaitUntil s) = WaitUntil . fromIntegral <$> (shrink . toInteger $ s)
  -- TODO: This trick should be part of every model. We should make waiting a builtin thing
  shrinkAction s _             = [WaitUntil $ s ^. currentSlot + n | n <- [1..10]]

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]

prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions_

tests :: TestTree
tests = testGroup "escrow"
    [ let con = void $ payEp @() @EscrowSchema @EscrowError (escrowParams startTime) in
      checkPredicate "can pay"
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
      checkPredicate "can redeem"
        ( assertDone con (Trace.walletInstanceTag w3) (const True) "escrow redeem not done"
          .&&. walletFundsChange w1 (Ada.adaValueOf (-10))
          .&&. walletFundsChange w2 (Ada.adaValueOf 10)
          .&&. walletFundsChange w3 mempty
        )
        redeemTrace

    , checkPredicate "can redeem even if more money than required has been paid in"

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
      checkPredicate "can refund"
        ( walletFundsChange w1 mempty
          .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "refund should succeed")
        refundTrace

    , HUnit.testCaseSteps "script size is reasonable"
        $ \step -> reasonable' step
                               (Scripts.validatorScript $ typedValidator (escrowParams startTime))
                               32000

    , testProperty "QuickCheck ContractModel" $ withMaxSuccess 10 prop_Escrow
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 10000
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

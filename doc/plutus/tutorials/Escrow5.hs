{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

--  This module contains a contract model for positive testing of the
--  simplified escrow contract in Plutus.Contracts.Tutorial.Escrow,
--  with generated escrow targets. See the "Parameterising Models and
--  Dynamic Contract Instances" section of the tutorial.

module Escrow5(prop_Escrow, prop_FinishEscrow, prop_FinishFast, prop_NoLockedFunds, prop_NoLockedFundsFast,
               check_propEscrowWithCoverage, EscrowModel) where

import Control.Lens (At (at), makeLenses, set, to, (%=), (.=), (^.))
import Control.Monad (void, when)
import Data.Data (Data)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (POSIXTime (POSIXTime), Slot (Slot, getSlot), minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Contract (Contract, selectList)
import Plutus.Contract.Test (Wallet, mockWalletPaymentPubKeyHash, w1, w2, w3, w4, w5)
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.V1.Ledger.Api (Datum)

import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Data.Default (Default (def))
import Ledger.TimeSlot (SlotConfig (scSlotLength), scSlotZeroTime)
import Plutus.Contract.Test.ContractModel (coverageIndex, currentSlot, defaultCoverageOptions)
import Plutus.Contract.Test.Coverage (writeCoverageReport)
import Plutus.Contracts.Escrow (EscrowError, EscrowParams (EscrowParams, escrowDeadline, escrowTargets), EscrowSchema,
                                covIdx, payEp, payToPaymentPubKeyTarget, redeemEp, refundEp)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, Positive (getPositive), Property, choose, elements,
                        frequency, infiniteListOf, scale, shrinkList, stdArgs, sublistOf, tabulate, withMaxSuccess)

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value.Value
                               , _targets       :: Map Wallet Value.Value
                               , _refundSlot    :: Slot
                               , _phase         :: Phase
                               } deriving (Eq, Show, Data)

data Phase = Initial | Running | Refunding deriving (Eq, Show, Data)

makeLenses ''EscrowModel

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where
  data Action EscrowModel = Init Slot [(Wallet, Integer)]
                          | Redeem Wallet
                          | Pay Wallet Integer
                          | Refund Wallet
    deriving (Eq, Show, Data)

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _refundSlot    = 0
                             , _phase         = Initial
                             }

  initialInstances = []

  startInstances _ (Init s wns) =
    [CM.StartContract (WalletKey w) (escrowParams s wns) | w <- testWallets]
  startInstances _ _ = []

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} params = testContract params

  nextState a = case a of
    Init s wns -> do
      phase   .= Running
      targets .= Map.fromList [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- wns]
      refundSlot .= s
    Pay w v -> do
      CM.withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      CM.wait 1
    Redeem w -> do
      targets <- CM.viewContractState targets
      contribs <- CM.viewContractState contributions
      sequence_ [ CM.deposit w v | (w, v) <- Map.toList targets ]
      let leftoverValue = fold contribs <> inv (fold targets)
      CM.deposit w leftoverValue
      contributions .= Map.empty
      CM.wait 1
    Refund w -> do
      v <- CM.viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      CM.deposit w v
      CM.wait 1


  nextReactiveState slot = do
    deadline <- CM.viewContractState refundSlot
    when (slot >= deadline) $ phase .= Refunding


  precondition s a = case a of
    Init s tgts -> currentPhase == Initial
                && s > 1
                && and [Ada.adaValueOf (fromInteger n) `Value.geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
    Redeem _    -> currentPhase == Running
                && fold (s ^. CM.contractState . contributions) `Value.geq` fold (s ^. CM.contractState . targets)
    Pay _ v     -> currentPhase == Running
                && Ada.adaValueOf (fromInteger v) `Value.geq` Ada.toValue minAdaTxOut
    Refund w    -> currentPhase == Refunding
                && w `Map.member` (s ^. CM.contractState . contributions)
    where currentPhase = s ^. CM.contractState . phase


  perform h _ _ a = case a of
    Init _ _       -> do
      return ()
    Pay w v        -> do
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.adaValueOf $ fromInteger v)
      CM.delay 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      CM.delay 1
    Refund w       -> do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      CM.delay 1

  arbitraryAction s
    | s ^. CM.contractState . phase == Initial
      = Init <$> (Slot . getPositive <$> scale (*10) arbitrary) <*> arbitraryTargets
    | otherwise
      = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. CM.contractState . contributions . to fold) `Value.geq` (s ^. CM.contractState . targets . to fold)
                    ]  ++
                    [ (1, Refund <$> elements testWallets) ]


  shrinkAction _ (Init s tgts) = map (Init s) (shrinkList (\(w,n)->(w,)<$>shrink n) tgts)
                              ++ map (`Init` tgts) (map Slot . shrink . getSlot $ s)
  shrinkAction _ (Pay w n)     = [Pay w n' | n' <- shrink n]
  shrinkAction _ _             = []

arbitraryTargets :: Gen [(Wallet,Integer)]
arbitraryTargets = do
  ws <- sublistOf testWallets
  vs <- infiniteListOf $ choose (1,30)
  return $ zip ws vs

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

testContract :: EscrowParams Datum -> Contract () EscrowSchema EscrowError ()
testContract params = selectList [ void $ payEp params
                                 , void $ redeemEp params
                                 , void $ refundEp params
                                 ] >> testContract params


prop_Escrow :: CM.Actions EscrowModel -> Property
prop_Escrow = CM.propRunActions_


escrowParams :: Slot -> [(Wallet, Integer)] -> EscrowParams d
escrowParams s tgts =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w) (Ada.adaValueOf (fromInteger n))
        | (w,n) <- tgts
        ]
    , escrowDeadline = scSlotZeroTime def + POSIXTime (getSlot s * scSlotLength def)
    }

finishEscrow :: CM.DL EscrowModel ()
finishEscrow = do
    CM.anyActions_
    finishingStrategy
    CM.assertModel "Locked funds are not zero" (CM.symIsZero . CM.lockedValue)

finishingStrategy :: CM.DL EscrowModel ()
finishingStrategy = do
    contribs <- CM.viewContractState contributions
    CM.monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    phase <- CM.viewContractState phase
    CM.monitor $ tabulate "Phase" [show phase]
    waitUntilDeadline
    sequence_ [CM.action $ Refund w | w <- testWallets, w `Map.member` contribs]

walletStrategy :: Wallet -> CM.DL EscrowModel ()
walletStrategy w = do
    contribs <- CM.viewContractState contributions
    --waitUntilDeadline
    when (w `Map.member` contribs) $ do
      CM.action $ Refund w

waitUntilDeadline :: CM.DL EscrowModel ()
waitUntilDeadline = do
    deadline <- CM.viewContractState refundSlot
    slot     <- CM.viewModelState currentSlot
    when (slot < deadline) $ CM.waitUntilDL deadline

noLockProof :: CM.NoLockedFundsProof EscrowModel
noLockProof = CM.defaultNLFP
  { CM.nlfpMainStrategy   = finishingStrategy
  , CM.nlfpWalletStrategy = walletStrategy    }

prop_FinishEscrow :: Property
prop_FinishEscrow = CM.forAllDL finishEscrow prop_Escrow

prop_FinishFast :: Property
prop_FinishFast = CM.forAllDL finishEscrow $ const True

prop_NoLockedFunds :: Property
prop_NoLockedFunds = CM.checkNoLockedFundsProof noLockProof

prop_NoLockedFundsFast :: Property
prop_NoLockedFundsFast = CM.checkNoLockedFundsProofFast noLockProof

{- START check_propEscrowWithCoverage -}
check_propEscrowWithCoverage :: IO ()
check_propEscrowWithCoverage = do
  cr <- CM.quickCheckWithCoverage stdArgs (set coverageIndex covIdx defaultCoverageOptions) $ \covopts ->
    withMaxSuccess 1000 $
      CM.propRunActionsWithOptions @EscrowModel CM.defaultCheckOptionsContractModel covopts
        (const (pure True))
  writeCoverageReport "Escrow" covIdx cr
{- END check_propEscrowWithCoverage -}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

--  This module contains a contract model for positive testing of the
--  simplified escrow contract in Plutus.Contracts.Tutorial.Escrow,
--  with generated escrow targets. See the "Parameterising Models and
--  Dynamic Contract Instances" section of the tutorial.

module Escrow4(prop_Escrow, prop_FinishEscrow, prop_NoLockedFunds, EscrowModel) where

import Control.Lens (At (at), makeLenses, to, (%=), (.=), (^.))
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
import Plutus.Contracts.Escrow (EscrowError, EscrowParams (EscrowParams, escrowDeadline, escrowTargets), EscrowSchema,
                                payEp, payToPaymentPubKeyTarget, redeemEp, refundEp)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, Positive (getPositive), Property, choose, elements,
                        frequency, infiniteListOf, shrinkList, sublistOf, tabulate)

{- START EscrowModel -}
data EscrowModel =
    EscrowModel
        { _contributions :: Map Wallet Value.Value
        , _targets       :: Map Wallet Value.Value
        , _refundSlot    :: Slot             -- NEW!!!
        , _phase         :: Phase
        } deriving (Eq, Show, Data)
{- END EscrowModel -}

{- START Phase -}
data Phase = Initial | Running | Refunding deriving (Eq, Show, Data)
{- END Phase -}

makeLenses ''EscrowModel

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where

{- START Action -}
  data Action EscrowModel = Init Slot [(Wallet, Integer)]    -- NEW!!!
                          | Redeem Wallet
                          | Pay Wallet Integer
                          | Refund Wallet
    deriving (Eq, Show, Data)
{- END Action -}

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _refundSlot    = 0
                             , _phase         = Initial
                             }

  initialInstances = []

{- START startInstances -}
  startInstances _ (Init s wns) =
    [CM.StartContract (WalletKey w) (escrowParams s wns) | w <- testWallets]
{- END startInstances -}
  startInstances _ _ = []

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} params = testContract params

{- START nextState -}
  nextState (Init s wns) = do
      phase   .= Running
      targets .= Map.fromList [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- wns]
      refundSlot .= s                 -- NEW!!!
{- END nextState -}

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

{- START nextReactiveState -}
  nextReactiveState slot = do
    deadline <- CM.viewContractState refundSlot
    when (slot >= deadline) $ phase .= Refunding
{- END nextReactiveState -}

{- START precondition -}
  precondition s a = case a of
    Init s tgts -> currentPhase == Initial
                && s > 1
                && and [Ada.adaValueOf (fromInteger n) `Value.geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
    Redeem _    -> currentPhase == Running
                && fold (s ^. CM.contractState . contributions) `Value.geq` fold (s ^. CM.contractState . targets)
    Pay _ v     -> currentPhase == Running
                && Ada.adaValueOf (fromInteger v) `Value.geq` Ada.toValue minAdaTxOut
    Refund w    -> currentPhase == Refunding           -- NEW!!!
                && w `Map.member` (s ^. CM.contractState . contributions)
    where currentPhase = s ^. CM.contractState . phase
{- END precondition -}

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

{- START arbitraryAction -}
  arbitraryAction s
    | s ^. CM.contractState . phase == Initial
      = Init <$> (Slot . getPositive <$> arbitrary) <*> arbitraryTargets
{- END arbitraryAction -}
    | otherwise
      = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. CM.contractState . contributions . to fold) `Value.geq` (s ^. CM.contractState . targets . to fold)
                    ]  ++
                    [ (1, Refund <$> elements testWallets) ]
{-
{- START weightedArbitraryAction -}
  arbitraryAction s
    | s ^.contractState . phase == Initial
      = Init <$> (Slot . getPositive <$> scale (*10) arbitrary) <*> arbitraryTargets
{- END weightedArbitraryAction -}
-}

{- START shrinkAction -}
  shrinkAction _ (Init s tgts) = map (Init s) (shrinkList (\(w,n)->(w,)<$>shrink n) tgts)
                              ++ map (`Init` tgts) (map Slot . shrink . getSlot $ s)      -- NEW!!!
{- END shrinkAction -}
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

{- START escrowParams -}
escrowParams :: Slot -> [(Wallet, Integer)] -> EscrowParams d
escrowParams s tgts =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w) (Ada.adaValueOf (fromInteger n))
        | (w,n) <- tgts
        ]
    , escrowDeadline = scSlotZeroTime def + POSIXTime (getSlot s * scSlotLength def)     -- NEW!!!
    }
{- END escrowParams -}


finishEscrow :: CM.DL EscrowModel ()
finishEscrow = do
    CM.anyActions_
    finishingStrategy
    CM.assertModel "Locked funds are not zero" (CM.symIsZero . CM.lockedValue)

{-
{- START oldFinishingStrategy -}
finishingStrategy :: DL EscrowModel ()
finishingStrategy = do
    contribs <- viewContractState contributions
    monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs]
{- END oldFinishingStrategy -}
-}

{- START finishingStrategy -}
finishingStrategy :: CM.DL EscrowModel ()
finishingStrategy = do
    contribs <- CM.viewContractState contributions
    CM.monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    waitUntilDeadline                                                  -- NEW!!!
    sequence_ [CM.action $ Refund w | w <- testWallets, w `Map.member` contribs]
{- END finishingStrategy -}
{-
{- START monitoredFinishingStrategy -}
finishingStrategy :: DL EscrowModel ()
finishingStrategy = do
    contribs <- viewContractState contributions
    monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    phase <- viewContractState phase           -- NEW!!!
    monitor $ tabulate "Phase" [show phase]    -- NEW!!!
    waitUntilDeadline
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs]
{- END monitoredFinishingStrategy -}
-}

walletStrategy :: Wallet -> CM.DL EscrowModel ()
walletStrategy w = do
    contribs <- CM.viewContractState contributions
    when (w `Map.member` contribs) $ do
      --waitUntilDeadline
      CM.action $ Refund w

{- START waitUntilDeadline -}
waitUntilDeadline :: CM.DL EscrowModel ()
waitUntilDeadline = do
    deadline <- CM.viewContractState refundSlot
    slot     <- CM.viewModelState CM.currentSlot
    when (slot < deadline) $ CM.waitUntilDL deadline
{- END waitUntilDeadline -}

noLockProof :: CM.NoLockedFundsProof EscrowModel
noLockProof = CM.defaultNLFP
  { CM.nlfpMainStrategy   = finishingStrategy
  , CM.nlfpWalletStrategy = walletStrategy    }

{- START prop_FinishEscrow -}
prop_FinishEscrow :: Property
prop_FinishEscrow = CM.forAllDL finishEscrow prop_Escrow
{- END prop_FinishEscrow -}

{-
{- START prop_FinishFast -}
prop_FinishFast :: Property
prop_FinishFast = forAllDL finishEscrow $ const True
{- END prop_FinishFast -}
-}

prop_NoLockedFunds :: Property
prop_NoLockedFunds = CM.checkNoLockedFundsProof noLockProof


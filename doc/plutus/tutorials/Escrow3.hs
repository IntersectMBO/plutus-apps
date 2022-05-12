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

module Escrow3(prop_Escrow, prop_FinishEscrow, prop_NoLockedFunds, EscrowModel) where

import Control.Lens (At (at), makeLenses, to, (%=), (.=), (^.))
import Control.Monad (void, when)
import Data.Data (Data)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Contract (Contract, selectList)
import Plutus.Contract.Test (Wallet, mockWalletPaymentPubKeyHash, w1, w2, w3, w4, w5)
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.V1.Ledger.Api (Datum)

import Plutus.Contracts.Tutorial.Escrow (EscrowError, EscrowParams (EscrowParams, escrowTargets), EscrowSchema, payEp,
                                         payToPaymentPubKeyTarget, redeemEp, refundEp)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck (Arbitrary (shrink), Gen, Property, choose, elements, frequency, infiniteListOf, shrinkList,
                        sublistOf, tabulate)

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value.Value
                               , _targets       :: Map Wallet Value.Value
                               , _phase         :: Phase
                               } deriving (Eq, Show, Data)

data Phase = Initial | Running deriving (Eq, Show, Data)

makeLenses ''EscrowModel

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where
{- START EscrowModel -}
  data Action EscrowModel = Init [(Wallet, Integer)]
                          | Redeem Wallet
                          | Pay Wallet Integer
                          | Refund Wallet            -- NEW!
    deriving (Eq, Show, Data)
{- END EscrowModel -}

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _phase         = Initial
                             }

  initialInstances = []

  startInstances _ (Init wns) =
    [CM.StartContract (WalletKey w) (escrowParams wns) | w <- testWallets]
  startInstances _ _ = []

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} params = testContract params

  nextState a = case a of
    Init wns -> do
      phase   .= Running
      targets .= Map.fromList [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- wns]
    Pay w v -> do
      CM.withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      CM.wait 1
    Redeem w -> do
      targets <- CM.viewContractState targets
      contribs <- CM.viewContractState contributions
      sequence_ [ CM.deposit w v | (w, v) <- Map.toList targets ]
      -- omit next two lines to disable disbursement of the surplus
      let leftoverValue = fold contribs <> inv (fold targets)
      CM.deposit w leftoverValue
      contributions .= Map.empty
      CM.wait 1
    Refund w -> do
      v <- CM.viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      CM.deposit w v
      CM.wait 1

  precondition s a = case a of
    Init tgts   -> currentPhase == Initial
                && and [Ada.adaValueOf (fromInteger n) `Value.geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
    Redeem _    -> currentPhase == Running
                && fold (s ^. CM.contractState . contributions) `Value.geq` fold (s ^. CM.contractState . targets)
             --   && fold (s ^. contractState . contributions) == fold (s ^. contractState . targets)
    Pay _ v     -> currentPhase == Running
                && Ada.adaValueOf (fromInteger v) `Value.geq` Ada.toValue minAdaTxOut
    Refund w    -> currentPhase == Running
                && w `Map.member` (s ^. CM.contractState . contributions)
    where currentPhase = s ^. CM.contractState . phase

{-
{- START strongPrecondition -}
precondition s (Redeem _) =
     currentPhase == Running
  && fold (s ^. contractState . contributions) == fold (s ^. contractState . targets)
{- END strongPrecondition -}
-}

  perform h _ _ a = case a of
    Init _         -> do
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

{-
{-START RefundModel -}
  nextState (Refund w) = do
      v <- viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      deposit w v
      wait 1

  precondition s (Refund w) =
       currentPhase == Running
    && w `Map.member` (s ^. contractState . contributions)
    where currentPhase = s ^. contractState . phase

  perform h _ _ (Refund w) = do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      delay 1

  arbitraryAction s
    ...
      = frequency $ ... ++
                    [ (1, Refund <$> elements testWallets) ]
{- END RefundModel -}
      -}

  arbitraryAction s
    | s ^. CM.contractState . phase == Initial
      = Init <$> arbitraryTargets
    | otherwise
      = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. CM.contractState . contributions . to fold) `Value.geq` (s ^. CM.contractState . targets . to fold)
                    ] ++
                    [ (1, Refund <$> elements testWallets) ]


  shrinkAction _ (Init tgts) = map Init (shrinkList (\(w,n)->(w,)<$>shrink n) tgts)
  shrinkAction _ (Pay w n)   = [Pay w n' | n' <- shrink n]
  shrinkAction _ _           = []

arbitraryTargets :: Gen [(Wallet,Integer)]
arbitraryTargets = do
  ws <- sublistOf testWallets
  vs <- infiniteListOf $ choose (1,30)
  return $ zip ws vs

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

{- START testContract -}
testContract :: EscrowParams Datum -> Contract () EscrowSchema EscrowError ()
testContract params = selectList [ void $ payEp params
                                 , void $ redeemEp params
                                 , void $ refundEp params     -- NEW!
                                 ] >> testContract params
{- END testContract -}

prop_Escrow :: CM.Actions EscrowModel -> Property
prop_Escrow = CM.propRunActions_


escrowParams :: [(Wallet, Integer)] -> EscrowParams d
escrowParams tgts =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w) (Ada.adaValueOf (fromInteger n))
        | (w,n) <- tgts
        ]
    }

{-
-- This is the first--bad--approach to recovering locked funds.
{- START finishEscrow -}
finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy w1
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)
{- END finishEscrow -}

{- START badFinishingStrategy -}
finishingStrategy :: Wallet -> DL EscrowModel ()
finishingStrategy w = do
    currentPhase <- viewContractState phase
    when (currentPhase /= Initial) $ do
      action $ Redeem w
{- END badFinishingStrategy -}

{- START finishingStrategy -}
finishingStrategy :: Wallet -> DL EscrowModel ()
finishingStrategy w = do
    currentPhase <- viewContractState phase
    when (currentPhase /= Initial) $ do
      currentTargets  <- viewContractState targets
      currentContribs <- viewContractState contributions
      let deficit = fold currentTargets <> inv (fold currentContribs)
      when (deficit `gt` Ada.adaValueOf 0) $
        action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
      action $ Redeem w
{- END finishingStrategy -}

-- This unilateral strategy fails.
{- START noLockProof -}
noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy w1
  , nlfpWalletStrategy = finishingStrategy    }
{- END noLockProof -}
-}

finishEscrow :: CM.DL EscrowModel ()
finishEscrow = do
    CM.anyActions_
    finishingStrategy
    CM.assertModel "Locked funds are not zero" (CM.symIsZero . CM.lockedValue)

{-
{- START betterFinishingStrategy -}
finishingStrategy :: (Wallet -> Bool) -> DL EscrowModel ()
finishingStrategy walletActive = do
    contribs <- viewContractState contributions
    monitor (classify (Map.null contribs) "no need for extra refund to recover funds")
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs, walletActive w]
{- END betterFinishingStrategy -}
-}

{- START prop_FinishEscrow -}
prop_FinishEscrow :: Property
prop_FinishEscrow = CM.forAllDL finishEscrow prop_Escrow
{- END prop_FinishEscrow -}

{- START BetterNoLockProof -}
noLockProof :: CM.NoLockedFundsProof EscrowModel
noLockProof = CM.defaultNLFP
  { CM.nlfpMainStrategy   = finishingStrategy
  , CM.nlfpWalletStrategy = walletStrategy    }
{- END BetterNoLockProof -}

{- START prop_NoLockedFunds -}
prop_NoLockedFunds :: Property
prop_NoLockedFunds = CM.checkNoLockedFundsProof noLockProof
{- END prop_NoLockedFunds -}

{-
{- START fixedTargets -}
fixedTargets :: DL EscrowModel ()
fixedTargets = do
  action $ Init [(w1,10),(w2,20)]
  anyActions_
{- END fixedTargets -}
-}

{- START BetterStrategies -}
finishingStrategy :: CM.DL EscrowModel ()
finishingStrategy = do
    contribs <- CM.viewContractState contributions
    CM.monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    sequence_ [CM.action $ Refund w | w <- testWallets, w `Map.member` contribs]

walletStrategy :: Wallet -> CM.DL EscrowModel ()
walletStrategy w = do
    contribs <- CM.viewContractState contributions
    when (w `Map.member` contribs) $ CM.action $ Refund w
{- END BetterStrategies -}

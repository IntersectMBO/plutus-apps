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

module Spec.Tutorial.Escrow3(prop_Escrow, prop_FinishEscrow, prop_NoLockedFunds, EscrowModel) where

import Control.Lens hiding (both, elements)
import Control.Monad (void, when)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (Datum, minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Value
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel

import Plutus.Contracts.Tutorial.Escrow hiding (Action (..))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value
                               , _targets       :: Map Wallet Value
                               , _phase         :: Phase
                               } deriving (Eq, Show)

data Phase = Initial | Running deriving (Eq, Show)

makeLenses ''EscrowModel

deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where
{- START EscrowModel -}
  data Action EscrowModel = Init [(Wallet, Integer)]
                          | Redeem Wallet
                          | Pay Wallet Integer
                          | Refund Wallet            -- NEW!
    deriving (Eq, Show)
{- END EscrowModel -}

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _phase         = Initial
                             }

  initialInstances = []

  startInstances _ (Init wns) =
    [StartContract (WalletKey w) (escrowParams wns) | w <- testWallets]
  startInstances _ _ = []

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} params = testContract params

  nextState a = case a of
    Init wns -> do
      phase   .= Running
      targets .= Map.fromList [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- wns]
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      -- omit next two lines to disable disbursement of the surplus
      let leftoverValue = fold contribs <> inv (fold targets)
      deposit w leftoverValue
      contributions .= Map.empty
      wait 1
    Refund w -> do
      v <- viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      deposit w v
      wait 1

  precondition s a = case a of
    Init tgts   -> currentPhase == Initial
                && and [Ada.adaValueOf (fromInteger n) `geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
    Redeem _    -> currentPhase == Running
                && fold (s ^. contractState . contributions) `geq` fold (s ^. contractState . targets)
             --   && fold (s ^. contractState . contributions) == fold (s ^. contractState . targets)
    Pay _ v     -> currentPhase == Running
                && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut
    Refund w    -> currentPhase == Running
                && w `Map.member` (s ^. contractState . contributions)
    where currentPhase = s ^. contractState . phase

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
      delay 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      delay 1
    Refund w       -> do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      delay 1

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
    | s ^.contractState . phase == Initial
      = Init <$> arbitraryTargets
    | otherwise
      = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
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

prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions_


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
finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy (const True)
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

{- START betterFinishingStrategy -}
finishingStrategy :: (Wallet -> Bool) -> DL EscrowModel ()
finishingStrategy walletActive = do
    contribs <- viewContractState contributions
    monitor (classify (Map.null contribs) "no need for extra refund to recover funds")
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs, walletActive w]
{- END betterFinishingStrategy -}

{- START prop_FinishEscrow -}
prop_FinishEscrow :: Property
prop_FinishEscrow = forAllDL finishEscrow prop_Escrow
{- END prop_FinishEscrow -}

{- START BetterNoLockProof -}
noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy
  , nlfpWalletStrategy = walletStrategy    }
{- END BetterNoLockProof -}

{- START prop_NoLockedFunds -}
prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProof noLockProof
{- END prop_NoLockedFunds -}

{- START fixedTargets -}
fixedTargets :: DL EscrowModel ()
fixedTargets = do
  action $ Init [(w1,10),(w2,20)]
  anyActions_
{- END fixedTargets -}

{- START BetterStrategies -}
finishingStrategy :: DL EscrowModel ()
finishingStrategy = do
    contribs <- viewContractState contributions
    monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs]

walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    contribs <- viewContractState contributions
    when (w `Map.member` contribs) $ action $ Refund w
{- END BetterStrategies -}

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
module Spec.Tutorial.Escrow(tests, prop_Escrow,
                            prop_FinishEscrow, prop_NoLockedFunds,
                            prop_CrashTolerance, prop_Whitelist,
                            check_propEscrowWithCoverage,
                            EscrowModel) where

import Control.Lens hiding (both, elements)
import Control.Monad (void, when)
import Data.Data
import Data.Foldable
import Data.Function
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Value
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Contract.Test.Coverage
import Plutus.V1.Ledger.Api (Datum)

import Plutus.Contracts.Tutorial.Escrow hiding (Action (..))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&&.))

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value
                               , _targets       :: Map Wallet Value
                               , _phase         :: Phase
                               } deriving (Eq, Show, Data)

data Phase = Initial | Running deriving (Eq, Show, Data)

makeLenses ''EscrowModel

deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where
  data Action EscrowModel = Init [(Wallet, Integer)]
                          | Redeem Wallet
                          | Pay Wallet Integer
                          | Refund Wallet
    deriving (Eq, Show, Data)

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _phase         = Initial
                             }
{-                             , _targets       = Map.fromList [ (w1, Ada.adaValueOf 10)
                                                             , (w2, Ada.adaValueOf 20)
                                                             ]
                             }
-}

  --initialInstances = [StartContract (WalletKey w) () | w <- testWallets]
  initialInstances = []

  startInstances _ (Init wns) =
    [StartContract (WalletKey w) (escrowParams wns) | w <- testWallets]
  startInstances _ _ = []

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} params = testContract
    where
      testContract = selectList [ void $ payEp params
                                , void $ redeemEp params
                                , void $ refundEp params
                                ] >> testContract

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
    Init tgts-> currentPhase == Initial
             && and [Ada.adaValueOf (fromInteger n) `geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
--             && and [Ada.adaValueOf (fromInteger n) `gt` Ada.toValue 0 | (_,n) <- tgts]
    Redeem _ -> currentPhase == Running
             && (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
--             && (s ^. contractState . contributions . to fold) == (s ^. contractState . targets . to fold)
    Refund w -> Nothing /= (s ^. contractState . contributions . at w)
    Pay _ v  -> currentPhase == Running
            && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut
            -- disallow payments that take us over the targets
            -- && ((s ^. contractState . contributions . to fold) <> Ada.adaValueOf (fromInteger v)) `leq` (s ^. contractState . targets . to fold)
    where currentPhase = s ^. contractState . phase

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


  arbitraryAction s
    | s ^.contractState . phase == Initial
      = Init <$> arbitraryTargets
    | otherwise
      = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
                    ]
                                   ++
                                  [ (1, Refund <$> elements (s ^. contractState . contributions . to Map.keys))
                                  | Prelude.not . null $ s ^. contractState . contributions . to Map.keys ]


  shrinkAction _ (Init tgts) = map Init (shrinkList (\(w,n)->(w,)<$>shrink n) tgts)
  shrinkAction _ (Pay w n)   = [Pay w n' | n' <- shrink n]
  shrinkAction _ _           = []

{-
  monitoring _ (Redeem _) = classify True "Contains Redeem"
  monitoring (s,s') _ = classify (redeemable s' && Prelude.not (redeemable s)) "Redeemable"
    where redeemable s = precondition s (Redeem undefined)
-}

arbitraryTargets :: Gen [(Wallet,Integer)]
arbitraryTargets = do
  ws <- sublistOf testWallets
  vs <- infiniteListOf $ choose (1,30)
  return $ zip ws vs

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions_

finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: DL EscrowModel ()
finishingStrategy = do
    contribs <- viewContractState contributions
    monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs]

walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    contribs <- viewContractState contributions
    when (w `Map.member` contribs) $ action $ Refund w

prop_FinishEscrow :: Property
prop_FinishEscrow = forAllDL finishEscrow prop_Escrow

noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy
  , nlfpWalletStrategy = walletStrategy    }

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProof noLockProof

instance CrashTolerance EscrowModel where
  available (Init _) _ = True
  available a alive = (Key $ WalletKey w) `elem` alive
    where w = case a of
                Pay w _  -> w
                Redeem w -> w
                Refund w -> w
                _        -> error "This case is unreachable"

  restartArguments s WalletKey{} = escrowParams' $ Map.toList (s ^. contractState . targets)

prop_CrashTolerance :: Actions (WithCrashTolerance EscrowModel) -> Property
prop_CrashTolerance = propRunActions_

prop_Whitelist :: Actions EscrowModel -> Property
prop_Whitelist = checkErrorWhitelist defaultWhitelist

tests :: TestTree
tests = testGroup "escrow"
    [ testProperty "QuickCheck ContractModel" $ withMaxSuccess 10 prop_Escrow
    , testProperty "QuickCheck NoLockedFunds" $ withMaxSuccess 10 prop_NoLockedFunds
    ]

escrowParams :: [(Wallet, Integer)] -> EscrowParams d
escrowParams tgts = escrowParams' [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- tgts]

escrowParams' :: [(Wallet,Value)] -> EscrowParams d
escrowParams' tgts' =
  EscrowParams
    { escrowTargets  = [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w) v
                       | (w,v) <- sortBy (compare `on` fst) tgts' ] }

check_propEscrowWithCoverage :: IO ()
check_propEscrowWithCoverage = do
  cr <- quickCheckWithCoverage stdArgs (set coverageIndex covIdx $ defaultCoverageOptions) $ \covopts ->
    withMaxSuccess 1000 $ propRunActionsWithOptions @EscrowModel defaultCheckOptionsContractModel
                                                    covopts (const (pure True))
  writeCoverageReport "Escrow" covIdx cr

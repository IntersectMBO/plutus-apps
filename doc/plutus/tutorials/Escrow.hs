{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Spec.Tutorial.Escrow(tests, prop_Escrow,
                            --prop_FinishEscrow, prop_NoLockedFunds,
                            EscrowModel) where

import Control.Lens hiding (both, elements)
import Control.Monad (void)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel

import Plutus.Contracts.Tutorial.Escrow hiding (Action (..))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&&.))

{- START EscrowModel -}
data EscrowModel = EscrowModel { _contributions :: Map Wallet Value
                               , _targets       :: Map Wallet Value
                               } deriving (Eq, Show)

makeLenses ''EscrowModel
{- END EscrowModel -}

{- START ContractInstanceKeyDeriving :-}
deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)
{- END ContractInstanceKeyDeriving -}
{-
{- START ContractModelInstance -}
instance ContractModel EscrowModel where ...
{- END ContractModelInstance -}
-}
instance ContractModel EscrowModel where
{- START ActionType -}
  data Action EscrowModel = Pay Wallet Integer
                          | Redeem Wallet
    deriving (Eq, Show)
{- END ActionType -}

                          -- | Refund Wallet

{- START ContractInstanceKeyType -}
  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError ()
{- END ContractInstanceKeyType -}

{- START initialState -}
  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.fromList [ (w1, Ada.adaValueOf 10)
                                                             , (w2, Ada.adaValueOf 20)
                                                             ]
                             }
{- END initialState -}

{-
{- START testContract -}
testContract = selectList [ void $ payEp escrowParams
                          , void $ redeemEp escrowParams
                          ] >> testContract
{- END testContract -}
-}

{-

{- START ContractKeySemantics -}
  initialInstances = [StartContract (WalletKey w) () | w <- testWallets]

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} _ = testContract
{- END ContractKeySemantics -}

-}

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} _ = testContract
    where
      testContract = selectList [ void $ payEp escrowParams
                                , void $ redeemEp escrowParams
                                -- , void $ refundEp escrowParams
                                ] >> testContract
{- START initialInstances -}
  initialInstances = [StartContract (WalletKey w) () | w <- testWallets]
{- END initialInstances -}
{-
{- START 0nextState -}
  nextState a = case a of
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      contributions .= Map.empty
      wait 1
{- END 0nextState -}
-}
{-
{- START nextState1 -}
  nextState a = case a of
    Pay w v -> ...
    Redeem w -> do
      targets <- viewContractState targets
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      contribs <- viewContractState contributions                 -- NEW
      let leftoverValue = fold contribs <> inv (fold targets)     -- NEW
      deposit w leftoverValue                                     -- NEW
      contributions .= Map.empty
      wait 1
{- END nextState1 -}
-}
{- START nextState -}
  nextState a = case a of
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
{- END nextState -}
{-    Refund w -> do
      v <- viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      deposit w v
      wait 1
-}

{- START precondition1 -}
  precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to fold)
                `geq`
                (s ^. contractState . targets . to fold)
    _        -> True
{- END precondition1 -}
{-
{- START precondition2 -}
precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to fold)
                `geq`
                (s ^. contractState . targets . to fold)
    Pay _ v  -> Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut
{- END precondition2 -}
-}
  precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
    --Redeem _ -> (s ^. contractState . contributions . to fold) == (s ^. contractState . targets . to fold)
    --Refund w -> Nothing /= (s ^. contractState . contributions . at w)
    Pay _ v  -> Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut

{- START perform -}
  perform h _ _ a = case a of
    Pay w v        -> do
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.adaValueOf $ fromInteger v)
      delay 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      delay 1
{- END perform -}

{-    Refund w       -> do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      delay 1
-}

{- START arbitraryAction1 -}
  arbitraryAction _ = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30))
                                  , (1, Redeem <$> elements testWallets) ]
{- END arbitraryAction1 -}
                                  {- ++
                                  [ (1, Refund <$> elements (s ^. contractState . contributions . to Map.keys))
                                  | Prelude.not . null $ s ^. contractState . contributions . to Map.keys ]
                                  -}

{- START arbitraryAction2 -}
  arbitraryAction s = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                                  [ (1, Redeem <$> elements testWallets)
                                  | (s ^. contractState . contributions . to fold)   -- NEW
                                    `geq`                                            -- NEW
                                    (s ^. contractState . targets . to fold)         -- NEW
                                  ]
{- END arbitraryAction2 -}


{- START shrinkAction -}
  shrinkAction _ (Pay w n) = [Pay w n' | n' <- shrink n]
  shrinkAction _ _         = []
{- END shrinkAction -}

{-
  monitoring _ (Redeem _) = classify True "Contains Redeem"
  monitoring (s,s') _ = classify (redeemable s' && Prelude.not (redeemable s)) "Redeemable"
    where redeemable s = precondition s (Redeem undefined)
-}
{- START testWallets -}
testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]
{- END testWallets -}

{- START prop_Escrow -}
prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions_
{- END prop_Escrow -}

{-
finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy (const True)
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: (Wallet -> Bool) -> DL EscrowModel ()
finishingStrategy walletAlive = do
    contribs <- viewContractState contributions
    monitor (classify (Map.null contribs) "no need for extra refund to recover funds")
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs, walletAlive w]

prop_FinishEscrow :: Property
prop_FinishEscrow = forAllDL finishEscrow prop_Escrow

noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy (const True)
  , nlfpWalletStrategy = finishingStrategy . (==) }

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProof defaultCheckOptionsContractModel noLockProof
-}

tests :: TestTree
tests = testGroup "escrow"
    [ testProperty "QuickCheck ContractModel" $ withMaxSuccess 10 prop_Escrow
--    , testProperty "QuickCheck NoLockedFunds" $ withMaxSuccess 10 prop_NoLockedFunds
    ]

{- START escrowParams -}
escrowParams :: EscrowParams d
escrowParams =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w1) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 20)
        ]
    }
{- END escrowParams -}

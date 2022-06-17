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

module Escrow2(prop_Escrow, EscrowModel) where

import Control.Lens (makeLenses, to, (%=), (.=), (^.))
import Control.Monad (void)
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
                                         payToPaymentPubKeyTarget, redeemEp)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck (Arbitrary (shrink), Gen, Property, choose, elements, frequency, infiniteListOf, shrinkList,
                        sublistOf)

{- START ModelState -}
data EscrowModel = EscrowModel { _contributions :: Map Wallet Value.Value
                               , _targets       :: Map Wallet Value.Value
                               , _phase         :: Phase             -- NEW!
                               } deriving (Eq, Show, Data)
{- END ModelState -}

data Phase = Initial | Running deriving (Eq, Show, Data)

makeLenses ''EscrowModel

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where
{- START Action -}
  data Action EscrowModel = Init [(Wallet, Integer)]    -- NEW!
                          | Redeem Wallet
                          | Pay Wallet Integer
    deriving (Eq, Show, Data)
{- END Action -}

{- START ContractInstanceKey -}
  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)
{- END ContractInstanceKey -}

{- START initialState -}
  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _phase         = Initial
                             }
{- END initialState -}

{- START initialInstances -}
  initialInstances = []
{- END initialInstances -}

{- START startInstances -}
  startInstances _ (Init wns) =
    [CM.StartContract (WalletKey w) (escrowParams wns) | w <- testWallets]
  startInstances _ _ = []
{- END startInstances -}

  instanceWallet (WalletKey w) = w

{- START instanceContract -}
  instanceContract _ WalletKey{} params = testContract params
{- END instanceContract -}

{-
{- START nextState -}
  nextState a = case a of
    Init wns -> do
      phase   .= Running
      targets .= Map.fromList [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- wns]
    ...
{- END nextState -}
-}

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

{-
{- START precondition -}
  precondition s a = case a of
    Init _   -> currentPhase == Initial
    Redeem _ -> currentPhase == Running && ...
    Pay _ v  -> currentPhase == Running && ...
    where currentPhase = s ^. contractState . phase
{- END precondition -}

{- START tightprecondition -}
  precondition s a = case a of
    Init tgts-> currentPhase == Initial
             && and [Ada.adaValueOf (fromInteger n) `geq` Ada.toValue minAdaTxOut | (w,n) <- tgts]
    ...
{- END tightprecondition -}
-}

  precondition s a = case a of
    Init _   -> currentPhase == Initial
    Redeem _ -> currentPhase == Running
             && (s ^. CM.contractState . contributions . to fold) `Value.geq` (s ^. CM.contractState . targets . to fold)
    Pay _ v  -> currentPhase == Running
             && Ada.adaValueOf (fromInteger v) `Value.geq` Ada.toValue minAdaTxOut
    where currentPhase = s ^. CM.contractState . phase

{-
{- START perform -}
  perform h _ _ a = case a of
    Init _         -> do
      return ()
    ...
{- END perform -}
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

{-
{- START arbitraryAction -}
  arbitraryAction s
    | s ^.contractState . phase == Initial
      = Init <$> arbitraryTargets
    | otherwise
      = ...as before...
{- END arbitraryAction -}
-}

  arbitraryAction s
    | s ^.CM.contractState . phase == Initial
      = Init <$> arbitraryTargets
    | otherwise
      = frequency $ (3, Pay <$> elements testWallets <*> choose (1, 30)) :
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. CM.contractState . contributions . to fold) `Value.geq` (s ^. CM.contractState . targets . to fold)
                    ]

{- START shrinkAction -}
  shrinkAction _ (Init tgts) = map Init (shrinkList (\(w,n)->(w,) <$> shrink n) tgts)
{- END shrinkAction -}
  shrinkAction _ (Pay w n)   = [Pay w n' | n' <- shrink n]
  shrinkAction _ _           = []

{- START arbitraryTargets -}
arbitraryTargets :: Gen [(Wallet,Integer)]
arbitraryTargets = do
  ws <- sublistOf testWallets
  vs <- infiniteListOf $ choose (1,30)
  return $ zip ws vs
{- END arbitraryTargets -}

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

{- START testContract -}
testContract :: EscrowParams Datum -> Contract () EscrowSchema EscrowError ()
testContract params = selectList [ void $ payEp params
                                 , void $ redeemEp params
                                 ] >> testContract params
{- END testContract -}

prop_Escrow :: CM.Actions EscrowModel -> Property
prop_Escrow = CM.propRunActions_

{- START escrowParams -}
escrowParams :: [(Wallet, Integer)] -> EscrowParams d
escrowParams tgts =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w) (Ada.adaValueOf (fromInteger n))
        | (w,n) <- tgts
        ]
    }
{- END escrowParams -}

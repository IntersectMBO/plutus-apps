{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

--  This module contains a contract model for positive testing of the
--  simplified escrow contract in
--  Plutus.Contracts.Tutorial.Escrow. The code is explained in the
--  'Basic Contract Models' section of the contract model tutorial.

module Spec.Tutorial.Escrow1(prop_Escrow, EscrowModel) where

import Control.Lens hiding (both, elements)
import Control.Monad (void)
import Data.Data
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (minAdaTxOut)
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
                               } deriving (Eq, Show, Data)

makeLenses ''EscrowModel

deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where
  data Action EscrowModel = Pay Wallet Integer
                          | Redeem Wallet
    deriving (Eq, Show, Data)

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError ()

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.fromList [ (w1, Ada.adaValueOf 10)
                                                             , (w2, Ada.adaValueOf 20)
                                                             ]
                             }

  initialInstances = [StartContract (WalletKey w) () | w <- testWallets]

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} _ = testContract
    where
      testContract = selectList [ void $ payEp escrowParams
                                , void $ redeemEp escrowParams
                                ] >> testContract

  nextState a = case a of
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

  precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
    Pay _ v  -> Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut

  perform h _ _ a = case a of
    Pay w v        -> do
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.adaValueOf $ fromInteger v)
      delay 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      delay 1

  arbitraryAction s = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                                  [ (1, Redeem <$> elements testWallets)
                                  | (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
                                  ]

  shrinkAction _ (Pay w n) = [Pay w n' | n' <- shrink n]
  shrinkAction _ _         = []

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions_

escrowParams :: EscrowParams d
escrowParams =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w1) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 20)
        ]
    }

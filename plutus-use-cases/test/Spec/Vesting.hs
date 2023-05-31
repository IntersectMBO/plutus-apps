{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Spec.Vesting (VestingModel, tests, prop_Vesting, prop_CheckNoLockedFundsProof, retrieveFundsTrace) where

import Control.Lens hiding (elements)
import Control.Monad (void, when)
import Data.Default (Default (def))
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Slot
import Ledger.Time (POSIXTime)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Vesting
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value
import Plutus.Trace.Emulator (EmulatorTrace, callEndpoint)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified
import PlutusTx.Numeric qualified as Numeric
import Prelude
import Test.QuickCheck hiding ((.&&.))

-- | The scenario used in the property tests. It sets up a vesting scheme for a
--   total of 60 ada over 20 blocks (20 ada can be taken out before
--   that, at 10 blocks).
vesting :: POSIXTime -> VestingParams
vesting startTime =
    VestingParams
        { vestingTranche1 = VestingTranche (startTime + 10000) (Ada.adaValueOf 20)
        , vestingTranche2 = VestingTranche (startTime + 20000) (Ada.adaValueOf 40)
        , vestingOwner    = mockWalletPaymentPubKeyHash w1 }

params :: VestingParams
params = vesting (TimeSlot.scSlotZeroTime def)

-- * QuickCheck model

data VestingModel =
  VestingModel { _vestedAmount :: Value -- ^ How much value is in the contract
               , _vested       :: [Wallet] -- ^ What wallets have already vested money
               , _t1Slot       :: Slot -- ^ The time for the first tranche
               , _t2Slot       :: Slot -- ^ The time for the second tranche
               , _t1Amount     :: Value -- ^ The size of the first tranche
               , _t2Amount     :: Value -- ^ The size of the second tranche
               } deriving (Show, Eq, Generic)

makeLenses 'VestingModel

deriving instance Eq (ContractInstanceKey VestingModel w schema err params)
deriving instance Show (ContractInstanceKey VestingModel w schema err params)

-- This instance models the behaviour of the vesting contract. There are some peculiarities
-- that stem from the implementation of the contract that are apparent in the precondition
-- to the `Vest` endpoint.
instance ContractModel VestingModel where
  data ContractInstanceKey VestingModel w schema err params where
    WalletKey :: Wallet -> ContractInstanceKey VestingModel () VestingSchema VestingError ()

  data Action VestingModel = Vest Wallet
                           | Retrieve Wallet Value
                           deriving (Eq, Show, Generic)

  initialState = VestingModel
    { _vestedAmount = mempty
    , _vested       = []
    , _t1Slot       = TimeSlot.posixTimeToEnclosingSlot def $ vestingTrancheDate (vestingTranche1 params)
    , _t2Slot       = TimeSlot.posixTimeToEnclosingSlot def $ vestingTrancheDate (vestingTranche2 params)
    , _t1Amount     = vestingTrancheAmount (vestingTranche1 params)
    , _t2Amount     = vestingTrancheAmount (vestingTranche2 params) }

  initialInstances = (`StartContract` ()) . WalletKey <$> [w1, w2, w3]

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} _ = vestingContract params

  perform handle _ _ cmd = case cmd of
    Vest w -> do
      callEndpoint @"vest funds" (handle $ WalletKey w) ()
      delay 1

    Retrieve w val -> do
      callEndpoint @"retrieve funds" (handle $ WalletKey w) val
      delay 2

  -- Vest the sum of the two tranches
  nextState (Vest w) = do
    let amount =  vestingTrancheAmount (vestingTranche1 params)
               <> vestingTrancheAmount (vestingTranche2 params)
    withdraw w amount
    vestedAmount %= (<> amount)
    vested       %= (w:)
    wait 1

  -- Retrieve `v` value as long as that leaves enough value to satisfy
  -- the tranche requirements
  nextState (Retrieve w v) = do
    slot   <- viewModelState currentSlot
    amount <- viewContractState vestedAmount
    let newAmount = amount Numeric.- v
    s      <- viewContractState id
    when ( enoughValueLeft slot s v
         && v `leq` amount
         && mockWalletPaymentPubKeyHash w == vestingOwner params
         && Ada.fromValue v >= Ledger.minAdaTxOutEstimated
         && (Ada.fromValue newAmount == 0 || Ada.fromValue newAmount >= Ledger.minAdaTxOutEstimated)) $ do
      deposit w v
      vestedAmount .= newAmount
    wait 2

  precondition s (Vest w) =  w `notElem` s ^. contractState . vested -- After a wallet has vested the contract shuts down
                          && mockWalletPaymentPubKeyHash w /= vestingOwner params -- The vesting owner shouldn't vest
                          && slot < t1 -- If you vest after slot 1 it can cause the vesting owner to terminate prematurely
    where
      slot   = s ^. currentSlot
      t1     = s ^. contractState . t1Slot

  precondition s (Retrieve w v) = enoughValueLeft slot (s ^. contractState) v
                                && mockWalletPaymentPubKeyHash w == vestingOwner params
                                && Ada.fromValue v >= Ledger.minAdaTxOutEstimated
                                && (Ada.fromValue newAmount == 0 || Ada.fromValue newAmount >= Ledger.minAdaTxOutEstimated)
    where
      slot   = s ^. currentSlot
      amount = s ^. contractState . vestedAmount
      newAmount = amount Numeric.- v

  arbitraryAction s = frequency [ (1, Vest <$> genWallet)
                                , (1, Retrieve <$> genWallet
                                               <*> (Ada.lovelaceValueOf
                                                   <$> choose (Ada.getLovelace Ledger.minAdaTxOutEstimated, valueOf amount Ada.adaSymbol Ada.adaToken)
                                                   )
                                  )
                                ]
    where
      amount   = s ^. contractState . vestedAmount



  shrinkAction _ (Vest _)       = []
  shrinkAction _ (Retrieve w v) = Retrieve w <$> shrinkValue v

-- | Check that the amount of value left in the contract
-- is at least the amount that remains locked at the current
-- slot.
enoughValueLeft :: Slot -- ^ current slot
                -> VestingModel
                -> Value
                -> Bool
enoughValueLeft slot s take =
  let availableValue   = mconcat $ [ t1v | slot > t1 ] ++ [ t2v | slot > t2 ]
      totalValue       = t1v <> t2v
      mustRemainLocked = totalValue Numeric.- availableValue
  in mustRemainLocked `leq` (vested Numeric.- take)
  where
    vested = s ^. vestedAmount
    t1     = s ^. t1Slot
    t1v    = s ^. t1Amount
    t2     = s ^. t2Slot
    t2v    = s ^. t2Amount

wallets :: [Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen Wallet
genWallet = elements wallets

shrinkValue :: Value -> [Value]
shrinkValue v = Ada.lovelaceValueOf <$> filter (\val -> val >= Ada.getLovelace Ledger.minAdaTxOutEstimated) (shrink (valueOf v Ada.adaSymbol Ada.adaToken))

prop_Vesting :: Actions VestingModel -> Property
prop_Vesting = propRunActions_

noLockProof :: NoLockedFundsProof VestingModel
noLockProof = defaultNLFP {
      nlfpMainStrategy   = mainStrat,
      nlfpWalletStrategy = walletStrat }
    where
        -- To get all the money out simply wait until after the
        -- deadline and take as much money as has been vested.
        mainStrat = do
            amount <- viewContractState vestedAmount
            t2     <- viewContractState t2Slot
            slot   <- viewModelState currentSlot
            when (slot < t2 + Slot 1) $ do
              waitUntilDL $ t2 + Slot 1
            when (amount `gt` mempty) $ do
              action (Retrieve w1 amount)

        -- No one but w1 ever gets any money out of the contract.
        walletStrat w | w == w1 = mainStrat
                      | otherwise = return ()

prop_CheckNoLockedFundsProof :: Property
prop_CheckNoLockedFundsProof = checkNoLockedFundsProof noLockProof

-- Tests

tests :: TestTree
tests =
    let con = vestingContract (vesting startTime) in
    testGroup "vesting"
    [ checkPredicate "secure some funds with the vesting script"
        (walletFundsChangePlutus w2 (Numeric.negate $ totalAmount $ vesting startTime))
        $ do
            hdl <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl ()
            void $ Trace.waitNSlots 1

    , checkPredicate "retrieve some funds"
        (walletFundsChangePlutus w2 (Numeric.negate $ totalAmount $ vesting startTime)
        .&&. assertNoFailedTransactions
        .&&. walletFundsChangePlutus w1 (Ada.adaValueOf 10))
        retrieveFundsTrace

    , checkPredicate "cannot retrieve more than allowed"
        (walletFundsChangePlutus w1 mempty
        .&&. assertContractError con (Trace.walletInstanceTag w1) (== expectedError) "error should match")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl2 ()
            Trace.waitNSlots 10
            Trace.callEndpoint @"retrieve funds" hdl1 (Ada.adaValueOf 30)
            void $ Trace.waitNSlots 1

    , checkPredicate "can retrieve everything at the end"
        (walletFundsChangePlutus w1 (totalAmount $ vesting startTime)
        .&&. assertNoFailedTransactions
        .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "should be done")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl2 ()
            Trace.waitNSlots 20
            Trace.callEndpoint @"retrieve funds" hdl1 (totalAmount $ vesting startTime)
            void $ Trace.waitNSlots 2

    , goldenPir "test/Spec/vesting.pir" $$(PlutusTx.compile [|| validate ||])
    , HUnit.testCaseSteps "script size is reasonable" $ \step -> reasonable' step (vestingScript $ vesting startTime) 33000
    , testProperty "prop_Vesting" $ withMaxSuccess 20 prop_Vesting
    , testProperty "prop_CheckNoLockedFundsProof" $ withMaxSuccess 20 prop_CheckNoLockedFundsProof
    -- TODO: re-activate when double satisfaction is turned on again
    -- , testProperty "prop_doubleSatisfaction" $ withMaxSuccess 20 prop_doubleSatisfaction
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

-- TODO: re-activate when double satisfaction is turned on again
-- prop_doubleSatisfaction :: Actions VestingModel -> Property
-- prop_doubleSatisfaction = checkDoubleSatisfaction

retrieveFundsTrace :: EmulatorTrace ()
retrieveFundsTrace = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let con = vestingContract (vesting startTime)
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    Trace.callEndpoint @"vest funds" hdl2 ()
    Trace.waitNSlots 10
    Trace.callEndpoint @"retrieve funds" hdl1 (Ada.adaValueOf 10)
    void $ Trace.waitNSlots 2

expectedError :: VestingError
expectedError =
    let payment = Ada.adaValueOf 30
        maxPayment = Ada.adaValueOf 20
        mustRemainLocked = Ada.adaValueOf 40
    in InsufficientFundsError payment maxPayment mustRemainLocked

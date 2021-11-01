{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}

module Spec.Crowdfunding where

import Control.Foldl qualified as L
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Freer (run)
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Default (Default (..))
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.Encoding qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty, vsep)
import Prettyprinter.Render.Text (renderStrict)
import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck hiding ((.&&.))

import Ledger (Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Slot (Slot (..))
import Ledger.Time (POSIXTime)
import Ledger.TimeSlot qualified as TimeSlot
import Plutus.Contract hiding (currentSlot, runError)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Crowdfunding
import Plutus.Trace.Emulator (ContractHandle (..), EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream (filterLogLevel, foldEmulatorStreamM)

-- This has to be here because of the call to `makeLenses` below
data CrowdfundingModel = CrowdfundingModel { _contributions       :: Map Wallet Value
                                           , _ownerWallet         :: Wallet
                                           , _ownerContractDone   :: Bool
                                           , _ownerOnline         :: Bool
                                           , _collectDeadlineSlot :: Slot
                                           , _endSlot             :: Slot
                                           } deriving (Show)

makeLenses ''CrowdfundingModel


theContract :: POSIXTime -> Contract () CrowdfundingSchema ContractError ()
theContract startTime = crowdfunding $ theCampaign startTime

tests :: TestTree
tests = testGroup "crowdfunding"
    [ checkPredicate "Expose 'contribute' and 'scheduleCollection' endpoints"
        (endpointAvailable @"contribute" (theContract startTime) (Trace.walletInstanceTag w1)
        .&&. endpointAvailable @"schedule collection" (theContract startTime) (Trace.walletInstanceTag w1)
        )
        $ do
            slotCfg <- Trace.getSlotConfig
            void (Trace.activateContractWallet w1 $ theContract $ TimeSlot.scSlotZeroTime slotCfg)

    , checkPredicateOptions defaultCheckOptions "make contribution"
        (walletFundsChange w1 (Ada.adaValueOf (-10)))
        $ let contribution = Ada.adaValueOf 10
          in makeContribution w1 contribution >> void Trace.nextSlot

    , checkPredicate "make contributions and collect"
        (walletFundsChange w1 (Ada.adaValueOf 22.5))
        successfulCampaign

    , checkPredicate "cannot collect money too late"
        (walletFundsChange w1 PlutusTx.zero
        .&&. assertNoFailedTransactions)
        $ do
            ContractHandle{chInstanceId} <- startCampaign
            makeContribution w2 (Ada.adaValueOf 10)
            makeContribution w3 (Ada.adaValueOf 10)
            makeContribution w4 (Ada.adaValueOf 2.5)
            Trace.freezeContractInstance chInstanceId
            -- Add some blocks to bring the total up to 31
            -- (that is, above the collection deadline)
            void $ Trace.waitUntilSlot (Slot 31)
            -- Then inform the wallet. It's too late to collect the funds
            -- now.
            Trace.thawContractInstance chInstanceId

    , checkPredicate "cannot collect unless notified"
        (walletFundsChange w1 PlutusTx.zero)
        $ do
            ContractHandle{chInstanceId} <- startCampaign
            makeContribution w2 (Ada.adaValueOf 10)
            makeContribution w3 (Ada.adaValueOf 10)
            makeContribution w4 (Ada.adaValueOf 2.5)
            Trace.freezeContractInstance chInstanceId
            -- The contributions could be collected now, but without
            -- the slot notifications, wallet 1 is not aware that the
            -- time has come, so it does not submit the transaction.
            void $ Trace.waitUntilSlot 35

    , checkPredicate "can claim a refund"
        (walletFundsChange w1 mempty
        .&&. walletFundsChange w2 mempty
        .&&. walletFundsChange w3 mempty)
        $ do
            ContractHandle{chInstanceId} <- startCampaign
            makeContribution w2 (Ada.adaValueOf 50)
            void $ makeContribution w3 (Ada.adaValueOf 50)
            Trace.freezeContractInstance chInstanceId
            void $ Trace.waitUntilSlot 31

    , goldenPir "test/Spec/crowdfunding.pir" $$(PlutusTx.compile [|| mkValidator ||])
    ,   let
            deadline = 10000
            collectionDeadline = 15000
            owner = w1
            cmp = mkCampaign deadline collectionDeadline owner
        in HUnit.testCaseSteps "script size is reasonable" $ \step -> reasonable' step (contributionScript cmp) 30000

    , goldenVsString
        "renders the log of a single contract instance sensibly"
        "test/Spec/crowdfundingWallet1TestOutput.txt"
        (pure $ renderWalletLog successfulCampaign)

    , goldenVsString
        "renders the emulator log sensibly"
        "test/Spec/crowdfundingEmulatorTestOutput.txt"
        (pure $ renderEmulatorLog successfulCampaign)

    , let con :: Contract () EmptySchema ContractError () = throwError "something went wrong" in
        goldenVsString
        "renders an error sensibly"
        "test/Spec/contractError.txt"
        (pure $ renderWalletLog (void $ Trace.activateContractWallet w1 con))

    , testProperty "QuickCheck ContractModel" $ withMaxSuccess 10 prop_Crowdfunding

    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

renderWalletLog :: EmulatorTrace () -> ByteString
renderWalletLog trace =
    let result =
            run
            $ foldEmulatorStreamM (L.generalize $ Folds.instanceLog (Trace.walletInstanceTag w1))
            $ filterLogLevel Info
            $ Trace.runEmulatorStream def trace
    in BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result

renderEmulatorLog :: EmulatorTrace () -> ByteString
renderEmulatorLog trace =
    let result =
            run
            $ foldEmulatorStreamM (L.generalize Folds.emulatorLog)
            $ filterLogLevel Info
            $ Trace.runEmulatorStream def trace
    in BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result


params :: Campaign
params = theCampaign (TimeSlot.scSlotZeroTime def)

deriving instance Eq (Action CrowdfundingModel)
deriving instance Show (Action CrowdfundingModel)

deriving instance Eq (ContractInstanceKey CrowdfundingModel w schema err)
deriving instance Show (ContractInstanceKey CrowdfundingModel w schema err)

instance ContractModel CrowdfundingModel where
  data Action CrowdfundingModel = CContribute Wallet Value
                                | CWaitUntil Slot
                                | CStart

  data ContractInstanceKey CrowdfundingModel w schema err where
    ContributorKey :: Wallet -> ContractInstanceKey CrowdfundingModel () CrowdfundingSchema ContractError
    OwnerKey :: Wallet -> ContractInstanceKey CrowdfundingModel () CrowdfundingSchema ContractError

  initialState = CrowdfundingModel { _contributions       = Map.empty
                                   , _ownerWallet         = w1
                                   , _ownerOnline         = False
                                   , _ownerContractDone   = False
                                   , _collectDeadlineSlot = TimeSlot.posixTimeToEnclosingSlot def $ campaignCollectionDeadline params
                                   , _endSlot             = TimeSlot.posixTimeToEnclosingSlot def $ campaignDeadline params
                                   }

  initialHandleSpecs = ContractInstanceSpec (OwnerKey w1) w1 (crowdfunding params) :
                       [ ContractInstanceSpec (ContributorKey w) w (crowdfunding params) | w <- contributorWallets ]

  perform h s a = case a of
    CWaitUntil slot -> void $ Trace.waitUntilSlot slot
    CContribute w v -> Trace.callEndpoint @"contribute" (h $ ContributorKey w) Contribution{contribValue=v}
    CStart          -> Trace.callEndpoint @"schedule collection" (h $ OwnerKey $ s ^. contractState . ownerWallet) ()

  nextState a = case a of
    CWaitUntil slot -> waitUntil slot
    CContribute w v -> do
      withdraw w v
      contributions $~ Map.insert w v
    CStart -> do
      ownerOnline $= True

  nextReactiveState slot' = do
    -- If the owner is online and its after the
    -- contribution deadline deadline
    -- they collect all the money
    end <- viewContractState endSlot
    online  <- viewContractState ownerOnline
    when (slot' >= end && online) $ do
      owner   <- viewContractState ownerWallet
      cMap <- viewContractState contributions
      deposit owner (fold cMap)
      contributions $= Map.empty
      ownerOnline $= False
      ownerContractDone $= True
    -- If its after the end of the collection time range
    -- the remaining funds are collected by the contracts
    collectDeadline <- viewContractState collectDeadlineSlot
    when (slot' >= collectDeadline) $ do
      cMap <- viewContractState contributions
      mapM_ (uncurry deposit) (Map.toList cMap)
      contributions $= Map.empty

  -- The 'precondition' says when a particular command is allowed.
  precondition s cmd = case cmd of
    CWaitUntil slot -> slot > s ^. currentSlot
    -- In order to contribute, we need to satisfy the constraint where each tx
    -- output must have at least N Ada.
    --
    -- We must make sure that we don't contribute a too high value such that:
    --   - we can't pay for fees anymore
    --   - have a tx output of less than N Ada.
    --
    -- We suppose the initial balance is 100 Ada. Needs to be changed if
    -- the emulator initialises the wallets with a different value.
    CContribute w v -> let currentWalletBalance = Ada.adaOf 100 + Ada.fromValue (s ^. balanceChange w)
                        in w `notElem` Map.keys (s ^. contractState . contributions)
                        && w /= (s ^. contractState . ownerWallet)
                        && s ^. currentSlot < s ^. contractState . endSlot
                        && Ada.fromValue v >= Ledger.minAdaTxOut
                        && (currentWalletBalance - Ada.fromValue v) >= (Ledger.minAdaTxOut <> Ledger.maxFee)
    CStart          -> Prelude.not (s ^. contractState . ownerOnline || s ^. contractState . ownerContractDone)

  -- To generate a random test case we need to know how to generate a random
  -- command given the current model state.
  arbitraryAction s = oneof $
    [ CWaitUntil . step <$> choose (1, 100 :: Integer) ]
    ++
    [ CContribute <$> QC.elements availableWallets <*> (Ada.lovelaceValueOf . abs <$> choose (2000000, 100000000))
    | Prelude.not . null $ availableWallets
    , s ^. currentSlot < s ^. contractState . endSlot ]
    ++
    [ pure CStart
    | Prelude.not (s ^. contractState . ownerOnline || s ^. contractState . ownerContractDone) ]
    where
      availableWallets = [ w | w <- contributorWallets, w `notElem` Map.keys (s ^. contractState . contributions) ]
      slot = s ^. currentSlot
      step n = slot + fromIntegral n

  shrinkAction _ a = case a of
    CWaitUntil s -> CWaitUntil . fromIntegral <$> (shrink . toInteger $ s)
    _            -> []

  monitoring _ _ = id

contributorWallets :: [Wallet]
contributorWallets = [w2, w3, w4, w5, w6, w7, w8, w9, w10]

prop_Crowdfunding :: Actions CrowdfundingModel -> Property
prop_Crowdfunding = propRunActions_

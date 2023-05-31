{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
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
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty, vsep)
import Prettyprinter.Render.Text (renderStrict)
import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck hiding ((.&&.))

import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Slot (Slot (..))
import Ledger.Time (POSIXTime)
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract hiding (currentSlot, runError)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Crowdfunding
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value)
import Plutus.Trace.Emulator (ContractHandle (..), EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified
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
                                           } deriving (Show, Generic)

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

    , checkPredicate "make contribution"
        (walletFundsChange w1 (Value.adaValueOf (-10)))
        $ let contribution = Ada.adaValueOf 10
          in makeContribution w1 contribution >> void Trace.nextSlot

    , checkPredicate "make contributions and collect"
        (walletFundsChange w1 (Value.adaValueOf 22.5))
        successfulCampaign

    , checkPredicate "cannot make contribution after campaign dealine"
        (walletFundsChange w1 mempty
        .&&. assertFailedTransaction (\_ err ->
            case err of
                Ledger.CardanoLedgerValidationError msg ->
                    "OutsideValidityIntervalUTxO" `Text.isInfixOf` msg
                _ -> False
            ))
        $ do
            void $ Trace.waitUntilSlot $ Slot 20
            makeContribution w1 (Ada.adaValueOf 10)

    , checkPredicate "cannot collect money too late"
        (walletFundsChange w1 mempty
        .&&. assertFailedTransaction (\_ err ->
            case err of
                Ledger.CardanoLedgerValidationError msg ->
                    "OutsideValidityIntervalUTxO" `Text.isInfixOf` msg
                _ -> False
            ))
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
        (walletFundsChange w1 mempty)
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

    , testProperty "QuickCheck ContractModel" prop_Crowdfunding

    , testProperty "start-at-slot-20" $ withMaxSuccess 1 $
        let fixedTestCase = do
              action $ CContribute w6 (Ada.lovelaceValueOf 20000000)
              waitUntilDL (Slot 20)
              action CStart
        in forAllDL fixedTestCase prop_Crowdfunding
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

deriving instance Eq (ContractInstanceKey CrowdfundingModel w schema err params)
deriving instance Show (ContractInstanceKey CrowdfundingModel w schema err params)

instance ContractModel CrowdfundingModel where
  data Action CrowdfundingModel = CContribute Wallet Value
                                | CStart
                                deriving (Eq, Show, Generic)

  data ContractInstanceKey CrowdfundingModel w schema err params where
    ContributorKey :: Wallet -> ContractInstanceKey CrowdfundingModel () CrowdfundingSchema ContractError ()
    OwnerKey :: Wallet -> ContractInstanceKey CrowdfundingModel () CrowdfundingSchema ContractError ()

  initialState = CrowdfundingModel { _contributions       = Map.empty
                                   , _ownerWallet         = w1
                                   , _ownerOnline         = False
                                   , _ownerContractDone   = False
                                   , _collectDeadlineSlot = TimeSlot.posixTimeToEnclosingSlot def $ campaignCollectionDeadline params
                                   , _endSlot             = TimeSlot.posixTimeToEnclosingSlot def $ campaignDeadline params
                                   }

  initialInstances = StartContract (OwnerKey w1) () : [ StartContract (ContributorKey w) () | w <- contributorWallets ]

  instanceWallet (OwnerKey w)       = w
  instanceWallet (ContributorKey w) = w

  instanceContract _ OwnerKey{}       _ = crowdfunding params
  instanceContract _ ContributorKey{} _ = crowdfunding params

  perform h _ s a = case a of
    CContribute w v -> do
      Trace.callEndpoint @"contribute" (h $ ContributorKey w) Contribution{contribValue=v}
      delay 1
    CStart          -> do
      Trace.callEndpoint @"schedule collection" (h $ OwnerKey $ s ^. contractState . ownerWallet) ()
      delay 1

  nextState a = case a of
    CContribute w v -> do
      withdraw w v
      contributions %= Map.insert w v
      wait 1
    CStart -> do
      slot' <- viewModelState currentSlot
      end <- viewContractState endSlot
      wait 1
      -- Collecting happens immediately if the campaign deadline has passed
      if slot' < end
        then do
          ownerOnline .= True
        else do
          cMap <- viewContractState contributions
          owner <- viewContractState ownerWallet
          deposit owner (fold cMap)
          contributions .= Map.empty
          ownerContractDone .= True

  nextReactiveState slot' = do
    -- If the owner is online and its after the
    -- contribution deadline deadline
    -- they collect all the money
    end <- viewContractState endSlot
    online <- viewContractState ownerOnline
    when (slot' > end && online) $ do
      owner <- viewContractState ownerWallet
      cMap <- viewContractState contributions
      deposit owner (fold cMap)
      contributions .= Map.empty
      ownerOnline .= False
      ownerContractDone .= True
    -- If its after the end of the collection time range
    -- the remaining funds are collected by the contracts
    collectDeadline <- viewContractState collectDeadlineSlot
    when (slot' > collectDeadline) $ do
      cMap <- viewContractState contributions
      mapM_ (uncurry deposit) (Map.toList cMap)
      contributions .= Map.empty

  -- The 'precondition' says when a particular command is allowed.
  precondition s cmd = case cmd of
    -- In order to contribute, we need to satisfy the constraint where each tx
    -- output must have at least N Ada.
    CContribute w v -> w `notElem` Map.keys (s ^. contractState . contributions)
                    && w /= (s ^. contractState . ownerWallet)
                    && s ^. currentSlot < s ^. contractState . endSlot
                    && Ada.fromValue v >= Ledger.minAdaTxOutEstimated
    CStart          -> Prelude.not (s ^. contractState . ownerOnline || s ^. contractState . ownerContractDone)

  -- To generate a random test case we need to know how to generate a random
  -- command given the current model state.

  arbitraryAction s = oneof $
    [ CContribute <$> QC.elements availableWallets <*> (Ada.lovelaceValueOf . abs <$> choose (2000000, 100000000))
    | Prelude.not . null $ availableWallets
    , s ^. currentSlot < s ^. contractState . endSlot ]
    ++
    [ pure CStart
    | Prelude.not (s ^. contractState . ownerOnline || s ^. contractState . ownerContractDone) ]
    where
      availableWallets = [ w | w <- contributorWallets, w `notElem` Map.keys (s ^. contractState . contributions) ]

  waitProbability s
    | Prelude.not . null $ availableWallets
    , s ^. currentSlot < s ^. contractState . endSlot = 0.05
    | Prelude.not (s ^. contractState . ownerOnline || s ^. contractState . ownerContractDone) = 0.05
    | otherwise = 1
    where
      availableWallets = [ w | w <- contributorWallets, w `notElem` Map.keys (s ^. contractState . contributions) ]

  shrinkAction _ _ = []

  monitoring _ _ = id

contributorWallets :: [Wallet]
contributorWallets = [w2, w3, w4, w5, w6, w7, w8, w9, w10]

prop_Crowdfunding :: Actions CrowdfundingModel -> Property
prop_Crowdfunding actions = propRunActionsWithOptions
    (defaultCheckOptionsContractModel & increaseTransactionLimits)
    defaultCoverageOptions
    (\ _ -> pure True)
    actions


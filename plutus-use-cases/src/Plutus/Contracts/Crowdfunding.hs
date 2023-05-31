-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.Crowdfunding (
    -- * Campaign parameters
      Campaign(..)
    , CrowdfundingSchema
    , crowdfunding
    , theCampaign
    -- * Functionality for campaign contributors
    , contribute
    , Contribution(..)
    -- * Functionality for campaign owners
    , scheduleCollection
    , campaignAddress
    -- * Validator script
    , contributionScript
    , mkValidator
    , mkCampaign
    , CampaignAction(..)
    , collectionRange
    , refundRange
    -- * Traces
    , startCampaign
    , makeContribution
    , successfulCampaign
    ) where

import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Control.Applicative (Applicative (..))
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), getCardanoTxId)
import Ledger qualified
import Ledger.Interval qualified as Interval
import Ledger.Tx.CardanoAPI (fromCardanoTxId)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.ValidityInterval qualified as ValidityInterval
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Plutus.Contract
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V2.Scripts as V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2 hiding (validatorHash)
import Plutus.Trace.Effects.EmulatorControl (getSlotConfig)
import Plutus.Trace.Emulator (ContractHandle, EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), return, (<$>), (>>), (>>=))
import Prelude (Semigroup (..), (<$>), (>>=))
import Prelude qualified as Haskell
import Wallet.Emulator (Wallet (..), knownWallet)
import Wallet.Emulator qualified as Emulator

-- | A crowdfunding campaign.
data Campaign = Campaign
    { campaignDeadline           :: V2.POSIXTime
    -- ^ The date by which the campaign funds can be contributed.
    , campaignCollectionDeadline :: V2.POSIXTime
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignOwner              :: PaymentPubKeyHash
    -- ^ Public key of the campaign owner. This key is entitled to retrieve the
    --   funds if the campaign is successful.
    } deriving (Generic, ToJSON, FromJSON, Haskell.Show)

PlutusTx.makeLift ''Campaign

-- | Action that can be taken by the participants in this contract. A value of
--   `CampaignAction` is provided as the redeemer. The validator script then
--   checks if the conditions for performing this action are met.
--
data CampaignAction = Collect | Refund

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
    Endpoint "schedule collection" ()
    .\/ Endpoint "contribute" Contribution

newtype Contribution = Contribution
        { contribValue :: V2.Value
        -- ^ how much to contribute
        } deriving stock (Haskell.Eq, Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | Construct a 'Campaign' value from the campaign parameters,
--   using the wallet's public key.
mkCampaign :: V2.POSIXTime -> V2.POSIXTime -> Wallet -> Campaign
mkCampaign ddl collectionDdl ownerWallet =
    Campaign
        { campaignDeadline = ddl
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = Emulator.mockWalletPaymentPubKeyHash ownerWallet
        }

-- | The 'ValidityInterval POSIXTime' during which the funds can be collected
{-# INLINABLE collectionRange #-}
collectionRange :: Campaign -> ValidityInterval.ValidityInterval V2.POSIXTime
collectionRange cmp = ValidityInterval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp)

-- | The 'ValidityInterval POSIXTime' during which a refund may be claimed
{-# INLINABLE refundRange #-}
refundRange :: Campaign -> ValidityInterval.ValidityInterval V2.POSIXTime
refundRange cmp = ValidityInterval.from (campaignCollectionDeadline cmp)

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PaymentPubKeyHash

typedValidator :: Campaign -> V2.TypedValidator Crowdfunding
typedValidator = V2.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PaymentPubKeyHash -> V2.TxInfo -> Bool
validRefund campaign contributor txinfo =
    -- Check that the transaction falls in the refund range of the campaign
    ValidityInterval.toPlutusInterval (refundRange campaign) `Interval.contains` V2.txInfoValidRange txinfo
    -- Check that the transaction is signed by the contributor
    && (txinfo `V2.txSignedBy` unPaymentPubKeyHash contributor)

{-# INLINABLE validCollection #-}
validCollection :: Campaign -> V2.TxInfo -> Bool
validCollection campaign txinfo =
    -- Check that the transaction falls in the collection range of the campaign
    (ValidityInterval.toPlutusInterval (collectionRange campaign) `Interval.contains` V2.txInfoValidRange txinfo)
    -- Check that the transaction is signed by the campaign owner
    && (txinfo `V2.txSignedBy` unPaymentPubKeyHash (campaignOwner campaign))

{-# INLINABLE mkValidator #-}
-- | The validator script is of type 'CrowdfundingValidator', and is
-- additionally parameterized by a 'Campaign' definition. This argument is
-- provided by the Plutus client, using 'PlutusTx.applyCode'.
-- As a result, the 'Campaign' definition is part of the script address,
-- and different campaigns have different addresses. The Campaign{..} syntax
-- means that all fields of the 'Campaign' value are in scope
-- (for example 'campaignDeadline' in l. 70).
mkValidator :: Campaign -> PaymentPubKeyHash -> CampaignAction -> V2.ScriptContext -> Bool
mkValidator c con act V2.ScriptContext{V2.scriptContextTxInfo} = case act of
    -- the "refund" branch
    Refund  -> validRefund c con scriptContextTxInfo
    -- the "collection" branch
    Collect -> validCollection c scriptContextTxInfo

-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
--
contributionScript :: Campaign -> V2.Validator
contributionScript = V2.validatorScript . typedValidator

-- | The address of a [[Campaign]]
campaignAddress :: Campaign -> V2.ValidatorHash
campaignAddress = V2.validatorHash . contributionScript

-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: Campaign -> Contract () CrowdfundingSchema ContractError ()
crowdfunding c = selectList [contribute c, scheduleCollection c]

-- | A sample campaign
theCampaign :: V2.POSIXTime -> Campaign
theCampaign startTime = Campaign
    { campaignDeadline = startTime + 20000
    , campaignCollectionDeadline = startTime + 30000
    , campaignOwner = Emulator.mockWalletPaymentPubKeyHash (knownWallet 1)
    }

-- | The "contribute" branch of the contract for a specific 'Campaign'. Exposes
--   an endpoint that allows the user to enter their public key and the
--   contribution. Then waits until the campaign is over, and collects the
--   refund if the funding was not collected.
contribute :: Campaign -> Promise () CrowdfundingSchema ContractError ()
contribute cmp = endpoint @"contribute" $ \Contribution{contribValue} -> do
    logInfo @Text $ "Contributing " <> Text.pack (Haskell.show contribValue)
    contributor <- ownFirstPaymentPubKeyHash
    let inst = typedValidator cmp
        validityTimeRange = ValidityInterval.lessThan (campaignDeadline cmp)
        tx = Constraints.mustPayToTheScriptWithDatumInTx contributor contribValue
                <> Constraints.mustValidateInTimeRange validityTimeRange
    txid <- fmap getCardanoTxId $ mkTxConstraints (Constraints.typedValidatorLookups inst) tx
        >>= adjustUnbalancedTx >>= submitUnbalancedTx

    utxo <- watchAddressUntilTime (Scripts.validatorCardanoAddress Params.testnet inst) $ campaignCollectionDeadline cmp

    -- 'utxo' is the set of unspent outputs at the campaign address at the
    -- collection deadline. If 'utxo' still contains our own contribution
    -- then we can claim a refund.

    let flt Ledger.TxOutRef{txOutRefId} _ = fromCardanoTxId txid Haskell.== txOutRefId
        tx' = Constraints.spendUtxosFromTheScriptFilter flt utxo Refund
                <> Constraints.mustValidateInTimeRange (refundRange cmp)
                <> Constraints.mustBeSignedBy contributor
    if Constraints.modifiesUtxoSet tx'
    then do
        logInfo @Text "Claiming refund"
        void $ mkTxConstraints (Constraints.typedValidatorLookups inst
                             <> Constraints.unspentOutputs utxo) tx'
            >>= adjustUnbalancedTx >>= submitUnbalancedTx
    else pure ()

-- | The campaign owner's branch of the contract for a given 'Campaign'. It
--   watches the campaign address for contributions and collects them if
--   the funding goal was reached in time.
scheduleCollection :: Campaign -> Promise () CrowdfundingSchema ContractError ()
scheduleCollection cmp = endpoint @"schedule collection" $ \() -> do
    let inst = typedValidator cmp

    -- Expose an endpoint that lets the user fire the starting gun on the
    -- campaign. (This endpoint isn't technically necessary, we could just
    -- run the 'trg' action right away)
    logInfo @Text "Campaign started. Waiting for campaign deadline to collect funds."

    _ <- awaitTime $ campaignDeadline cmp
    unspentOutputs <- utxosAt (Scripts.validatorCardanoAddress Params.testnet inst)

    let tx = Constraints.spendUtxosFromTheScript unspentOutputs Collect
            <> Constraints.mustBeSignedBy (campaignOwner cmp)
            <> Constraints.mustValidateInTimeRange (collectionRange cmp)

    logInfo @Text "Collecting funds"
    void $ mkTxConstraints (Constraints.typedValidatorLookups inst
                         <> Constraints.unspentOutputs unspentOutputs) tx
        >>= adjustUnbalancedTx >>= submitUnbalancedTx

-- | Call the "schedule collection" endpoint and instruct the campaign owner's
--   wallet (wallet 1) to start watching the campaign address.
startCampaign :: EmulatorTrace (ContractHandle () CrowdfundingSchema ContractError)
startCampaign = do
    startTime <- TimeSlot.scSlotZeroTime <$> getSlotConfig
    hdl <- Trace.activateContractWallet (knownWallet 1) (crowdfunding $ theCampaign startTime)
    Trace.callEndpoint @"schedule collection" hdl ()
    pure hdl

-- | Call the "contribute" endpoint, contributing the amount from the wallet
makeContribution :: Wallet -> V2.Value -> EmulatorTrace ()
makeContribution w v = do
    startTime <- TimeSlot.scSlotZeroTime <$> getSlotConfig
    hdl <- Trace.activateContractWallet w (crowdfunding $ theCampaign startTime)
    Trace.callEndpoint @"contribute" hdl Contribution{contribValue=v}

-- | Run a successful campaign with contributions from wallets 2, 3 and 4.
successfulCampaign :: EmulatorTrace ()
successfulCampaign = do
    _ <- startCampaign
    makeContribution (knownWallet 2) (Ada.adaValueOf 10)
    makeContribution (knownWallet 3) (Ada.adaValueOf 10)
    makeContribution (knownWallet 4) (Ada.adaValueOf 2.5)
    void $ Trace.waitUntilSlot 21

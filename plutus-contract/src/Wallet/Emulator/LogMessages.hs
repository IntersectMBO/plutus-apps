{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
-- | The log messages produced by the emulator.
module Wallet.Emulator.LogMessages(
  RequestHandlerLogMsg(..)
  , TxBalanceMsg(..)
  , _BalancingUnbalancedTx
  ) where

import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (Address, CardanoTx, getCardanoTxId)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Slot (Slot)
import Ledger.Value (Value)
import Prettyprinter (Pretty (..), hang, viaShow, vsep, (<+>))
import Wallet.Emulator.Error (WalletAPIError)

data RequestHandlerLogMsg =
    SlotNoticationTargetVsCurrent Slot Slot
    | StartWatchingContractAddresses
    | HandleTxFailed WalletAPIError
    | UtxoAtFailed Address
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty RequestHandlerLogMsg where
    pretty = \case
        SlotNoticationTargetVsCurrent target current ->
            "target slot:" <+> pretty target <> "; current slot:" <+> pretty current
        StartWatchingContractAddresses -> "Start watching contract addresses"
        HandleTxFailed e -> "handleTx failed:" <+> viaShow e
        UtxoAtFailed addr -> "UtxoAt failed:" <+> pretty addr

data TxBalanceMsg =
    BalancingUnbalancedTx UnbalancedTx
    | NoOutputsAdded
    | AddingPublicKeyOutputFor Value
    | NoInputsAdded
    | AddingInputsFor Value
    | NoCollateralInputsAdded
    | AddingCollateralInputsFor Value
    | FinishedBalancing CardanoTx
    | SigningTx CardanoTx
    | SubmittingTx CardanoTx
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty TxBalanceMsg where
    pretty = \case
        BalancingUnbalancedTx utx    -> hang 2 $ vsep ["Balancing an unbalanced transaction:", pretty utx]
        NoOutputsAdded               -> "No outputs added"
        AddingPublicKeyOutputFor vl  -> "Adding public key output for" <+> pretty vl
        NoInputsAdded                -> "No inputs added"
        AddingInputsFor vl           -> "Adding inputs for" <+> pretty vl
        NoCollateralInputsAdded      -> "No collateral inputs added"
        AddingCollateralInputsFor vl -> "Adding collateral inputs for" <+> pretty vl
        FinishedBalancing tx         -> hang 2 $ vsep ["Finished balancing:", pretty tx]
        SigningTx tx                 -> "Signing tx:" <+> pretty (getCardanoTxId tx)
        SubmittingTx tx              -> "Submitting tx:" <+> pretty (getCardanoTxId tx)

makePrisms ''TxBalanceMsg

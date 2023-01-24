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
  , _AdjustingUnbalancedTx
  , _BalancingUnbalancedTx
  , _ValidationFailed
  ) where

import Cardano.Api qualified as C
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (Address, CardanoTx, TxId, getCardanoTxId)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Index (ValidationError, ValidationPhase)
import Ledger.Slot (Slot)
import Prettyprinter (Pretty (..), colon, hang, viaShow, vsep, (<+>))
import Wallet.Emulator.Error (WalletAPIError)

data RequestHandlerLogMsg =
    SlotNoticationTargetVsCurrent Slot Slot
    | StartWatchingContractAddresses
    | HandleTxFailed WalletAPIError
    | UtxoAtFailed Address
    | AdjustingUnbalancedTx [C.Lovelace]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makePrisms ''RequestHandlerLogMsg

instance Pretty RequestHandlerLogMsg where
    pretty = \case
        SlotNoticationTargetVsCurrent target current ->
            "target slot:" <+> pretty target <> "; current slot:" <+> pretty current
        StartWatchingContractAddresses -> "Start watching contract addresses"
        HandleTxFailed e -> "handleTx failed:" <+> viaShow e
        UtxoAtFailed addr -> "UtxoAt failed:" <+> pretty addr
        AdjustingUnbalancedTx vl -> "Adjusting an unbalanced transaction:" <+> pretty vl

data TxBalanceMsg =
    BalancingUnbalancedTx UnbalancedTx
    | FinishedBalancing CardanoTx
    | SigningTx CardanoTx
    | SubmittingTx CardanoTx
    | ValidationFailed
        ValidationPhase
        TxId
        CardanoTx
        ValidationError
        C.Value -- ^ The amount of collateral stored in the transaction.
        [Text]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty TxBalanceMsg where
    pretty = \case
        BalancingUnbalancedTx utx    -> hang 2 $ vsep ["Balancing an unbalanced transaction:", pretty utx]
        FinishedBalancing tx         -> hang 2 $ vsep ["Finished balancing:", pretty tx]
        SigningTx tx                 -> "Signing tx:" <+> pretty (getCardanoTxId tx)
        SubmittingTx tx              -> "Submitting tx:" <+> pretty (getCardanoTxId tx)
        ValidationFailed p i _ e _ _ -> "Validation error:" <+> pretty p <+> pretty i <> colon <+> pretty e

makePrisms ''TxBalanceMsg

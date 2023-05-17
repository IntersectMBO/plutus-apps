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
import Cardano.Node.Emulator.LogMessages (TxBalanceMsg (..), _BalancingUnbalancedTx, _ValidationFailed)
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (Address)
import Ledger.Slot (Slot)
import Prettyprinter (Pretty (..), viaShow, (<+>))
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

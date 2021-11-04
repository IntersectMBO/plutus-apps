{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE OverloadedStrings  #-}
module Wallet.Emulator.Error where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter

import Ledger (PubKeyHash, ValidationError)
import Ledger.Tx.CardanoAPI (ToCardanoError)

-- | An error thrown by wallet interactions.
data WalletAPIError =
    InsufficientFunds Text
    -- ^ There were insufficient funds to perform the desired operation.
    | PrivateKeyNotFound PubKeyHash
    -- ^ The private key of this public key hahs is not known to the wallet.
    | ValidationError ValidationError
    -- ^ There was an error during off-chain validation.
    | ToCardanoError ToCardanoError
    -- ^ There was an error while converting to Cardano.API format.
    | OtherError Text
    -- ^ Some other error occurred.
    deriving stock (Show, Eq, Generic)

instance Pretty WalletAPIError where
    pretty = \case
        InsufficientFunds t ->
            "Insufficient funds:" <+> pretty t
        PrivateKeyNotFound pk ->
            "Private key not found:" <+> viaShow pk
        ValidationError e ->
            "Validation error:" <+> pretty e
        ToCardanoError t ->
            "Error during conversion to a Cardano.Api format:" <+> pretty t
        OtherError t ->
            "Other error:" <+> pretty t

instance FromJSON WalletAPIError
instance ToJSON WalletAPIError

throwInsufficientFundsError :: Member (Error WalletAPIError) effs => Text -> Eff effs a
throwInsufficientFundsError = throwError . InsufficientFunds

throwOtherError :: Member (Error WalletAPIError) effs => Text -> Eff effs a
throwOtherError = throwError . OtherError

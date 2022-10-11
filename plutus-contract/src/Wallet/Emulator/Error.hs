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
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

import Ledger (PaymentPubKeyHash, ValidationError, Value)
import Ledger.Constraints qualified as Constraints
import Ledger.Tx.CardanoAPI (ToCardanoError)
import Plutus.V1.Ledger.Ada (Ada)

-- | An error thrown by wallet interactions.
data WalletAPIError =
    InsufficientFunds Text
    -- ^ There were insufficient funds to perform the desired operation.
    | ChangeHasLessThanNAda Value Ada
    -- ^ The change when selecting coins contains less than the minimum amount
    -- of Ada.
    | NoPaymentPubKeyHashError
    -- ^ The wallet doesn't have any payment key hash, which should not be
    -- possible.
    | PaymentPrivateKeyNotFound PaymentPubKeyHash
    -- ^ The private key of this public key hash is not known to the wallet.
    | ValidationError ValidationError
    -- ^ There was an error during off-chain validation.
    | ToCardanoError ToCardanoError
    -- ^ There was an error while converting to Cardano.API format.
    | PaymentMkTxError Constraints.MkTxError
    -- ^ There was an error while creating a payment transaction
    | RemoteClientFunctionNotYetSupported Text
    -- ^ The called wallet effect is not yet supported in a remote wallet client scenario.
    | OtherError Text
    -- ^ Some other error occurred.
    deriving stock (Show, Eq, Generic)

instance Pretty WalletAPIError where
    pretty = \case
        InsufficientFunds t ->
            "Insufficient funds:" <+> pretty t
        ChangeHasLessThanNAda v ada ->
            "Coin change has less than" <+> pretty ada <> ":" <+> pretty v
        NoPaymentPubKeyHashError ->
            "No payment public hash found"
        PaymentPrivateKeyNotFound pk ->
            "Payment private key not found:" <+> viaShow pk
        ValidationError e ->
            "Validation error:" <+> pretty e
        ToCardanoError t ->
            "Error during conversion to a Cardano.Api format:" <+> pretty t
        PaymentMkTxError e ->
            "Payment transaction error:" <+> pretty e
        RemoteClientFunctionNotYetSupported e ->
            "Remote client function not yet supported:" <+> pretty e
        OtherError t ->
            "Other error:" <+> pretty t

instance FromJSON WalletAPIError
instance ToJSON WalletAPIError

throwInsufficientFundsError :: Member (Error WalletAPIError) effs => Text -> Eff effs a
throwInsufficientFundsError = throwError . InsufficientFunds

throwOtherError :: Member (Error WalletAPIError) effs => Text -> Eff effs a
throwOtherError = throwError . OtherError

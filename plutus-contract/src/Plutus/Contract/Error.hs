{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Plutus.Contract.Error
    ( ContractError(..)
    , AsContractError(..)
    , MatchingError(..)
    , AsMatchingError(..)
    , AssertionError(..)
    , AsAssertionError(..)
    ) where

import Control.Lens (prism')
import Control.Lens.TH (makeClassyPrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

import Data.Aeson qualified as JSON
import Ledger.Constraints.OffChain (MkTxError)
import Plutus.Contract.CardanoAPI (ToCardanoError)
import Plutus.Contract.Checkpoint (AsCheckpointError (_CheckpointError), CheckpointError)
import Plutus.Contract.Effects (ChainIndexResponse)
import Wallet.Error (WalletAPIError)
import Wallet.Types (EndpointDescription (EndpointDescription), EndpointValue (EndpointValue))

-- | An error
newtype MatchingError = WrongVariantError { unWrongVariantError :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
makeClassyPrisms ''MatchingError

instance Pretty MatchingError where
  pretty = \case
    WrongVariantError t -> "Wrong variant:" <+> pretty t

-- | An error emitted when an 'Assertion' fails.
newtype AssertionError = GenericAssertion { unAssertionError :: T.Text }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
makeClassyPrisms ''AssertionError

instance Pretty AssertionError where
    pretty = \case
        GenericAssertion t -> "Generic assertion:" <+> pretty t

-- | This lets people use 'T.Text' as their error type.
instance AsAssertionError T.Text where
    _AssertionError = prism' (T.pack . show) (const Nothing)

data ContractError =
    WalletContractError WalletAPIError
  | ChainIndexContractError T.Text ChainIndexResponse
  | EmulatorAssertionContractError AssertionError -- TODO: Why do we need this constructor
  | ConstraintResolutionContractError MkTxError
  | TxToCardanoConvertContractError ToCardanoError
  | ResumableContractError MatchingError
  | CCheckpointContractError CheckpointError
  | EndpointDecodeContractError
      { eeEndpointDescription :: EndpointDescription
      -- ^ The endpoint description which the decoding error occurred from
      , eeEndpointValue       :: EndpointValue JSON.Value
      -- ^ The endpoint value that was used as an endpoint parameter
      , eeErrorMessage        :: T.Text
      -- ^ JSON decoding error message
      }
  | OtherContractError T.Text
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
makeClassyPrisms ''ContractError

instance Pretty ContractError where
  pretty = \case
    WalletContractError e               -> "Wallet error:" <+> pretty e
    ChainIndexContractError expectedResp actualResp
        -> "Wrong response type from chain index request: Expected"
        <+> pretty expectedResp
        <> ", got"
        <+> pretty actualResp
    EmulatorAssertionContractError a    -> "Emulator assertion error:" <+> pretty a
    ConstraintResolutionContractError e -> "Constraint resolution error:" <+> pretty e
    TxToCardanoConvertContractError e   -> "To Cardano transaction conversation error:" <+> pretty e
    ResumableContractError e            -> "Resumable error:" <+> pretty e
    CCheckpointContractError e          -> "Checkpoint error:" <+> pretty e
    EndpointDecodeContractError (EndpointDescription ed) (EndpointValue ev) err
        -> "Failed to decode endpoint \""
        <> pretty ed
        <> "\" with value"
        <+> viaShow ev
        <> ":"
        <+> pretty err
    OtherContractError t                -> "Other error:" <+> pretty t

-- | This lets people use 'T.Text' as their error type.
instance AsContractError T.Text where
    _ContractError = prism' (T.pack . show) (const Nothing)

instance IsString ContractError where
  fromString = OtherContractError . fromString

instance AsAssertionError ContractError where
    _AssertionError = _EmulatorAssertionContractError

instance AsCheckpointError ContractError where
  _CheckpointError = _CCheckpointContractError

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
-- | The Atala Mirror application that initialises the state machine
module Plutus.Contracts.Prism.Mirror(
    MirrorSchema
    , CredentialOwnerReference(..)
    , MirrorError(..)
    , mirror
    ) where

import Control.Lens
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName)
import Plutus.Contract
import Plutus.Contract.StateMachine (AsSMContractError (..), SMContractError, StateMachineTransition (..))
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Contracts.Prism.Credential (Credential (..), CredentialAuthority (..))
import Plutus.Contracts.Prism.Credential qualified as Credential
import Plutus.Contracts.Prism.StateMachine as StateMachine
import Schema (ToSchema)
import Wallet.Emulator (mockWalletPaymentPubKeyHash)
import Wallet.Emulator.Wallet (Wallet)

-- | Reference to a credential tied to a specific owner (public key address).
--   From this, and the public key of the Mirror instance, we can compute the
--   address of the state machine script that locks the token for the owner.
data CredentialOwnerReference =
    CredentialOwnerReference
        { coTokenName :: TokenName
        , coOwner     :: Wallet
        }
    deriving stock (Generic, Eq, Show, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type MirrorSchema =
        Endpoint "issue" CredentialOwnerReference -- lock a single credential token in a state machine tied to the credential token owner
        .\/ Endpoint "revoke" CredentialOwnerReference -- revoke a credential token token from its owner by calling 'Revoke' on the state machine instance

mirror ::
    ( HasEndpoint "revoke" CredentialOwnerReference s
    , HasEndpoint "issue" CredentialOwnerReference s
    )
    => Contract w s MirrorError ()
mirror = do
    logInfo @String "mirror started"
    authority <- mapError SetupError $ CredentialAuthority <$> ownFirstPaymentPubKeyHash
    forever $ do
        logInfo @String "waiting for 'issue' call"
        selectList [createTokens authority, revokeToken authority]

createTokens ::
    ( HasEndpoint "issue" CredentialOwnerReference s
    )
    => CredentialAuthority
    -> Promise w s MirrorError ()
createTokens authority = endpoint @"issue" $ \CredentialOwnerReference{coTokenName, coOwner} -> do
    logInfo @String "Endpoint 'issue' called"
    let pk      = Credential.unCredentialAuthority authority
        lookups = Constraints.mintingPolicy (Credential.policy authority)
                <> Constraints.ownPaymentPubKeyHash pk
        theToken = Credential.token Credential{credAuthority=authority,credName=coTokenName}
        constraints =
            Constraints.mustMintValue theToken
            <> Constraints.mustBeSignedBy pk
            <> Constraints.mustPayToPubKey pk (Ada.lovelaceValueOf 1)   -- Add self-spend to force an input
    _ <- mapError CreateTokenTxError $ do
            mkTxConstraints @Scripts.Any lookups constraints
              >>= adjustUnbalancedTx >>= submitTxConfirmed
    let stateMachine = StateMachine.mkMachineClient authority (mockWalletPaymentPubKeyHash coOwner) coTokenName
    void $ mapError StateMachineError $ SM.runInitialise stateMachine Active theToken

revokeToken ::
    ( HasEndpoint "revoke" CredentialOwnerReference s
    )
    => CredentialAuthority
    -> Promise w s MirrorError ()
revokeToken authority = endpoint @"revoke" $ \CredentialOwnerReference{coTokenName, coOwner} -> do
    let stateMachine = StateMachine.mkMachineClient authority (mockWalletPaymentPubKeyHash coOwner) coTokenName
        lookups = Constraints.mintingPolicy (Credential.policy authority) <>
                  Constraints.ownPaymentPubKeyHash  (Credential.unCredentialAuthority authority)
    t <- mapError StateMachineError $ SM.mkStep stateMachine RevokeCredential
    case t of
        Left{} -> return () -- Ignore invalid transitions
        Right StateMachineTransition{smtConstraints=constraints, smtLookups=lookups'} -> do
            mkTxConstraints (lookups <> lookups') constraints
              >>= adjustUnbalancedTx >>= submitTxConfirmed

---
-- Errors and Logging
---

data MirrorError =
    StateNotFound TokenName PaymentPubKeyHash
    | SetupError ContractError
    | MirrorEndpointError ContractError
    | CreateTokenTxError ContractError
    | StateMachineError SMContractError
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''MirrorError

instance AsSMContractError MirrorError where
    _SMContractError = _StateMachineError

instance AsContractError MirrorError where
    _ContractError = _MirrorEndpointError . _ContractError


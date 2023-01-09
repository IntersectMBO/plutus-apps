{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Minting policy script for credential tokens.
module Plutus.Contracts.Prism.Credential(
    CredentialAuthority(..)
    , Credential(..)
    , policy
    , token
    , tokens
    , tokenAccount
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Plutus.Contracts.TokenAccount (Account (..))
import Plutus.Script.Utils.Typed qualified as Scripts
import Plutus.Script.Utils.V1.Scripts (mintingPolicyHash)
import Plutus.Script.Utils.Value (TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V1.Ledger.Api (ScriptContext (..))
import Plutus.V1.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell
import Schema (ToSchema)

-- | Entity that is authorised to mint credential tokens
newtype CredentialAuthority =
    CredentialAuthority
        { unCredentialAuthority :: PaymentPubKeyHash
        }
    deriving stock (Generic, Haskell.Eq, Haskell.Show, Haskell.Ord)
    deriving anyclass (ToJSON, FromJSON, Hashable, ToSchema)

-- | Named credential issued by a credential authority
data Credential =
    Credential
        { credAuthority :: CredentialAuthority
        , credName      :: TokenName
        }
    deriving stock (Generic, Haskell.Eq, Haskell.Show, Haskell.Ord)
    deriving anyclass (ToJSON, FromJSON, Hashable, ToSchema)

-- | The minting policy script validating the creation of credential tokens
{-# INLINABLE validateMint #-}
validateMint :: CredentialAuthority -> () -> ScriptContext -> Bool
validateMint CredentialAuthority{unCredentialAuthority} _ ScriptContext{scriptContextTxInfo=txinfo} =
    -- the credential authority is allowed to mint or destroy any number of
    -- tokens, so we just need to check the signature
    txinfo `txSignedBy` unPaymentPubKeyHash unCredentialAuthority

policy :: CredentialAuthority -> Scripts.MintingPolicy
policy credential = Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \c -> Scripts.mkUntypedMintingPolicy (validateMint c) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode credential

-- | A single credential of the given name
token :: Credential -> Value
token credential = tokens credential 1

-- | A number of credentials of the given name
tokens :: Credential -> Integer -> Value
tokens Credential{credAuthority, credName} n =
    let sym = Value.mpsSymbol (mintingPolicyHash $ policy credAuthority)
    in Value.singleton sym credName n

-- | The 'Account' that can be spent by presenting the credential
tokenAccount :: Credential -> Account
tokenAccount Credential{credAuthority, credName} =
    let sym = Value.mpsSymbol (mintingPolicyHash $ policy credAuthority)
    in Account $ Value.assetClass sym credName

PlutusTx.makeLift ''CredentialAuthority
PlutusTx.unstableMakeIsData ''CredentialAuthority
PlutusTx.makeLift ''Credential
PlutusTx.unstableMakeIsData ''Credential

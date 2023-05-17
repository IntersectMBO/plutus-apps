{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-- | Plutus implementation of an account that can be unlocked with a token.
--   Whoever owns the token can spend the outputs locked by the contract.
--   (A suitable token can be created with the 'Plutus.Contracts.Currency'
--   contract, or with 'newAccount' in this module)
module Plutus.Contracts.TokenAccount(
  Account(..)
  -- * Contract functionality
  , pay
  , redeem
  , newAccount
  , balance
  , address
  , accountToken
  , payTx
  , redeemTx
  -- * Endpoints
  , TokenAccountSchema
  , HasTokenAccountSchema
  , tokenAccountContract
  -- * Etc.
  , TokenAccount
  , TokenAccountError(..)
  , AsTokenAccountError(..)
  , validatorHash
  , typedValidator
  ) where

import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Control.Lens (makeClassyPrisms, review)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Prettyprinter (Pretty)

import Plutus.Contract (AsContractError (_ContractError), Contract, ContractError, Endpoint, HasEndpoint,
                        adjustUnbalancedTx, endpoint, logInfo, mapError, mkTxConstraints, selectList,
                        submitUnbalancedTx, type (.\/), utxosAt)
import Plutus.Contract.Constraints (ScriptLookups, TxConstraints)
import PlutusTx qualified

import Ledger (CardanoAddress, toPlutusAddress)
import Ledger.Tx (CardanoTx, decoratedTxOutPlutusValue)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts (DatumType, RedeemerType, ValidatorTypes)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (AssetClass, TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as V2

import Prettyprinter.Extras (PrettyShow (PrettyShow))

newtype Account = Account { accountOwner :: AssetClass }
    deriving stock    (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving Pretty via (PrettyShow Account)

data TokenAccount

instance ValidatorTypes TokenAccount where
    type RedeemerType TokenAccount = ()
    type DatumType TokenAccount = ()

type TokenAccountSchema =
        Endpoint "redeem" (Account, CardanoAddress)
        .\/ Endpoint "pay" (Account, Value)
        .\/ Endpoint "new-account" (TokenName, CardanoAddress)

type HasTokenAccountSchema s =
    ( HasEndpoint "redeem" (Account, CardanoAddress) s
    , HasEndpoint "pay" (Account, Value) s
    , HasEndpoint "new-account" (TokenName, CardanoAddress) s
    )

data TokenAccountError =
    TAContractError ContractError
    | TACurrencyError Currency.CurrencyError
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''TokenAccountError

instance AsContractError TokenAccountError where
    _ContractError = _TAContractError

instance Currency.AsCurrencyError TokenAccountError where
    _CurrencyError = _TACurrencyError

-- | 'redeem', 'pay' and 'newAccount' with endpoints.
tokenAccountContract
    :: forall w s e.
       ( HasTokenAccountSchema s
       , AsTokenAccountError e
       )
    => Contract w s e ()
tokenAccountContract = mapError (review _TokenAccountError) (selectList [redeem_, pay_, newAccount_]) where
    redeem_ = endpoint @"redeem" @(Account, CardanoAddress) @w @s $ \(accountOwner, destination) -> do
        void $ redeem destination accountOwner
        tokenAccountContract
    pay_ = endpoint @"pay" @_ @w @s $ \(accountOwner, value) -> do
        void $ pay accountOwner value
        tokenAccountContract
    newAccount_ = endpoint @"new-account" @_ @w @s $ \(tokenName, initialOwner) -> do
        void $ newAccount tokenName initialOwner
        tokenAccountContract

{-# INLINEABLE accountToken #-}
accountToken :: Account -> Value
accountToken (Account currency) = Value.assetClassValue currency 1

{-# INLINEABLE validate #-}
validate :: Account -> () -> () -> V2.ScriptContext -> Bool
validate account _ _ ptx = V2.valueSpent (V2.scriptContextTxInfo ptx) `Value.geq` accountToken account

typedValidator :: Account -> V2.TypedValidator TokenAccount
typedValidator = V2.mkTypedValidatorParam @TokenAccount
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

address :: Account -> CardanoAddress
address = mkValidatorCardanoAddress Params.testnet . Scripts.validatorScript . typedValidator

validatorHash :: Account -> V2.ValidatorHash
validatorHash = V2.validatorHash . typedValidator

-- | A transaction that pays the given value to the account
payTx
    ::
    Value
    -> TxConstraints (Scripts.RedeemerType TokenAccount) (Scripts.DatumType TokenAccount)
payTx = Constraints.mustPayToTheScriptWithDatumInTx ()

-- | Pay some money to the given token account
pay
    :: ( AsTokenAccountError e
       )
    => Account
    -> Value
    -> Contract w s e CardanoTx
pay account vl = do
    let inst = typedValidator account
    logInfo @String
        $ "TokenAccount.pay: Paying "
        <> show vl
        <> " into "
        <> show account
    mapError (review _TAContractError) $
          mkTxConstraints (Constraints.typedValidatorLookups inst) (payTx vl)
      >>= adjustUnbalancedTx >>= submitUnbalancedTx

-- | Create a transaction that spends all outputs belonging to the 'Account'.
redeemTx :: forall w s e.
    ( AsTokenAccountError e
    )
    => Account
    -> CardanoAddress
    -> Contract w s e (TxConstraints () (), ScriptLookups TokenAccount)
redeemTx account addr = mapError (review _TAContractError) $ do
    let inst = typedValidator account
    utxos <- utxosAt (address account)
    let totalVal = foldMap decoratedTxOutPlutusValue utxos
        numInputs = Map.size utxos
    logInfo @String
        $ "TokenAccount.redeemTx: Redeeming "
            <> show numInputs
            <> " outputs with a total value of "
            <> show totalVal
    let constraints = Constraints.spendUtxosFromTheScript utxos ()
                <> Constraints.mustPayToAddress (toPlutusAddress addr) (accountToken account)
        lookups = Constraints.typedValidatorLookups inst
                <> Constraints.unspentOutputs utxos
    pure (constraints, lookups)

-- | Empty the account by spending all outputs belonging to the 'Account'.
redeem
  :: ( AsTokenAccountError e
     )
  => CardanoAddress
  -- ^ Where the token should go after the transaction
  -> Account
  -- ^ The token account
  -> Contract w s e CardanoTx
redeem pk account = mapError (review _TokenAccountError) $ do
    (constraints, lookups) <- redeemTx account pk
    mkTxConstraints lookups constraints >>= adjustUnbalancedTx >>= submitUnbalancedTx

-- | @balance account@ returns the value of all unspent outputs that can be
--   unlocked with @accountToken account@
balance
    :: ( AsTokenAccountError e
       )
    => Account
    -> Contract w s e Value
balance account = mapError (review _TAContractError) $ do
    utxos <- utxosAt (address account)
    pure $ foldMap decoratedTxOutPlutusValue utxos

-- | Create a new token and return its 'Account' information.
newAccount
    :: forall w s e.
    (AsTokenAccountError e)
    => TokenName
    -- ^ Name of the token
    -> CardanoAddress
    -- ^ Address of the token's initial owner
    -> Contract w s e Account
newAccount tokenName addr = mapError (review _TokenAccountError) $ do
    cur <- Currency.mintContract addr [(tokenName, 1)]
    let sym = Currency.currencySymbol cur
    pure $ Account $ Value.assetClass sym tokenName

PlutusTx.makeLift ''Account
PlutusTx.unstableMakeIsData ''Account

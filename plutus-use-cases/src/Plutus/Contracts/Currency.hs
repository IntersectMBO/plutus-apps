{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements a custom currency with a minting policy that allows
--   the minting of a fixed amount of units.
module Plutus.Contracts.Currency(
      OneShotCurrency(..)
    , CurrencySchema
    , CurrencyError(..)
    , AsCurrencyError(..)
    , curPolicy
    -- * Actions etc
    , mintContract
    , mintedValue
    , currencySymbol
    , currencyPolicyId
    -- * Simple minting policy currency
    , SimpleMPS(..)
    , mintCurrency
    ) where

import Control.Lens
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup (Last (..))
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (Monoid (..), Semigroup (..))

import Ledger (CardanoAddress, TxOutRef (..), getCardanoTxId)
import Ledger.Scripts
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as CValue
import Plutus.Contract as Contract
import Plutus.Contract.Request (getUnspentOutput)
import Plutus.Script.Utils.V2.Scripts qualified as V2
import Plutus.Script.Utils.Value (CurrencySymbol, TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2

import Prelude (Semigroup (..))
import Prelude qualified as Haskell

{- HLINT ignore "Use uncurry" -}

-- | A currency that can be created exactly once
data OneShotCurrency = OneShotCurrency
  { curRefTransactionOutput :: (V2.TxId, Integer)
  -- ^ Transaction input that must be spent when
  --   the currency is minted.
  , curAmounts              :: AssocMap.Map TokenName Integer
  -- ^ How many units of each 'TokenName' are to
  --   be minted.
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OneShotCurrency

currencyValue :: CurrencySymbol -> OneShotCurrency -> Value
currencyValue s OneShotCurrency{curAmounts = amts} =
    let
        values = map (\(tn, i) -> Value.singleton s tn i) (AssocMap.toList amts)
    in fold values

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
mkCurrency (TxOutRef h i) amts =
    OneShotCurrency
        { curRefTransactionOutput = (h, i)
        , curAmounts              = AssocMap.fromList amts
        }

checkPolicy :: OneShotCurrency -> () -> V2.ScriptContext -> Bool
checkPolicy c@(OneShotCurrency (refHash, refIdx) _) _ ctx@V2.ScriptContext{V2.scriptContextTxInfo=txinfo} =
    let
        -- see note [Obtaining the currency symbol]
        ownSymbol = V2.ownCurrencySymbol ctx

        minted = V2.txInfoMint txinfo
        expected = currencyValue ownSymbol c

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = V2.spendsOutput txinfo refHash refIdx
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent

curPolicy :: OneShotCurrency -> MintingPolicy
curPolicy cur = V2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \c -> Scripts.mkUntypedMintingPolicy (checkPolicy c) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode cur

{- note [Obtaining the currency symbol]

The currency symbol is the address (hash) of the validator. That is why
we can use 'Ledger.scriptAddress' here to get the symbol  in off-chain code,
for example in 'mintedValue'.

Inside the validator script (on-chain) we can't use 'Ledger.scriptAddress',
because at that point we don't know the hash of the script yet. That
is why we use 'V2.ownCurrencySymbol', which obtains the hash from the
'PolicyCtx' value.

-}

-- | The 'Value' minted by the 'OneShotCurrency' contract
mintedValue :: OneShotCurrency -> Value
mintedValue cur = currencyValue (currencySymbol cur) cur

currencySymbol :: OneShotCurrency -> CurrencySymbol
currencySymbol = V2.scriptCurrencySymbol . curPolicy

currencyPolicyId :: OneShotCurrency -> CValue.PolicyId
currencyPolicyId = CValue.policyId . (`Versioned` PlutusV2) . curPolicy

newtype CurrencyError =
    CurContractError ContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CurrencyError

instance AsContractError CurrencyError where
    _ContractError = _CurContractError

-- | @mint [(n1, c1), ..., (n_k, c_k)]@ creates a new currency with
--   @k@ token names, minting @c_i@ units of each token @n_i@.
--   If @k == 0@ then no value is minted. A one-shot minting policy
--   script is used to ensure that no more units of the currency can
--   be minted afterwards.
mintContract
    :: forall w s e.
    ( AsCurrencyError e
    )
    => CardanoAddress
    -> [(TokenName, Integer)]
    -> Contract w s e OneShotCurrency
mintContract addr amounts = mapError (review _CurrencyError) $ do
    txOutRef <- getUnspentOutput
    utxos <- utxosAt addr
    let theCurrency = mkCurrency txOutRef amounts
        curVali     = curPolicy theCurrency
        lookups     = Constraints.plutusV2MintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (mintedValue theCurrency)
    tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    void $ awaitTxConfirmed (getCardanoTxId tx)
    pure theCurrency

-- | Minting policy for a currency that has a fixed amount of tokens issued
--   in one transaction
data SimpleMPS =
    SimpleMPS
        { tokenName :: TokenName
        , amount    :: Integer
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (FromJSON, ToJSON)

type CurrencySchema =
        Endpoint "Create native token" SimpleMPS

-- | Use 'mintContract' to create the currency specified by a 'SimpleMPS'
mintCurrency
    :: Promise (Maybe (Last OneShotCurrency)) CurrencySchema CurrencyError OneShotCurrency
mintCurrency = endpoint @"Create native token" $ \SimpleMPS{tokenName, amount} -> do
    ownAddr <- ownAddress
    cur <- mintContract ownAddr [(tokenName, amount)]
    tell $ Just $ Last cur
    pure cur

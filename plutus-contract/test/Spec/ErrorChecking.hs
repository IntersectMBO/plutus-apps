{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Spec.ErrorChecking where

import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Freer.Extras.Log
import Data.Data
import Data.Row
import Test.Tasty

import Ledger.Ada qualified as Ada
import Ledger.Address
import Ledger.Constraints
import Ledger.Scripts
import Ledger.Tx
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Plutus.Contract as Contract
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel
import Plutus.Script.Utils.V1.Scripts (validatorHash)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators hiding (validatorHash)
import Plutus.Trace.Emulator as Trace
import Plutus.V1.Ledger.Contexts (ScriptContext (..))
import PlutusTx qualified
import PlutusTx.ErrorCodes
import PlutusTx.IsData.Class
import PlutusTx.Prelude hiding ((<$))

import Prelude qualified as Haskell

import Test.QuickCheck hiding (Success)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Success)

{- HLINT ignore "Use camelCase" -}

tests :: TestTree
tests = testGroup "error checking"
  [ testProperty "Normal failures allowed" $ withMaxSuccess 1 prop_FailFalse
  , testProperty "Failure due to head [] not allowed" $ withMaxSuccess 1 $ expectFailure prop_FailHeadNil
  , testProperty "Division by zero not allowed" $ withMaxSuccess 1 $ expectFailure prop_DivZero
  , testProperty "Can't trick division by zero check using trace" $ withMaxSuccess 1 $ expectFailure prop_DivZero_t
  , testProperty "Normal success allowed" $ withMaxSuccess 1 prop_Success
  , testCase "Check defaultWhitelist is ok" $ assertBool "whitelistOk defaultWhitelist" $ whitelistOk defaultWhitelist ]

-- | Normal failures should be allowed
prop_FailFalse :: Property
prop_FailFalse = checkErrorWhitelistWithOptions checkOptions defaultCoverageOptions defaultWhitelist (actionsFromList [FailFalse])

-- | Head Nil failure should not be allowed
prop_FailHeadNil :: Property
prop_FailHeadNil = checkErrorWhitelistWithOptions checkOptions defaultCoverageOptions defaultWhitelist (actionsFromList [FailHeadNil])

-- | Division by zero failure should not be allowed
prop_DivZero :: Property
prop_DivZero = checkErrorWhitelistWithOptions checkOptions defaultCoverageOptions defaultWhitelist (actionsFromList [DivZero])

-- | Division by zero failure should not be allowed (tracing before the failure).
prop_DivZero_t :: Property
prop_DivZero_t = checkErrorWhitelistWithOptions checkOptions defaultCoverageOptions defaultWhitelist (actionsFromList [DivZero_t])

-- | Successful validation should be allowed
prop_Success :: Property
prop_Success = checkErrorWhitelistWithOptions checkOptions defaultCoverageOptions defaultWhitelist (actionsFromList [Success])

checkOptions :: CheckOptions
checkOptions = set minLogLevel Critical defaultCheckOptionsContractModel

-- | This QuickCheck model only provides an interface to the validators used in this
-- test that are convenient for testing them in isolation.
data DummyModel = DummyModel deriving (Haskell.Show, Data)

deriving instance Haskell.Eq (ContractInstanceKey DummyModel w schema err param)
deriving instance Haskell.Show (ContractInstanceKey DummyModel w schema err param)

instance ContractModel DummyModel where
  data ContractInstanceKey DummyModel w schema err param where
    WalletKey :: Wallet -> ContractInstanceKey DummyModel () Schema ContractError ()

  data Action DummyModel = FailFalse
                         | FailHeadNil
                         | DivZero
                         | DivZero_t    -- Trace before dividing by zero
                         | Success
                         deriving (Haskell.Eq, Haskell.Show, Data)

  perform handle _ _ cmd = void $ case cmd of
    FailFalse -> do
      callEndpoint @"failFalse" (handle $ WalletKey w1) ()
      Trace.waitNSlots 2
    FailHeadNil -> do
      callEndpoint @"failHeadNil" (handle $ WalletKey w1) ()
      Trace.waitNSlots 2
    DivZero -> do
      callEndpoint @"divZero" (handle $ WalletKey w1) ()
      Trace.waitNSlots 2
    DivZero_t -> do
      callEndpoint @"divZero_t" (handle $ WalletKey w1) ()
      Trace.waitNSlots 2
    Success -> do
      callEndpoint @"success" (handle $ WalletKey w1) ()
      Trace.waitNSlots 2

  initialState = DummyModel

  initialInstances = [StartContract (WalletKey w1) ()]

  instanceWallet (WalletKey w) = w

  instanceContract _ (WalletKey _) _ = contract

  nextState _ = wait 2

  arbitraryAction _ = elements [FailFalse, FailHeadNil, DivZero, DivZero_t, Success]

data Validators
instance Scripts.ValidatorTypes Validators where
    type instance RedeemerType Validators = Integer
    type instance DatumType Validators = ()

type Schema = Endpoint "failFalse" ()
            .\/ Endpoint "failHeadNil" ()
            .\/ Endpoint "divZero" ()
            .\/ Endpoint "divZero_t" ()
            .\/ Endpoint "success" ()

-- | For each endpoint in the schema: pay to the corresponding validator
-- and then spend that UTxO
contract :: Contract () Schema ContractError ()
contract = selectList [failFalseC, failHeadNilC, divZeroC, divZeroTraceC, successC]
  where
    run validator = void $ do
      let addr = scriptAddress (validatorScript validator)
          hash = validatorHash (validatorScript validator)
          tx = mustPayToOtherScript hash (Datum $ toBuiltinData ()) (Ada.adaValueOf 10)
      r <- submitTx tx
      awaitTxConfirmed (getCardanoTxId r)
      utxos <- utxosAt addr
      let tx' = collectFromScript utxos 0
      submitTxConstraintsSpending validator utxos tx'

    failFalseC = endpoint @"failFalse" $ \ _ -> do
      run v_failFalse

    failHeadNilC = endpoint @"failHeadNil" $ \ _ -> do
      run v_failHeadNil

    divZeroC = endpoint @"divZero" $ \ _ -> do
      run v_divZero

    divZeroTraceC = endpoint @"divZero_t" $ \ _ -> do
      run v_divZero_t

    successC = endpoint @"success" $ \ _ -> do
      run v_success

-- | Always fail "benignly"
{-# INLINEABLE failFalse #-}
failFalse :: () -> Integer -> ScriptContext -> Bool
failFalse _ _ _ = False

v_failFalse :: Scripts.TypedValidator Validators
v_failFalse = Scripts.mkTypedValidator @Validators
    $$(PlutusTx.compile [|| failFalse ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Always fail due to a partial function
{-# INLINEABLE failHeadNil #-}
failHeadNil :: () -> Integer -> ScriptContext -> Bool
failHeadNil _ _ _ = head []

v_failHeadNil :: Scripts.TypedValidator Validators
v_failHeadNil = Scripts.mkTypedValidator @Validators
    $$(PlutusTx.compile [|| failHeadNil ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Always fail with a division by zero error
{-# INLINEABLE divZero #-}
divZero :: () -> Integer -> ScriptContext -> Bool
divZero _ _ _ = (10 `divide` 0) > 5

v_divZero :: Scripts.TypedValidator Validators
v_divZero = Scripts.mkTypedValidator @Validators
    $$(PlutusTx.compile [|| divZero ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

{-# INLINEABLE divZero_t #-}
divZero_t :: () -> Integer -> ScriptContext -> Bool
divZero_t _ _ _ = trace checkHasFailedError False || (10 `divide` 0) > 5
                  -- Trying to cheat by tracing a whitelisted error before failing.
                  -- Currently this tricks the whitelist check.

v_divZero_t :: Scripts.TypedValidator Validators
v_divZero_t = Scripts.mkTypedValidator @Validators
    $$(PlutusTx.compile [|| divZero_t ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Always succeed
{-# INLINEABLE success #-}
success :: () -> Integer -> ScriptContext -> Bool
success _ _ _ = True

v_success :: Scripts.TypedValidator Validators
v_success = Scripts.mkTypedValidator @Validators
    $$(PlutusTx.compile [|| success ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

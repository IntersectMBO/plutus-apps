{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.TimeValidity(tests) where

import Cardano.Api.Shelley (protocolParamProtocolVersion)
import Control.Lens hiding (contains, from, (.>))
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, emulatorConfig, w1)
import Plutus.Script.Utils.V1.Scripts (ValidatorHash)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (POSIXTime, TxInfo, Validator)
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum, unitRedeemer)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Wallet.Emulator.Stream (params)

tests :: TestTree
tests =
    testGroup "time validity"
        [ protocolV5
        , protocolV6
        , defaultProtocolParams
        ]

contract :: Contract () Empty ContractError ()
contract = do
    now <- Con.currentTime
    logInfo @String $ "now: " ++ show now
    let lookups1 = Constraints.typedValidatorLookups $ typedValidator deadline
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25000000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups2 =
            Constraints.plutusV1OtherScript (validatorScript deadline)
            <> Constraints.unspentOutputs utxos
        tx2 =
            foldMap (\oref -> Constraints.mustSpendScriptOutput oref unitRedeemer) orefs
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustValidateIn (from $ now + 1000)
    ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Trace.EmulatorTrace ()
trace = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 3

protocolV5 :: TestTree
protocolV5 = checkPredicateOptions
    (defaultCheckOptions & over (emulatorConfig . params . Ledger.protocolParamsL) (\pp -> pp { protocolParamProtocolVersion = (5, 0) }))
    "tx valid time interval is not supported in protocol v5"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Invalid range":_) _) -> True; _ -> False  }))
    (void trace)

protocolV6 :: TestTree
protocolV6 = checkPredicateOptions
    (defaultCheckOptions & over (emulatorConfig . params . Ledger.protocolParamsL) (\pp -> pp { protocolParamProtocolVersion = (6, 0) }))
    "tx valid time interval is supported in protocol v6"
    (assertValidatedTransactionCount 2)
    (void trace)

defaultProtocolParams :: TestTree
defaultProtocolParams = checkPredicateOptions
    defaultCheckOptions
    "tx valid time interval is supported in protocol v6+"
    (assertValidatedTransactionCount 2)
    (void trace)

deadline :: POSIXTime
deadline = 1596059092000 -- (milliseconds) transaction's valid range must be after this

{-# INLINEABLE mkValidator #-}
mkValidator :: P.POSIXTime -> () -> () -> P.ScriptContext -> Bool
mkValidator dl _ _ ctx = (from dl `contains` range) || P.traceError "Invalid range"
    where
    info :: TxInfo
    info = P.scriptContextTxInfo ctx

    range :: P.POSIXTimeRange
    range = P.txInfoValidRange info


data UnitTest
instance Scripts.ValidatorTypes UnitTest

typedValidator :: POSIXTime -> Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidatorParam @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: P.POSIXTime -> Validator
validatorScript = Scripts.validatorScript . typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash $ typedValidator deadline

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash

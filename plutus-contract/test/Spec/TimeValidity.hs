{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TimeValidity(tests) where

import Cardano.Api.Shelley (ProtocolParameters, protocolParamProtocolVersion)
import Control.Lens hiding (contains, now, to, (.>))
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract as Con
import Plutus.Contract.Logging (logInfo)
import Plutus.Contract.Test (assertAccumState, assertValidatedTransactionCount, changeInitialWalletValue,
                             checkPredicate, checkPredicateOptions, defaultCheckOptions, emulatorConfig, w1, w2)
import Plutus.Script.Utils.V1.Generators (someTokenValue)
import Plutus.Script.Utils.V1.Scripts (ValidatorHash, validatorHash)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Address, POSIXTime, POSIXTimeRange, ScriptContext, TxInfo, Validator)
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Interval (contains, to)
import Plutus.V1.Ledger.Scripts (Datum (Datum), unitDatum, unitRedeemer)
import PlutusTx qualified
import Prelude hiding (not)
import Wallet.Emulator qualified as EM
import Wallet.Emulator.Stream (params)

tests :: TestTree
tests =
    testGroup "time validity"
        [ protocolV5,
          defaultProtocolParams
        ]

protocolV5 :: TestTree
protocolV5 =
    let contract :: Contract () Empty ContractError ()
        contract = do
            now <- Con.currentTime
            logInfo @String $ "now: " ++ show now
            let tx1 = Constraints.mustPayToOtherScript valHash unitDatum $ Ada.lovelaceValueOf 25000000
            ledgerTx1 <- submitTx tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
            utxos <- utxosAt scrAddress
            let orefs = fst <$> Map.toList utxos
                lookups =
                    Constraints.otherScript (validatorScript deadline)
                    <> Constraints.unspentOutputs utxos
                tx2 =
                    mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
                    <> Constraints.mustIncludeDatum unitDatum
                    <> Constraints.mustValidateIn (to $ now + 2000)
            ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

        trace = do
                void $ Trace.activateContractWallet w1 contract
                void $ Trace.waitNSlots 3

    in checkPredicateOptions (defaultCheckOptions & over (emulatorConfig . params . Ledger.protocolParamsL) (\pp -> pp { protocolParamProtocolVersion = (5, 0) })) "tx valid time interval is not supported in protocol v5" (assertValidatedTransactionCount 1) (void trace)

defaultProtocolParams :: TestTree
defaultProtocolParams =
    let options = defaultCheckOptions

        contract :: Contract () Empty ContractError ()
        contract = do
            now <- Con.currentTime
            let tx1 = Constraints.mustPayToOtherScript valHash unitDatum $ Ada.lovelaceValueOf 25000000
            ledgerTx1 <- submitTx tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
            utxos <- utxosAt scrAddress
            let orefs = fst <$> Map.toList utxos
                lookups =
                    Constraints.otherScript (validatorScript deadline)
                    <> Constraints.unspentOutputs utxos
                tx2 =
                    mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
                    <> Constraints.mustIncludeDatum unitDatum
                    <> Constraints.mustValidateIn (to $ now + 2000)
            ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

        trace = do
                void $ Trace.activateContractWallet w1 contract
                void $ Trace.waitNSlots 3

    in checkPredicateOptions defaultCheckOptions "tx valid time interval is supported in protocol v6+" (assertValidatedTransactionCount 2) (void trace)

deadline :: POSIXTime
deadline = 1596059095000 -- (milliseconds) transaction's valid range must be before this

{-# INLINEABLE mkValidator #-}
mkValidator :: POSIXTime -> () -> () -> ScriptContext -> Bool
mkValidator dl _ _ ctx = to dl `contains` range
    where
    info :: TxInfo
    info = P.scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = P.txInfoValidRange info

instance Scripts.ValidatorTypes POSIXTime where
    type instance RedeemerType POSIXTime = ()
    type instance DatumType POSIXTime = ()

typedValidator :: POSIXTime -> Scripts.TypedValidator POSIXTime
typedValidator = Scripts.mkTypedValidatorParam @POSIXTime
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: POSIXTime -> Validator
validatorScript = Scripts.validatorScript . typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash $ typedValidator deadline

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash

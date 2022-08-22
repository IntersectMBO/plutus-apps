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
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, emulatorConfig, w1)
import Plutus.Script.Utils.V1.Scripts (ValidatorHash)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (POSIXTime, TxInfo, Validator)
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V1.Ledger.Interval qualified as I
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum, unitRedeemer)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Wallet.Emulator.Stream (params)

tests :: TestTree
tests = testGroup "time validitity constraint"
    [ testGroup "with Ledger constraints"
        [ protocolV5
        , protocolV6
        , defaultProtocolParams
        ]
    , testGroup "with Tx.Constraints"
        [ protocolV5Cardano
        , protocolV6Cardano
        , defaultProtocolParamsValidCardano
        , defaultProtocolParamsInvalidCardano
        ]
    ]

contract :: Contract () Empty ContractError ()
contract = do
    now <- Con.currentTime
    logInfo @String $ "now: " ++ show now
    let lookups1 = Constraints.plutusV1TypedValidatorLookups $ typedValidator deadline
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
    cSlot <- Con.currentPABSlot
    logInfo @String $ "Current slot: " ++ show cSlot

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

traceCardano :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
traceCardano c = do
    void $ Trace.activateContractWallet w1 c
    void $ Trace.waitNSlots 4

validContractCardano :: Ledger.Params -> Contract () Empty ContractError ()
validContractCardano p = do
    let mkTx lookups constraints = either (error . show) id $ Tx.Constraints.mkTx @UnitTest p lookups constraints
    pkh <- Con.ownFirstPaymentPubKeyHash
    utxos <- Con.ownUtxos
    now <- Con.currentTime
    logInfo @String $ "now: " ++ show now
    let utxoRef = fst $ head' $ Map.toList utxos
        lookups = Tx.Constraints.unspentOutputs utxos
        tx  =  Tx.Constraints.mustPayToPubKey pkh (Ada.toValue Ledger.minAdaTxOut)
            <> Tx.Constraints.mustSpendPubKeyOutput utxoRef
            <> Tx.Constraints.mustValidateIn (from $ 1000 + now)
    ledgerTx <- submitUnbalancedTx $ mkTx lookups tx
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    cSlot <- Con.currentPABSlot
    logInfo @String $ "Current slot: " ++ show cSlot
    let txRange = Tx.getCardanoTxValidityRange ledgerTx
    logInfo @String $ show txRange

    P.unless (cSlot `I.member` txRange) $ P.traceError "InvalidRange"


invalidContractCardano :: Ledger.Params -> Contract () Empty ContractError ()
invalidContractCardano p = do
    let mkTx lookups constraints = either (error . show) id $ Tx.Constraints.mkTx @UnitTest p lookups constraints
    pkh <- Con.ownFirstPaymentPubKeyHash
    utxos <- Con.ownUtxos
    now <- Con.currentTime
    logInfo @String $ "now: " ++ show now
    let (utxoRef1, utxoRef2) = get2 $ map fst $ Map.toList utxos
        lookups = Tx.Constraints.unspentOutputs utxos
        tx1 =  Tx.Constraints.mustPayToPubKey pkh (Ada.toValue Ledger.minAdaTxOut)
            <> Tx.Constraints.mustSpendPubKeyOutput utxoRef1
        tx2 =  Tx.Constraints.mustPayToPubKey pkh (Ada.toValue Ledger.minAdaTxOut)
            <> Tx.Constraints.mustSpendPubKeyOutput utxoRef2
            <> Tx.Constraints.mustValidateIn (I.to now)
    ledgerTx1 <- submitUnbalancedTx $ mkTx lookups tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
    ledgerTx2 <- submitUnbalancedTx $ mkTx lookups tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    cSlot <- Con.currentPABSlot
    logInfo @String $ "Current slot: " ++ show cSlot
    let txRange = Tx.getCardanoTxValidityRange ledgerTx1
    logInfo @String $ show txRange

    P.unless (cSlot `I.member` txRange) $ P.traceError "InvalidRange"

protocolV5Cardano :: TestTree
protocolV5Cardano =
    let checkOptions = defaultCheckOptions & over (emulatorConfig . params . Ledger.protocolParamsL) (\pp -> pp { protocolParamProtocolVersion = (5, 0) })
    in checkPredicateOptions
    checkOptions
    "tx valid time interval is not supported in protocol v5"
    (assertFailedTransaction (\_ _ _ -> True))
    (void $ traceCardano $ validContractCardano $ view (emulatorConfig . params) checkOptions)

protocolV6Cardano :: TestTree
protocolV6Cardano =
    let checkOptions = defaultCheckOptions & over (emulatorConfig . params . Ledger.protocolParamsL) (\pp -> pp { protocolParamProtocolVersion = (6, 0) })
    in checkPredicateOptions
    checkOptions
    "tx valid time interval is supported in protocol v6"
    (assertValidatedTransactionCount 1)
    (void $ traceCardano $ validContractCardano $ view (emulatorConfig . params) checkOptions)

defaultProtocolParamsValidCardano :: TestTree
defaultProtocolParamsValidCardano = checkPredicateOptions
    defaultCheckOptions
    "tx valid time interval is supported in protocol v6+"
    (assertValidatedTransactionCount 1)
    (void $ traceCardano $ validContractCardano $ view (emulatorConfig . params) defaultCheckOptions)

-- We only test here if the contract ends with no valid transaction.
-- As the range is unreachable, the transaction should fail though (and it's unfortunately not the case).
defaultProtocolParamsInvalidCardano :: TestTree
defaultProtocolParamsInvalidCardano = checkPredicateOptions
    defaultCheckOptions
    "tx valid time interval in the past make transactions fail"
    (assertValidatedTransactionCount 1)
    (void $ traceCardano $ invalidContractCardano $ view (emulatorConfig . params) defaultCheckOptions)

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

head' :: [a] -> a
head' (x:_) = x
head' _     = error "Spec.TxConstraints.TimeValidity: Not enough inputs"

get2 :: [a] -> (a, a)
get2 (x:y:_) = (x,y)
get2 _       = error "Spec.TxConstraints.TimeValidity: Not enough inputs"

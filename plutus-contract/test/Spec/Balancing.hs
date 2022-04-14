{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Spec.Balancing(tests) where

import Control.Lens hiding ((.>))
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Plutus.Contract as Con
import Plutus.Contract.Test (assertAccumState, assertValidatedTransactionCount, changeInitialWalletValue,
                             checkPredicate, checkPredicateOptions, defaultCheckOptions, w1, w2)
import Plutus.Script.Utils.V1.Generators (someTokenValue)
import Plutus.Script.Utils.V1.Scripts (mintingPolicyHash, scriptCurrencySymbol, validatorHash)
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Address, Validator)
import Plutus.V1.Ledger.Scripts (Datum (Datum), unitDatum, unitRedeemer)
import Plutus.V1.Ledger.Scripts qualified as Ledger
import PlutusTx qualified
import Prelude hiding (not)
import Wallet.Emulator qualified as EM

tests :: TestTree
tests =
    testGroup "balancing"
        [ balanceTxnMinAda
        , balanceTxnMinAda2
        , balanceTxnNoExtraOutput
        ]

balanceTxnMinAda :: TestTree
balanceTxnMinAda =
    let ee = someTokenValue "ee" 1
        ff = someTokenValue "ff" 1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (Value.scale 1000 (ee <> ff) <>)
        vHash = validatorHash someValidator

        contract :: Contract () EmptySchema ContractError ()
        contract = do
            let constraints1 = Constraints.mustPayToOtherScript vHash unitDatum (Value.scale 100 ff <> Ada.toValue Ledger.minAdaTxOut)
                utx1 = either (error . show) id $ Constraints.mkTx @Void mempty constraints1
            submitTxConfirmed utx1
            utxo <- utxosAt someAddress
            let txOutRef = head (Map.keys utxo)
                constraints2 = Constraints.mustSpendScriptOutput txOutRef unitRedeemer
                    <> Constraints.mustPayToOtherScript vHash unitDatum (Value.scale 200 ee)
                lookups2 = Constraints.unspentOutputs utxo <> Constraints.otherScript someValidator
            utx2 <- Con.adjustUnbalancedTx $ either (error . show) id $ Constraints.mkTx @Void lookups2 constraints2
            submitTxConfirmed utx2

        trace = do
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 2

    in checkPredicateOptions options "balancing doesn't create outputs with no Ada" (assertValidatedTransactionCount 2) (void trace)

balanceTxnMinAda2 :: TestTree
balanceTxnMinAda2 =
    let vA n = someTokenValue "A" n
        vB n = someTokenValue "B" n
        mps  = MPS.mkForwardingMintingPolicy vHash
        vL n = Value.singleton (Value.mpsSymbol $ mintingPolicyHash mps) "L" n
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (<> vA 1 <> vB 2)
        vHash = validatorHash someValidator
        payToWallet w = Constraints.mustPayToPubKey (EM.mockWalletPaymentPubKeyHash w)
        mkTx lookups constraints = Con.adjustUnbalancedTx . either (error . show) id $ Constraints.mkTx @Void lookups constraints

        setupContract :: Contract () EmptySchema ContractError ()
        setupContract = do
            -- Make sure there is a utxo with 1 A, 1 B, and 4 ada at w2
            submitTxConfirmed =<< mkTx mempty (payToWallet w2 (vA 1 <> vB 1 <> Value.scale 2 (Ada.toValue Ledger.minAdaTxOut)))
            -- Make sure there is a UTxO with 1 B and datum () at the script
            submitTxConfirmed =<< mkTx mempty (Constraints.mustPayToOtherScript vHash unitDatum (vB 1))
            -- utxo0 @ wallet2 = 1 A, 1 B, 4 Ada
            -- utxo1 @ script  = 1 B, 2 Ada

        wallet2Contract :: Contract () EmptySchema ContractError ()
        wallet2Contract = do
            utxos <- utxosAt someAddress
            let txOutRef = head (Map.keys utxos)
                lookups = Constraints.unspentOutputs utxos
                        <> Constraints.otherScript someValidator
                        <> Constraints.mintingPolicy mps
                constraints = Constraints.mustSpendScriptOutput txOutRef unitRedeemer                                        -- spend utxo1
                            <> Constraints.mustPayToOtherScript vHash unitDatum (vB 1)                                       -- 2 ada and 1 B to script
                            <> Constraints.mustPayToOtherScript vHash (Datum $ PlutusTx.toBuiltinData (0 :: Integer)) (vB 1) -- 2 ada and 1 B to script (different datum)
                            <> Constraints.mustMintValue (vL 1) -- 1 L and 2 ada to wallet2
            submitTxConfirmed =<< mkTx lookups constraints

        trace = do
            void $ Trace.activateContractWallet w1 setupContract
            void $ Trace.waitNSlots 10
            void $ Trace.activateContractWallet w2 wallet2Contract
            void $ Trace.waitNSlots 10

    in checkPredicateOptions options "balancing doesn't create outputs with no Ada (2)" (assertValidatedTransactionCount 3) (void trace)

balanceTxnNoExtraOutput :: TestTree
balanceTxnNoExtraOutput =
    let vL n = Value.singleton (scriptCurrencySymbol coinMintingPolicy) "coinToken" n
        mkTx lookups constraints = either (error . show) id $ Constraints.mkTx @Void lookups constraints

        mintingOperation :: Contract [Int] EmptySchema ContractError ()
        mintingOperation = do
            pkh <- Con.ownPaymentPubKeyHash

            let val = vL 200
                lookups = Constraints.mintingPolicy coinMintingPolicy
                constraints = Constraints.mustMintValue val
                    <> Constraints.mustPayToPubKey pkh (val <> Ada.toValue Ledger.minAdaTxOut)

            tx <- submitUnbalancedTx $ mkTx lookups constraints
            tell [length $ Ledger.getCardanoTxOutRefs tx]

        trace = do
            void $ Trace.activateContract w1 mintingOperation "instance 1"
            void $ Trace.waitNSlots 2
        tracePred = assertAccumState mintingOperation "instance 1" (== [2]) "has 2 outputs"

    in checkPredicate "balancing doesn't create extra output" tracePred (void trace)

someAddress :: Address
someAddress = Ledger.scriptAddress someValidator

someValidator :: Validator
someValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

coinMintingPolicy :: Ledger.MintingPolicy
coinMintingPolicy = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [|| MPS.mkUntypedMintingPolicy mkPolicy ||])

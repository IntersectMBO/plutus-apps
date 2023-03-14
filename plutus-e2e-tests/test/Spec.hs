{-# LANGUAGE OverloadedStrings #-}

{- | Tests covering the integration of local modules and external packages. To name a few:
    Trace Emulator, plutus-tx-constraints, Contract.Test library,
    plutus-tx and plutus-ledger-api.
    Scenarios aim to use a variety of functions and assert relevant properties.
    Can also be considered living documentation for Contract and Tx Constraint use.
-}
module Main(main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Extras qualified as HE
import Helpers.Test (TestParams (TestParams), runTest, runTestWithPosixTime)
import Helpers.Testnet qualified as TN
import Helpers.Utils qualified as U
import Spec.AlonzoFeatures qualified as AlonzoFeatures
import Spec.BabbageFeatures qualified as BabbageFeatures
import Spec.Builtins.SECP256k1 qualified as Builtins
import Test.Base qualified as H
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

pv6Tests :: H.Property
pv6Tests = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsAlonzo6
    preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    runTestWithPosixTime "checkTxInfoV1Test" AlonzoFeatures.checkTxInfoV1Test options testParams preTestnetTime
    runTest "datumHashSpendTest" AlonzoFeatures.datumHashSpendTest options testParams
    runTest "mintBurnTest" AlonzoFeatures.mintBurnTest options testParams
    runTest "collateralContainsTokenErrorTest" AlonzoFeatures.collateralContainsTokenErrorTest options testParams
    runTest "missingCollateralInputErrorTest" AlonzoFeatures.missingCollateralInputErrorTest options testParams
    runTest "tooManyCollateralInputsErrorTest" AlonzoFeatures.tooManyCollateralInputsErrorTest options testParams
    runTest "verifySchnorrAndEcdsaTest" Builtins.verifySchnorrAndEcdsaTest options testParams

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

pv7Tests :: H.Property
pv7Tests = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsBabbage7
    preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    runTestWithPosixTime "checkTxInfoV1Test" AlonzoFeatures.checkTxInfoV1Test options testParams preTestnetTime
    runTestWithPosixTime "checkTxInfoV2Test" BabbageFeatures.checkTxInfoV2Test options testParams preTestnetTime
    runTest "datumHashSpendTest" AlonzoFeatures.datumHashSpendTest options testParams
    runTest "mintBurnTest" AlonzoFeatures.mintBurnTest options testParams
    runTest "collateralContainsTokenErrorTest" AlonzoFeatures.collateralContainsTokenErrorTest options testParams
    runTest "missingCollateralInputErrorTest" AlonzoFeatures.missingCollateralInputErrorTest options testParams
    runTest "tooManyCollateralInputsErrorTest" AlonzoFeatures.tooManyCollateralInputsErrorTest options testParams
    runTest "verifySchnorrAndEcdsaTest" Builtins.verifySchnorrAndEcdsaTest options testParams
    runTest "referenceScriptMintTest" BabbageFeatures.referenceScriptMintTest options testParams
    runTest "referenceScriptInlineDatumSpendTest" BabbageFeatures.referenceScriptInlineDatumSpendTest options testParams
    runTest "referenceScriptDatumHashSpendTest" BabbageFeatures.referenceScriptDatumHashSpendTest options testParams
    runTest "inlineDatumSpendTest" BabbageFeatures.inlineDatumSpendTest options testParams
    runTest "referenceInputWithV1ScriptErrorTest" BabbageFeatures.referenceInputWithV1ScriptErrorTest options testParams
    runTest "referenceScriptOutputWithV1ScriptErrorTest" BabbageFeatures.referenceScriptOutputWithV1ScriptErrorTest options testParams
    runTest "inlineDatumOutputWithV1ScriptErrorTest" BabbageFeatures.inlineDatumOutputWithV1ScriptErrorTest options testParams

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

pv8Tests :: H.Property
pv8Tests = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsBabbage8
    preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    runTestWithPosixTime "checkTxInfoV1Test" AlonzoFeatures.checkTxInfoV1Test options testParams preTestnetTime
    runTestWithPosixTime "checkTxInfoV2Test" BabbageFeatures.checkTxInfoV2Test options testParams preTestnetTime
    runTest "datumHashSpendTest" AlonzoFeatures.datumHashSpendTest options testParams
    runTest "mintBurnTest" AlonzoFeatures.mintBurnTest options testParams
    runTest "collateralContainsTokenErrorTest" AlonzoFeatures.collateralContainsTokenErrorTest options testParams
    runTest "missingCollateralInputErrorTest" AlonzoFeatures.missingCollateralInputErrorTest options testParams
    runTest "tooManyCollateralInputsErrorTest" AlonzoFeatures.tooManyCollateralInputsErrorTest options testParams
    runTest "verifySchnorrAndEcdsaTest" Builtins.verifySchnorrAndEcdsaTest options testParams
    runTest "referenceScriptMintTest" BabbageFeatures.referenceScriptMintTest options testParams
    runTest "referenceScriptInlineDatumSpendTest" BabbageFeatures.referenceScriptInlineDatumSpendTest options testParams
    runTest "referenceScriptDatumHashSpendTest" BabbageFeatures.referenceScriptDatumHashSpendTest options testParams
    runTest "inlineDatumSpendTest" BabbageFeatures.inlineDatumSpendTest options testParams
    runTest "referenceInputWithV1ScriptErrorTest" BabbageFeatures.referenceInputWithV1ScriptErrorTest options testParams
    runTest "referenceScriptOutputWithV1ScriptErrorTest" BabbageFeatures.referenceScriptOutputWithV1ScriptErrorTest options testParams
    runTest "inlineDatumOutputWithV1ScriptErrorTest" BabbageFeatures.inlineDatumOutputWithV1ScriptErrorTest options testParams

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

debug :: H.Property
debug = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsBabbage8
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    runTest "referenceScriptOutputWithV1ScriptErrorTest" BabbageFeatures.referenceScriptOutputWithV1ScriptErrorTest options testParams

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

-- localNodeTests :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
-- localNodeTests  options = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
--     --preTestnetTime <- liftIO Time.getPOSIXTime
--     (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
--     let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath

--     -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
--     -- TODO: pass in or query for slot range to use in checkTxInfo tests
--     --runTestWithPosixTime "checkTxInfoV1Test" AlonzoFeatures.checkTxInfoV1Test options testParams preTestnetTime
--     --runTestWithPosixTime "checkTxInfoV2Test" BabbageFeatures.checkTxInfoV2Test options testParams preTestnetTime
--     runTest "verifySchnorrAndEcdsaTest" Builtins.verifySchnorrAndEcdsaTest options testParams
--     runTest "referenceScriptMintTest" BabbageFeatures.referenceScriptMintTest options testParams
--     runTest "referenceScriptInlineDatumSpendTest" BabbageFeatures.referenceScriptInlineDatumSpendTest options testParams
--     runTest "referenceScriptDatumHashSpendTest" BabbageFeatures.referenceScriptDatumHashSpendTest options testParams

--     U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

tests :: TestTree
tests = testGroup "Plutus E2E Tests" [
  testProperty "Alonzo PV6 Tests" pv6Tests,
  testProperty "Babbage PV7 Tests" pv7Tests,
  testProperty "Babbage PV8 Tests" pv8Tests
  --testProperty "debug" debug
  --testProperty "Babbage PV8 Tests (on Preview testnet)" $ localNodeTests TN.localNodeOptionsPreview
  ]

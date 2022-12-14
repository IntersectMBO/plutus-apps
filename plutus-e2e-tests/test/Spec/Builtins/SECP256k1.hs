{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Spec.Builtins.SECP256k1(tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (serialise)
import Control.Lens hiding ((.>))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Void (Void)
import Plutus.Script.Utils.Typed as PSU
import Plutus.V1.Ledger.Scripts qualified as Plutus
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Test.Tasty (TestName, TestTree, testGroup)

import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.IO.Temp qualified as IO
import System.Info qualified as IO

import Hedgehog ((===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Test.Base qualified as H
import Test.Tasty.Hedgehog (testPropertyNamed)

import Helpers qualified as TN
import Testnet.Babbage qualified as TN

tests :: TestTree
tests = testGroup "SECP256k1"
  [ testPropertyNamed "schnorr verify builtin" "testSchnorr" testSchnorr
  ]

{- | Test builtin verifySchnorrSecp256k1Signature can be used to verify multiple signatures using a
   minting policy in a Babbage era transaction.

   Steps:
    - spin up a testnet
    - create a plutus script
    - build and submit a transaction to mint a token using the builtin
    - query the ledger to see if mint was successful
-}
testSchnorr :: H.Property
testSchnorr = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

-- 1: spin up a testnet
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  (localNodeConnectInfo, conf, runtime) <- TN.startTestnet TN.defaultTestnetOptions base tempAbsPath
  let networkId = TN.getNetworkId runtime
  --socketPathAbs <- TN.getSocketPathAbs conf runtime

-- 2: create a minting policy

-- Create an always succeeding validator script

-- THESE WORK
-- 1:
  let
    {-# INLINABLE mkPolicy #-}
    mkPolicy :: BI.BuiltinData -> BI.BuiltinData -> ()
    mkPolicy _ _ = ()

    policy :: Plutus.MintingPolicy
    policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy ||])

    serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV2
    serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV2.unMintingPolicyScript policy

    policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 serialisedPolicyScript :: C.PolicyId

-- 2:
--   let
--     serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV2
--     serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV2.unMintingPolicyScript policy
--       where
--         policy :: Plutus.MintingPolicy
--         policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])

--     policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 serialisedPolicyScript :: C.PolicyId

-- THESE DO NOT WORK (MalformedScriptWitnesses)
-- 1:
--   let
--     {-# INLINABLE mkPolicy #-}
--     mkPolicy :: BI.BuiltinData -> PlutusV2.ScriptContext -> Bool
--     mkPolicy _ _ = True

--     policy :: Plutus.MintingPolicy
--     policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
--          where
--              wrap = PSU.mkUntypedMintingPolicy mkPolicy

--     serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV2
--     serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV2.unMintingPolicyScript policy

--     policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 serialisedPolicyScript :: C.PolicyId

-- 2:
--   let
--     {-# INLINABLE mkPolicy #-}
--     mkPolicy :: BI.BuiltinData -> BI.BuiltinData -> ()
--     mkPolicy red _ =
--       case PlutusV2.fromBuiltinData red of
--         Nothing -> P.traceError "Trace error: Invalid redeemer"
--         Just (vkey, msg, sig) ->
--           if BI.verifySchnorrSecp256k1Signature vkey msg sig
--             then ()
--             else P.traceError "Trace error: Schnorr validation failed"

--     policy :: Plutus.MintingPolicy
--     policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy ||])

--     serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV2
--     serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV2.unMintingPolicyScript policy

--     policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 serialisedPolicyScript :: C.PolicyId

-- 3:
--   let
--     {-# INLINABLE mkPolicy #-}
--     mkPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString) -> PlutusV2.ScriptContext -> Bool
--     mkPolicy (vkey, msg, sig) _ = BI.verifySchnorrSecp256k1Signature vkey msg sig

--     policy :: Plutus.MintingPolicy
--     policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
--       where
--           wrap = PSU.mkUntypedMintingPolicy mkPolicy

--     serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV2
--     serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV2.unMintingPolicyScript policy

--     policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 serialisedPolicyScript :: C.PolicyId

  H.annotateShow policyId -- remove

-- 3: build a transaction

  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <-
    TN.readAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempAbsPath </> "utxo-keys/utxo1.vkey"
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <-
    TN.readAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempAbsPath </> "utxo-keys/utxo1.skey"

  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  (tx1in, C.TxOut _ v _ _) <- do
    utxo <- TN.findUTxOByAddress localNodeConnectInfo address
    H.headM $ Map.toList $ C.unUTxO utxo
  let totalLovelace = C.txOutValueToLovelace v

  tx1CollateralTxIn <- H.headM . Map.keys . C.unUTxO =<< TN.findUTxOByAddress localNodeConnectInfo address
  let collateral = C.TxInsCollateral C.CollateralInBabbageEra [tx1CollateralTxIn]

  pparams <- TN.getBabbageProtocolParams localNodeConnectInfo


  let tokenA = C.AssetId policyId (C.AssetName "A")
      tokenAValue = C.valueFromList [(tokenA, 666)]

      executionUnits = C.ExecutionUnits {C.executionSteps = 500_000, C.executionMemory = 10_000 }

      tx1Fee = 1_000_000 :: C.Lovelace
      amountPaid = totalLovelace - tx1Fee :: C.Lovelace
      --amountReturned = totalLovelace - amountPaid - tx1Fee :: C.Lovelace

      redeemer = C.ScriptDataNumber 555
        --C.ScriptDataConstructor 0 [C.ScriptDataBytes "ab", C.ScriptDataBytes "cd", C.ScriptDataBytes "ef"]

      scriptWitness :: C.ScriptWitness C.WitCtxMint C.BabbageEra
      scriptWitness = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
        (C.PScript serialisedPolicyScript) C.NoScriptDatumForMint redeemer executionUnits

      txOut1 :: C.TxOut ctx C.BabbageEra
      txOut1 =
        C.TxOut
          (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) address)
          (C.TxOutValue C.MultiAssetInBabbageEra $ (C.lovelaceToValue amountPaid <> tokenAValue))
          C.TxOutDatumNone
          C.ReferenceScriptNone
      txBodyContent :: C.TxBodyContent C.BuildTx C.BabbageEra
      txBodyContent = (TN.emptyTxBodyContent tx1Fee pparams)
        { C.txIns = [(tx1in, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
        , C.txInsCollateral = collateral
        , C.txOuts = [txOut1]
        , C.txMintValue = C.TxMintValue C.MultiAssetInBabbageEra tokenAValue (C.BuildTxWith $ Map.singleton policyId scriptWitness)
        , C.txProtocolParams   = C.BuildTxWith $ Just pparams
        }
  tx1body :: C.TxBody C.BabbageEra <- H.leftFail $ C.makeTransactionBody txBodyContent
  let
    kw :: C.KeyWitness C.BabbageEra
    kw = C.makeShelleyKeyWitness tx1body (C.WitnessPaymentKey $ C.castSigningKey genesisSKey)
    tx1 = C.makeSignedTransaction [kw] tx1body
    tx1Id = C.getTxId (C.getTxBody tx1)
  H.annotateShow tx1Id -- remove

  TN.submitTx localNodeConnectInfo tx1

  H.threadDelay 20_000_000 -- wait 20s
  endUtxos <- TN.getAddressTxInsValue localNodeConnectInfo address
  H.annotateShow endUtxos

  H.assert False

-- 4: submit transaction to mint

-- 5. query and assert successful mint


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
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Either
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (Void)
import GHC.Stack qualified as GHC
import Plutus.Script.Utils.Typed as PSU
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Bytes as P
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

import Cardano.Ledger.Alonzo.Scripts as LScripts
import Cardano.Ledger.Alonzo.TxInfo as TxInfo
import Cardano.Ledger.BaseTypes as BTs

data Secp256Params = Secp256Params
    { vkey :: !P.BuiltinByteString,
      msg  :: !P.BuiltinByteString,
      sig  :: !P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Secp256Params

-- move to helpers
bytesFromHex :: ByteString -> ByteString
bytesFromHex b = bytes $ unsafeFromEither $ P.fromHex b

unsafeFromEither ::Show l => Either l r -> r
unsafeFromEither (Left err)    = error (show err)
unsafeFromEither (Right value) = value

defExecutionUnits = C.ExecutionUnits {C.executionSteps = 0, C.executionMemory = 0 }

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
  (localNodeConnectInfo, conf, runtime, socketPathAbs) <- TN.startTestnet TN.defaultTestnetOptions base tempAbsPath
  pparams <- TN.getBabbageProtocolParams localNodeConnectInfo
  let networkId = TN.getNetworkId runtime

  -- set node socket environment for Cardano.Api.Convenience.Query
  liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" (socketPathAbs)

-- 2: create a minting policy

  let
    {-# INLINABLE mkPolicy #-}
    mkPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString) -> PlutusV2.ScriptContext -> Bool
    mkPolicy (vkey, msg, sig) _sc = BI.verifySchnorrSecp256k1Signature vkey msg sig

    policy :: PlutusV2.MintingPolicy
    policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
      where
          wrap = PSU.mkUntypedMintingPolicy mkPolicy

    serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV2
    serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV2.unMintingPolicyScript policy

    policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 serialisedPolicyScript :: C.PolicyId

  H.annotateShow serialisedPolicyScript -- remove
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
 -- let totalLovelace = C.txOutValueToLovelace v

  tx1CollateralTxIn <- H.headM . Map.keys . C.unUTxO =<< TN.findUTxOByAddress localNodeConnectInfo address
  let collateral = C.TxInsCollateral C.CollateralInBabbageEra [tx1CollateralTxIn]

      tokenA = C.AssetId policyId (C.AssetName "A")
      tokenAValue = C.valueFromList [(tokenA, 666)]

      schnorrParams = Secp256Params
         {
            vkey = BI.toBuiltin $ bytesFromHex "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b",
            msg  = BI.toBuiltin $ bytesFromHex "30303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
            sig  = BI.toBuiltin $ bytesFromHex "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b"
         }

      redeemer = C.fromPlutusData $ PlutusV2.toData schnorrParams

      scriptWitness :: C.ScriptWitness C.WitCtxMint C.BabbageEra
      scriptWitness = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
        (C.PScript serialisedPolicyScript) C.NoScriptDatumForMint redeemer defExecutionUnits

      txOut1 :: C.TxOut ctx C.BabbageEra
      txOut1 =
        C.TxOut
          (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) address)
          (C.TxOutValue C.MultiAssetInBabbageEra $ (C.lovelaceToValue 3_000_000 <> tokenAValue))
          C.TxOutDatumNone
          C.ReferenceScriptNone

      txBodyContent :: C.TxBodyContent C.BuildTx C.BabbageEra
      txBodyContent = (TN.emptyTxBodyContent pparams)
        { C.txIns = [(tx1in, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
        , C.txInsCollateral = collateral
        , C.txOuts = [txOut1]
        , C.txMintValue = C.TxMintValue C.MultiAssetInBabbageEra tokenAValue (C.BuildTxWith $ Map.singleton policyId scriptWitness)
        }

  (nodeEraUtxo, _pparams, eraHistory, systemStart, stakePools) <- H.leftFailM . liftIO $
    C.queryStateForBalancedTx C.BabbageEra networkId [tx1in]

--  H.annotateShow cpparams -- remove

  let newTx = unsafeFromEither $ C.constructBalancedTx
             C.BabbageEraInCardanoMode
             txBodyContent
             (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) address) -- change
             Nothing -- Override key witnesses
             nodeEraUtxo   -- tx inputs
             pparams
             eraHistory
             systemStart
             stakePools
             [C.WitnessPaymentKey $ C.castSigningKey genesisSKey]
  H.annotateShow newTx --remove

-- 4: submit transaction to mint

  TN.submitTx localNodeConnectInfo newTx

  H.threadDelay 10_000_000 -- wait 5s

  endUtxos <- TN.getAddressTxInsValue localNodeConnectInfo address
  H.annotateShow endUtxos -- to check if minted - remove

-- 5. query and assert successful mint
  H.assert False

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Spec.Builtins.SECP256k1(tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens ((??))
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as Map
import Plutus.V2.Ledger.Api qualified as PlutusV2
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (fromGroup, testProperty)

import Helpers qualified as TN
import PlutusScripts qualified as PS
import Testnet.Plutus qualified as TN

testnetOptionsAlonzo   = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.AlonzoEra,  TN.protocolVersion = 6}
testnetOptionsBabbage7 = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 7}
testnetOptionsBabbage8 = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 8}

tests :: TestTree
tests = testGroup "SECP256k1"
  [ testProperty "unable to use SECP256k1 builtins in Alonzo PV6" (verifySchnorrAndEcdsa testnetOptionsAlonzo) --change test used or outcome
  , testProperty "unable to use SECP256k1 builtins in Babbage PV7" (verifySchnorrAndEcdsa testnetOptionsBabbage7) --change test used or outcome
  , testProperty "can use SECP256k1 builtins in Babbage PV8" (verifySchnorrAndEcdsa testnetOptionsBabbage8)
  ]

{- | Test that builtins: verifySchnorrSecp256k1Signature and verifyEcdsaSecp256k1Signature can be
   used successfully to mint in a Babbage era transaction.

   Steps:
    - spin up a testnet
    - build and submit a transaction to mint a token using the builtin
    - query the ledger to see if mint was successful
-}
verifySchnorrAndEcdsa :: TN.TestnetOptions -> H.Property
verifySchnorrAndEcdsa testnetOptions = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

  C.AnyCardanoEra (era :: C.CardanoEra era) <- return $ TN.era testnetOptions
  shelleyBasedEra <- case cardanoEraToShelleyBasedEra era of
                       Just era -> pure era
                       Nothing  -> error "Not a shelley era"

-- 1: spin up a testnet
  base <- TN.getProjectBase
  (localNodeConnectInfo, pparams, networkId) <- TN.startTestnet era testnetOptions base tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

-- 2: build a transaction

  txIn <- TN.firstTxIn era localNodeConnectInfo w1Address

  let
    collateral = TN.txInsCollateral era [txIn] -- C.TxInsCollateral C.CollateralInBabbageEra [txIn]
    tokenValues = C.valueFromList [(PS.verifySchnorrAssetId, 4), (PS.verifyEcdsaAssetId, 2)]
    txOut = TN.txOutNoDatumOrRefScript era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses = Map.fromList [PS.verifySchnorrMintWitness era, PS.verifyEcdsaMintWitness era]

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      , C.txMintValue = TN.txMintValue era tokenValues mintWitnesses
      }

  signedTx <- withIsShelleyBasedEra shelleyBasedEra $ TN.buildTx shelleyBasedEra txBodyContent w1Address w1SKey networkId

  TN.submitTx era localNodeConnectInfo signedTx

-- 3. query and assert successful mint

  let expectedTxIn = TN.txInFromSignedTx signedTx 0
  resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn
  txOutHasTokenValue <- TN.txOutHasValue resultTxOut tokenValues

  H.assert txOutHasTokenValue

-- | Maybe converts a C.CardanoEra to a C.ShelleyBasedEra.
cardanoEraToShelleyBasedEra :: C.CardanoEra era -> Maybe (C.ShelleyBasedEra era)
cardanoEraToShelleyBasedEra cEra = case cEra of
    C.ByronEra   -> Nothing
    C.ShelleyEra -> Just C.ShelleyBasedEraShelley
    C.AllegraEra -> Just C.ShelleyBasedEraAllegra
    C.MaryEra    -> Just C.ShelleyBasedEraMary
    C.AlonzoEra  -> Just C.ShelleyBasedEraAlonzo
    C.BabbageEra -> Just C.ShelleyBasedEraBabbage

-- | Run code that needs an 'C.IsShelleyBasedEra era' constraint while you only have a
-- 'C.ShelleyBasedEra era' and not knowing what the specific era is.
withIsShelleyBasedEra :: C.ShelleyBasedEra era -> (C.IsShelleyBasedEra era => r) -> r
withIsShelleyBasedEra era r =
    case era of
        C.ShelleyBasedEraShelley -> r
        C.ShelleyBasedEraAllegra -> r
        C.ShelleyBasedEraMary    -> r
        C.ShelleyBasedEraAlonzo  -> r
        C.ShelleyBasedEraBabbage -> r

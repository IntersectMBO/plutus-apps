{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wmissing-import-lists #-}
module Ledger.Tx.CardanoAPISpec(tests) where

import Cardano.Api (AsType (AsPaymentKey, AsStakeKey), Key (verificationKeyHash), NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), PaymentCredential (PaymentCredentialByKey),
                    StakeAddressReference (NoStakeAddress, StakeAddressByValue), StakeCredential, makeShelleyAddress,
                    shelleyAddressInEra)
import Cardano.Api.Shelley (StakeCredential (StakeCredentialByKey), TxBody (ShelleyTxBody))
import Gen.Cardano.Api.Typed (genAssetName, genValueDefault)
import Gen.Cardano.Api.Typed qualified as Gen
import Ledger (toPlutusAddress)
import Ledger.Test (someValidator)
import Ledger.Tx (Language (PlutusV1), Tx (txMint), Versioned (Versioned), addMintingPolicy)
import Ledger.Tx.CardanoAPI (fromCardanoAssetName, fromCardanoValue, makeTransactionBody, toCardanoAddressInEra,
                             toCardanoAssetName, toCardanoPolicyId, toCardanoTxBodyContent, toCardanoValue)
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V1.Ledger.Scripts (unitRedeemer)

import Cardano.Api qualified as C
import Data.Default (def)
import Data.Function ((&))
import Hedgehog (Gen, Property, forAll, property, tripping, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Ledger.CardanoAPI"
    [ testPropertyNamed "Cardano Address -> Plutus Address roundtrip" "addressRoundTripSpec" addressRoundTripSpec
    , testPropertyNamed "Tx conversion retains minting policy scripts" "txConversionRetainsMPS" convertMintingTx
    , testPropertyNamed "TokenName <- Cardano AssetName roundtrip" "cardanoAssetNameRoundTrip" cardanoAssetNameRoundTrip
    , testPropertyNamed "Plutus Value <- Cardano Value roundtrip" "cardanoValueRoundTrip" cardanoValueRoundTrip
    ]

cardanoAssetNameRoundTrip :: Property
cardanoAssetNameRoundTrip = property $ do
    assetName <- forAll genAssetName
    tripping assetName fromCardanoAssetName toCardanoAssetName

cardanoValueRoundTrip :: Property
cardanoValueRoundTrip = property $ do
    value <- forAll genValueDefault
    tripping value fromCardanoValue toCardanoValue

-- | From a cardano address, we should be able to convert it to a plutus address,
-- back to the same initial cardano address.
addressRoundTripSpec :: Property
addressRoundTripSpec = property $ do
    networkId <- forAll genNetworkId
    shelleyAddr <- shelleyAddressInEra
               <$> forAll (makeShelleyAddress networkId <$> genPaymentCredential
                                                        <*> genStakeAddressReference)
    let plutusAddr = toPlutusAddress shelleyAddr
    case toCardanoAddressInEra networkId plutusAddr of
        Left _      -> Hedgehog.assert False
        Right cAddr -> cAddr === shelleyAddr

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- Gen.genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  Gen.choice
    [ StakeAddressByValue <$> genStakeCredential
    , return NoStakeAddress
    ]

genStakeCredential :: Gen StakeCredential
genStakeCredential = do
  vKey <- Gen.genVerificationKey AsStakeKey
  return . StakeCredentialByKey $ verificationKeyHash vKey

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded


convertMintingTx :: Property
convertMintingTx = property $ do
  let vHash = PV1.validatorHash someValidator
      mps  = MPS.mkForwardingMintingPolicy vHash
  policyId <- either (\err -> do Hedgehog.annotateShow err; Hedgehog.failure) pure $
    toCardanoPolicyId (PV1.mintingPolicyHash mps)
  let vL n = C.valueFromList [(C.AssetId policyId "L", n)]
      tx   = mempty { txMint = vL 1 }
          & addMintingPolicy (Versioned mps PlutusV1) (unitRedeemer, Nothing)
      ectx = toCardanoTxBodyContent (Testnet $ NetworkMagic 1) def [] tx >>= makeTransactionBody Nothing mempty
  case ectx of
    -- Check that the converted tx contains exactly one script
    Right (ShelleyTxBody _ _ [_script] _ _ _) -> do
      Hedgehog.success
    msg -> do
      Hedgehog.annotateShow msg
      Hedgehog.failure

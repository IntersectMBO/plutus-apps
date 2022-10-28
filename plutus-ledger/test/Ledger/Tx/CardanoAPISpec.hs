{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wmissing-import-lists #-}
module Ledger.Tx.CardanoAPISpec(tests) where

import Cardano.Api (AsType (AsPaymentKey, AsStakeKey), AssetId (AdaAssetId, AssetId), Key (verificationKeyHash),
                    NetworkId (Mainnet, Testnet), NetworkMagic (NetworkMagic),
                    PaymentCredential (PaymentCredentialByKey), PolicyId (PolicyId),
                    StakeAddressReference (NoStakeAddress, StakeAddressByValue), StakeCredential, makeShelleyAddress,
                    shelleyAddressInEra)
import Cardano.Api.Shelley (StakeCredential (StakeCredentialByKey), TxBody (ShelleyTxBody))
import Gen.Cardano.Api.Typed (genAssetName, genScriptHash, genValueDefault)
import Gen.Cardano.Api.Typed qualified as Gen
import Ledger.Generators (genAssetClass, genMintingPolicyHash, genTokenName, genValue)
import Ledger.Test (someValidator)
import Ledger.Tx (Language (PlutusV1), Tx (txMint), Versioned (Versioned), addMintingPolicy)
import Ledger.Tx.CardanoAPI (fromCardanoAddressInEra, fromCardanoAssetId, fromCardanoAssetName, fromCardanoPolicyId,
                             fromCardanoValue, makeTransactionBody, toCardanoAddressInEra, toCardanoAssetId,
                             toCardanoAssetName, toCardanoPolicyId, toCardanoTxBodyContent, toCardanoValue)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V1.Ledger.Scripts (unitRedeemer)

import Data.Default (def)
import Data.Function ((&))
import Data.String (fromString)
import Hedgehog (Gen, Property, evalEither, forAll, property, tripping, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Ledger.CardanoAPI"
    [ testPropertyNamed "Cardano Address -> Plutus Address roundtrip" "addressRoundTripSpec" addressRoundTripSpec
    , testPropertyNamed "Tx conversion retains minting policy scripts" "txConversionRetainsMPS" convertMintingTx
    , testPropertyNamed "MintingPolicyHash <- Cardano PolicyId roundtrip" "cardanoPolicyIdRoundTrip" cardanoPolicyIdRoundTrip
    , testPropertyNamed "MintingPolicyHash -> Cardano PolicyId roundtrip" "mintingPolicyHashRoundTrip" mintingPolicyHashRoundTrip
    , testPropertyNamed "TokenName <- Cardano AssetName roundtrip" "cardanoAssetNameRoundTrip" cardanoAssetNameRoundTrip
    , testPropertyNamed "TokenName -> Cardano AssetName roundtrip" "tokenNameRoundTrip" tokenNameRoundTrip
    , testPropertyNamed "AssetClass <- Cardano AssetId roundtrip" "cardanoAssetIdRoundTrip" cardanoAssetIdRoundTrip
    , testPropertyNamed "AssetClass -> Cardano AssetId roundtrip" "assetClassRoundTrip" assetClassRoundTrip
    , testPropertyNamed "Plutus Value <- Cardano Value roundtrip" "cardanoValueRoundTrip" cardanoValueRoundTrip
    , testPropertyNamed "Plutus Value -> Cardano Value roundtrip" "plutusValueRoundTrip" plutusValueRoundTrip
    ]

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genPolicyId :: Gen PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genAssetId :: Gen AssetId
genAssetId = Gen.choice
    [ AssetId <$> genPolicyId <*> genAssetName
    , return AdaAssetId
    ]

cardanoPolicyIdRoundTrip :: Property
cardanoPolicyIdRoundTrip = property $ do
    policyId <- forAll genPolicyId
    tripping policyId fromCardanoPolicyId toCardanoPolicyId

mintingPolicyHashRoundTrip :: Property
mintingPolicyHashRoundTrip = property $ do
    policyHash <- forAll genMintingPolicyHash
    policyId   <- evalEither $ toCardanoPolicyId policyHash
    policyHash === fromCardanoPolicyId policyId

cardanoAssetNameRoundTrip :: Property
cardanoAssetNameRoundTrip = property $ do
    assetName <- forAll genAssetName
    tripping assetName fromCardanoAssetName toCardanoAssetName

tokenNameRoundTrip :: Property
tokenNameRoundTrip = property $ do
    tokenName <- forAll genTokenName
    assetName <- evalEither $ toCardanoAssetName tokenName
    tokenName === fromCardanoAssetName assetName

cardanoAssetIdRoundTrip :: Property
cardanoAssetIdRoundTrip = property $ do
    assetId <- forAll genAssetId
    tripping assetId fromCardanoAssetId toCardanoAssetId

assetClassRoundTrip :: Property
assetClassRoundTrip = property $ do
    assetClass <- forAll genAssetClass
    assetId    <- evalEither $ toCardanoAssetId assetClass
    assetClass === fromCardanoAssetId assetId

cardanoValueRoundTrip :: Property
cardanoValueRoundTrip = property $ do
    value <- forAll genValueDefault
    tripping value fromCardanoValue toCardanoValue

plutusValueRoundTrip :: Property
plutusValueRoundTrip = property $ do
    plutusValue  <- forAll genValue
    cardanoValue <- evalEither $ toCardanoValue plutusValue
    plutusValue === fromCardanoValue cardanoValue

-- | From a cardano address, we should be able to convert it to a plutus address,
-- back to the same initial cardano address.
addressRoundTripSpec :: Property
addressRoundTripSpec = property $ do
    networkId <- forAll genNetworkId
    shelleyAddr <- shelleyAddressInEra
               <$> forAll (makeShelleyAddress networkId <$> genPaymentCredential
                                                        <*> genStakeAddressReference)
    let plutusAddr = fromCardanoAddressInEra shelleyAddr
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
      mpsHash = PV1.mintingPolicyHash mps
      vL n = Value.singleton (Value.mpsSymbol mpsHash) "L" n
      tx   = mempty { txMint = vL 1 }
          & addMintingPolicy (Versioned mps PlutusV1) (unitRedeemer, Nothing)
      ectx = toCardanoTxBodyContent def [] tx >>= makeTransactionBody mempty
  case ectx of
    -- Check that the converted tx contains exactly one script
    Right (ShelleyTxBody _ _ [_script] _ _ _) -> do
      Hedgehog.success
    msg -> do
      Hedgehog.annotateShow msg
      Hedgehog.failure

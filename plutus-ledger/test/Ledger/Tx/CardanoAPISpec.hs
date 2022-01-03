{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wmissing-import-lists #-}
module Ledger.Tx.CardanoAPISpec(tests) where

import Cardano.Api (AsType (AsPaymentKey, AsStakeKey), Key (verificationKeyHash), NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), PaymentCredential (PaymentCredentialByKey),
                    StakeAddressReference (NoStakeAddress, StakeAddressByValue), StakeCredential, makeShelleyAddress,
                    shelleyAddressInEra)
import Cardano.Api.Shelley (StakeCredential (StakeCredentialByKey))
import Ledger ()
import Ledger.Tx.CardanoAPI (fromCardanoAddress, toCardanoAddress)

import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Ledger.CardanoAPI"
    [ testProperty "Cardano Address -> Plutus Address roundtrip" addressRoundTripSpec
    ]

-- | From a cardano address, we should be able to convert it to a plutus address,
-- back to the same initial cardano address.
addressRoundTripSpec :: Property
addressRoundTripSpec = property $ do
    networkId <- forAll genNetworkId
    shelleyAddr <- shelleyAddressInEra
               <$> forAll (makeShelleyAddress networkId <$> genPaymentCredential
                                                        <*> genStakeAddressReference)
    case fromCardanoAddress shelleyAddr of
        Left _ -> Hedgehog.assert False
        Right plutusAddr ->
            case toCardanoAddress networkId plutusAddr of
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

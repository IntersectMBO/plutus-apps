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
import Data.Default (def)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Test (someValidator)
import Ledger.Tx (RedeemerPtr (RedeemerPtr), ScriptTag (Mint), Tx (txMint, txMintScripts, txRedeemers))
import Ledger.Tx.CardanoAPI (fromCardanoAddress, makeTransactionBody, toCardanoAddress)
import Ledger.Validation (fromPlutusTxToTxBodyContent)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V1.Scripts (mintingPolicyHash, validatorHash)
import Plutus.Script.Utils.V1.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V1.Ledger.Scripts (unitRedeemer)

import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Ledger.CardanoAPI"
    [ testPropertyNamed "Cardano Address -> Plutus Address roundtrip" "addressRoundTripSpec" addressRoundTripSpec
    , testPropertyNamed "Tx conversion retains minting policy scripts" "txConversionRetainsMPS" convertMintingTx
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


convertMintingTx :: Property
convertMintingTx = property $ do
  let vHash = validatorHash someValidator
      mps  = MPS.mkForwardingMintingPolicy vHash
      vL n = Value.singleton (Value.mpsSymbol $ mintingPolicyHash mps) "L" n
      tx   = mempty
        { txMint = vL 1
        , txMintScripts = Set.singleton mps
        , txRedeemers = Map.singleton (RedeemerPtr Mint 0) unitRedeemer
        }
      ectx = fromPlutusTxToTxBodyContent def [] tx >>= makeTransactionBody mempty
  case ectx of
    -- Check that the converted tx contains exactly one script
    Right (ShelleyTxBody _ _ [_script] _ _ _) -> do
      Hedgehog.success
    msg -> do
      Hedgehog.annotateShow msg
      Hedgehog.failure

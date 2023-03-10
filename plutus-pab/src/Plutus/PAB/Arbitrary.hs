{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Temporary code that'll make it easy for us to generate arbitrary events.
-- This should either be deleted when we can get real events, or at least moved
-- across to the test suite.
module Plutus.PAB.Arbitrary where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Params (testnet)
import Control.Monad (replicateM)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Either.Combinators (rightToMaybe)
import Ledger (TxOut (TxOut))
import Ledger qualified
import Ledger.Address (PaymentPubKey, PaymentPubKeyHash, StakePubKey, StakePubKeyHash)
import Ledger.Crypto (PubKey, Signature)
import Ledger.Interval (Extended, Interval, LowerBound, UpperBound)
import Ledger.Scripts (Language (..), Versioned (..))
import Ledger.Slot (Slot)
import Ledger.Tx (Certificate, RedeemerPtr, ScriptTag, TxId, TxIn, TxInType, TxInput, TxInputType, TxOutRef, Withdrawal)
import Ledger.Tx.CardanoAPI (ToCardanoError, toCardanoAddressInEra, toCardanoTxOut)
import Ledger.Tx.Constraints (MkTxError)
import Ledger.Value.CardanoAPI (policyId)
import Plutus.Contract.Effects (ActiveEndpoint (..), PABReq (..), PABResp (..))
import Plutus.Contract.StateMachine (ThreadToken)
import Plutus.Script.Utils.Ada qualified as Plutus
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Typed.Scripts (ConnectionError, WrongOutTypeError)
import Plutus.Script.Utils.Value qualified as Plutus
import Plutus.V1.Ledger.Api (Address (..), LedgerBytes, PubKeyHash, ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Bytes qualified as LedgerBytes
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (Gen, Positive (..), oneof, sized, suchThatMap)
import Test.QuickCheck.Arbitrary.Generic (Arbitrary, Arg, arbitrary, genericArbitrary, genericShrink, shrink)
import Test.QuickCheck.Instances ()
import Wallet (WalletAPIError)
import Wallet.Types (EndpointDescription (..), EndpointValue (..))

-- | A validator that always succeeds.
acceptingValidator :: Ledger.Validator
acceptingValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| (\_ _ _ -> ()) ||])

-- | A minting policy that always succeeds.
acceptingMintingPolicy :: Ledger.MintingPolicy
acceptingMintingPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [|| (\_ _ -> ()) ||])

instance Arbitrary PlutusTx.BuiltinByteString where
    arbitrary = PlutusTx.toBuiltin <$> (arbitrary :: Gen ByteString)

instance Arbitrary LedgerBytes where
    arbitrary = LedgerBytes.fromBytes <$> arbitrary

instance Arbitrary Ledger.MintingPolicy where
    arbitrary = pure acceptingMintingPolicy

instance Arbitrary Ledger.MintingPolicyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Script where
    arbitrary = oneof [
          pure $ Ledger.unValidatorScript acceptingValidator
        , pure $ Ledger.unMintingPolicyScript acceptingMintingPolicy
        ]

instance Arbitrary Ledger.ScriptHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.ValidationError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.ScriptError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MkTxError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ConnectionError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WrongOutTypeError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletAPIError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ToCardanoError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxIn where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxInputType where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxInput where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PV2.OutputDatum where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxOut where
    arbitrary = fmap (fmap TxOut . toCardanoTxOut testnet) genericArbitrary `suchThatMap` rightToMaybe
    shrink = pure

instance Arbitrary TxOutRef where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxInType where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Withdrawal where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Certificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Credential where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.StakingCredential where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.DCert where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ScriptTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RedeemerPtr where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Value where
    arbitrary = oneof [Aeson.String <$> arbitrary, Aeson.Number <$> arbitrary]

instance (Arg (Extended a) a, Arbitrary a) => Arbitrary (Extended a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arg (Extended a) a, Arg (LowerBound a) a, Arbitrary a) => Arbitrary (LowerBound a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arg (Extended a) a, Arg (UpperBound a) a, Arbitrary a) => Arbitrary (UpperBound a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arg (Extended a) a, Arg (LowerBound a) a, Arg (UpperBound a) a, Arg (Interval a) a, Arbitrary a) => Arbitrary (Interval a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PaymentPubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PaymentPubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StakePubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StakePubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Slot where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Signature where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ThreadToken where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PlutusTx.Data where
    arbitrary = sized arbitraryData
      where
        arbitraryData :: Int -> Gen PlutusTx.Data
        arbitraryData n =
            oneof [ arbitraryConstr n
                  , arbitraryMap n
                  , arbitraryList n
                  , arbitraryI
                  , arbitraryB
                  ]

        arbitraryConstr n = do
          (n', m) <- segmentRange (n - 1)
          (Positive ix) <- arbitrary
          args <- replicateM m (arbitraryData n')
          pure $ PlutusTx.Constr ix args

        arbitraryMap n = do
           -- NOTE: A pair always has at least 2 constructors/nodes so we divide by 2
          (n', m) <- segmentRange ((n - 1) `div` 2)
          PlutusTx.Map <$> replicateM m (arbitraryPair n')

        arbitraryPair n = do
          (,) <$> arbitraryData half <*> arbitraryData half
          where
            half = n `div` 2

        arbitraryList n = do
          (n', m) <- segmentRange (n - 1)
          PlutusTx.List <$> replicateM m (arbitraryData n')

        arbitraryI =
          PlutusTx.I <$> arbitrary

        arbitraryB =
          PlutusTx.B <$> arbitrary

        -- Used to break the sized generator up more or less evenly
        segmentRange n = do
          (Positive m) <- arbitrary
          let n' = n `div` (m + 1) -- Prevent division by 0
          pure (n', if n' > 0 then m else 0) -- Prevent segments of 0

    shrink = genericShrink

instance Arbitrary PlutusTx.BuiltinData where
    arbitrary = PlutusTx.dataToBuiltinData <$> arbitrary
    shrink d = PlutusTx.dataToBuiltinData <$> shrink (PlutusTx.builtinDataToData d)

instance Arbitrary Ledger.Language where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arg (Ledger.Versioned script) script, Arbitrary script) => Arbitrary (Ledger.Versioned script) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Datum where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.DatumHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Redeemer where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Validator where
    arbitrary = pure acceptingValidator

instance Arbitrary Plutus.TokenName where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Plutus.CurrencySymbol where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Plutus.Ada where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.Lovelace where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Plutus.Value where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.Value where
    arbitrary = C.valueFromList <$> arbitrary

instance Arbitrary C.AssetId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.AssetName where
    arbitrary = C.AssetName <$> arbitrary

instance Arbitrary C.Quantity where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.PolicyId where
    arbitrary = pure $ policyId (Versioned acceptingMintingPolicy PlutusV1)

instance (Arbitrary k, Arbitrary v) => Arbitrary (AssocMap.Map k v) where
    arbitrary = AssocMap.fromList <$> arbitrary

instance Arbitrary PABReq where
    arbitrary =
        oneof
            [ AwaitSlotReq <$> arbitrary
            , pure CurrentNodeClientSlotReq
            , pure CurrentChainIndexSlotReq
            , pure OwnContractInstanceIdReq
            , ExposeEndpointReq <$> arbitrary
            , pure OwnAddressesReq
            -- TODO This would need an Arbitrary Tx instance:
            -- , BalanceTxRequest <$> arbitrary
            -- , WriteBalancedTxRequest <$> arbitrary
            ]

instance Arbitrary Address where
    arbitrary = oneof [Ledger.pubKeyAddress <$> arbitrary <*> arbitrary, mkValidatorAddress <$> arbitrary]

instance Arbitrary (C.AddressInEra C.BabbageEra) where
    arbitrary = fmap (toCardanoAddressInEra testnet) genericArbitrary `suchThatMap` rightToMaybe

instance Arbitrary ValidatorHash where
    arbitrary = ValidatorHash <$> arbitrary

instance Arbitrary EndpointDescription where
    arbitrary = EndpointDescription <$> arbitrary

instance Arbitrary ActiveEndpoint where
    arbitrary = ActiveEndpoint . EndpointDescription <$> arbitrary <*> arbitrary

-- Maintainer's note: These requests are deliberately excluded - some
-- problem with the arbitrary instances for the responses never
-- terminating.
--
-- Since we're not going to keep this code for long, I won't worry
-- about fixing it, but I'll leave the offending data there as a
-- warning sign around the rabbit hole:
-- bad :: [Gen ContractRequest]
-- bad =
--     [ BalanceTxRequest <$> arbitrary
--     , WriteBalancedTxRequest <$> arbitrary
--     ]

-- | Generate responses for mock requests. This function returns a
-- 'Maybe' because we can't (yet) create a generator for every request
-- type.
genResponse :: PABReq -> Maybe (Gen PABResp)
genResponse (AwaitSlotReq slot)   = Just . pure . AwaitSlotResp $ slot
genResponse (ExposeEndpointReq _) = Just $ ExposeEndpointResp <$> arbitrary <*> (EndpointValue <$> arbitrary)
genResponse OwnAddressesReq       = Just $ OwnAddressesResp <$> arbitrary
genResponse _                     = Nothing

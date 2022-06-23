{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.NetworkId.ExtraSpec
    ( tests
    ) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (..))
import Control.Monad (void)
import Data.Aeson (FromJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Hedgehog (MonadGen, Property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.SmallCheck.Series qualified as SC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck qualified as QC
import Test.Tasty.SmallCheck qualified as SC

tests :: TestTree
tests =
    testGroup
        "Cardano.Api.NetworkId.Extra"
        [ testPropertyNamed "NetworkIdWrapper FromJSON->ToJSON inverse property" "jsonInvProp" jsonInvProp
        ]

jsonInvProp :: Property
jsonInvProp = Hedgehog.property $ do
    networkId <- Hedgehog.forAll networkIdGen
    Hedgehog.tripping networkId encode decode

networkIdGen :: MonadGen m => m NetworkIdWrapper
networkIdGen = do
    nid <- Gen.word32 (Range.linear 0 100)
    Gen.element [ NetworkIdWrapper Mainnet
                , NetworkIdWrapper $ Testnet $ NetworkMagic nid
                ]

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Spec.Rows(tests) where

import Data.Aeson qualified as Aeson
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit

import Plutus.Contract
import Plutus.Contract.Request qualified as Request

type TheSchema = Endpoint "endpoint1" Int .\/ Endpoint "endpoint2" String

tests :: TestTree
tests = testGroup "JSON instances"
         [ HUnit.testCase "should round-trip" $ do
            let e = Request.endpointReq @"endpoint1" @_ @TheSchema
                e' = Aeson.eitherDecode $ Aeson.encode e
            HUnit.assertBool "round-trip failed" $ Right e == e'
         ]

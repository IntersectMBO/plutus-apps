{-# LANGUAGE TemplateHaskell #-}

module Test.PlutusExample.ScriptData where

import Cardano.Prelude

import Cardano.Api

import Data.Aeson qualified as Aeson

import Hedgehog (Property, forAll, property, tripping, (===))
import Hedgehog.Internal.Property (failWith)

import PlutusExample.PlutusVersion1.RedeemerContextScripts
import PlutusExample.ScriptContextChecker

import Test.PlutusExample.Gen

prop_ScriptData_MyCustomRedeemer :: Property
prop_ScriptData_MyCustomRedeemer =
  property $ do
    myCusRedeem <- forAll genMyCustomRedeemer
    tripping myCusRedeem
             customRedeemerToScriptData
             (\v -> AnyPV1CustomRedeemer <$> pv1CustomRedeemerFromScriptData v)

prop_ScriptData_MyCustomRedeemer_JSON :: Property
prop_ScriptData_MyCustomRedeemer_JSON =
  property $ do
    myCusRedeem <- forAll genMyCustomRedeemer
    let sData = Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ customRedeemerToScriptData myCusRedeem
    case Aeson.eitherDecode sData of
      Left err -> failWith Nothing $ "Failed to decode script data (eitherDecode): " ++ err
      Right sDataJsonValue ->
        case scriptDataFromJson ScriptDataJsonDetailedSchema sDataJsonValue of
          Left err ->
            failWith Nothing $ "Failed to decode script data (scriptDataFromJson): " ++ displayError err
          Right sDataDecoded ->
            case AnyPV1CustomRedeemer <$> pv1CustomRedeemerFromScriptData sDataDecoded of
              Left err ->
                failWith Nothing $ "Failed to decode custom redeemer(pv1CustomRedeemerFromScriptData): " ++ err
              Right cusRedDecoded -> do
                 myCusRedeem === cusRedDecoded
                 sData === Aeson.encode
                             (scriptDataToJson ScriptDataJsonDetailedSchema
                               $ customRedeemerToScriptData cusRedDecoded)


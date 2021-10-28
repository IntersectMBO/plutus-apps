module StaticDataTests
  ( all
  ) where

import Prologue
import Data.Either (isRight)
import StaticData (mkContractDemos)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

all :: Spec Unit
all =
  describe "StaticData" do
    simulationDecodingSpec

simulationDecodingSpec :: Spec Unit
simulationDecodingSpec =
  describe "Simulation Decoding" do
    it "contractDemos" do
      mkContractDemos `shouldSatisfy` isRight

module StaticDataTests
  ( all
  ) where

import Prologue
import StaticData (mkContractDemos)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

all :: TestSuite
all =
  suite "StaticData" do
    simulationDecodingTests

simulationDecodingTests :: TestSuite
simulationDecodingTests =
  suite "Simulation Decoding" do
    test "contractDemos" $ equal (Right unit) $ unit <$ mkContractDemos

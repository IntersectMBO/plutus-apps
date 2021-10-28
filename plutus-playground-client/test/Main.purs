module Test.Main where

import Prologue
import ChainTests as ChainTests
import Data.BigInt.Argonaut (withJsonPatch)
import EditorTests as EditorTests
import Effect (Effect)
import Effect.Aff (launchAff_)
import GistsTests as GistsTests
import MainFrameTests as MainFrameTests
import Schema.TypesTests as Schema.TypesTests
import StaticDataTests as StaticDataTests
import Test.Spec (around_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

foreign import forDeps :: Effect Unit

main :: Effect Unit
main =
  launchAff_ do
    runSpec [ consoleReporter ]
      $ around_ withJsonPatch do
          ChainTests.all
          EditorTests.all
          GistsTests.all
          StaticDataTests.all
          MainFrameTests.all
          Schema.TypesTests.all

module EditorTests
  ( all
  ) where

import Prologue
import Data.Traversable (for_)
import Editor.Types (allKeyBindings, readKeyBindings)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

all :: Spec Unit
all =
  describe "Editor" do
    readShowKeyBindingsTests

readShowKeyBindingsTests :: Spec Unit
readShowKeyBindingsTests =
  it "readShowKeyBindingsTests" do
    for_ allKeyBindings \keyBindings ->
      readKeyBindings (show keyBindings) `shouldEqual` keyBindings

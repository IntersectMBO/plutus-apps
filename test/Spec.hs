import           Data.Maybe              (fromJust, isJust, isNothing)
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Index                   (Index, IndexView (..))
import qualified Index                   as Ix
import qualified Spec.Index              as Ix
import qualified Index.Split             as S

import qualified Debug.Trace             as Debug

tests :: TestTree
tests = testGroup "Index" [ixProperties]

ixProperties :: TestTree
ixProperties = testGroup "Basic model"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int Ix.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 10000 $ Ix.prop_insertRewindInverse @Int @Int
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 10000 $ Ix.prop_observeInsert @Int @Int
  , testProperty "Insert is increasing the length unless overflowing" $
      withMaxSuccess 10000 $ Ix.prop_insertSize @Int @Int
  ]


main :: IO ()
main = do
  -- quickSpec ixSignature
  defaultMain tests

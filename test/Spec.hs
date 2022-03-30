import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Spec.Index            as Ix
import qualified Spec.Split            as S

tests :: TestTree
tests = testGroup "Index" [ixProperties, siProperties]

ixProperties :: TestTree
ixProperties = testGroup "Basic model"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int Ix.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int Ix.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int Ix.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 10000 $ Ix.prop_insertRewindInverse @Int @Int @Int Ix.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 10000 $ Ix.prop_observeInsert @Int @Int @Int Ix.conversion
  , testProperty "Notifications are accumulated as the fold runs" $
      withMaxSuccess 10000 $ Ix.prop_observeNotifications @Int @Int @Int Ix.conversion
  ]

siProperties :: TestTree
siProperties = testGroup "Split index"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int S.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int S.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int S.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 10000 $ Ix.prop_insertRewindInverse @Int @Int @Int S.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 10000 $ Ix.prop_observeInsert @Int @Int @Int S.conversion
  ]

main :: IO ()
main = do
  -- quickSpec ixSignature
  defaultMain tests

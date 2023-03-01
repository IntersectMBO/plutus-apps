import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

import Marconi.Core.Model qualified as Ix
import Marconi.Core.Spec.Sqlite qualified as S
import Marconi.Core.Spec.VSplit qualified as V
import Marconi.Core.Spec.VSqlite qualified as VS
import Marconi.Core.Trace qualified as Ix

tests :: TestTree
tests = testGroup "Index" [ ixProperties, sProperties, viProperties, vsProperties
                          , traceProperties ]

traceProperties :: TestTree
traceProperties = testGroup "Model traces"
  [ testProperty "Weak bisimilarity (observed builder)" $
      withMaxSuccess 10000 $ Ix.prop_WeakBisimilarity  @Int @Int @Int Ix.modelConversion
  , testProperty "Weak bisimilarity (grammar builder)" $
      withMaxSuccess 100  $ Ix.prop_WeakBisimilarity' @Int @Int @Int Ix.modelConversion
  ]

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
  ,  testProperty "Notifications are not affected by rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindNotifications @Int @Int @Int Ix.conversion
  ]

sProperties :: TestTree
sProperties = testGroup "New index properties."
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int S.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 1000 $ Ix.prop_sizeLEDepth @Int @Int S.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 1000 $ Ix.prop_rewindDepth @Int @Int S.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int S.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int S.conversion
  ]

viProperties :: TestTree
viProperties = testGroup "Vector index"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int V.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int V.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int V.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int @Int V.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int @Int V.conversion
  , testProperty "Notifications are accumulated as the fold runs" $
      withMaxSuccess 1000 $ Ix.prop_observeNotifications @Int @Int @Int V.conversion
  ,  testProperty "Notifications are not affected by rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindNotifications @Int @Int @Int V.conversion
  ]

vsProperties :: TestTree
vsProperties = testGroup "SQLite vector index"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int VS.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int VS.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int VS.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int @Int VS.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int @Int VS.conversion
  , testProperty "Notifications are accumulated as the fold runs" $
      withMaxSuccess 1000 $ Ix.prop_observeNotifications @Int @Int @Int VS.conversion
  ,  testProperty "Notifications are not affected by rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindNotifications @Int @Int @Int VS.conversion
  ]

main :: IO ()
main = do
  -- quickSpec ixSignature
  defaultMain tests

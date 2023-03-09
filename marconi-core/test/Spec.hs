import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

import Marconi.Core.Model qualified as Ix
import Marconi.Core.Spec.Sqlite qualified as S
import Marconi.Core.Spec.TracedSqlite qualified as TS
import Marconi.Core.Spec.VSplit qualified as V
import Marconi.Core.Spec.VSqlite qualified as VS
import Marconi.Core.Trace qualified as Ix

tests :: TestTree
tests = testGroup "Everything" [ indexTests, traceTests ]

indexTests :: TestTree
indexTests = testGroup "Index" [ ixProperties, sProperties, viProperties, vsProperties
                               , tProperties ]

traceTests :: TestTree
traceTests = testGroup "Trace" [ traceModelProperties, traceIndexerProperties ]

traceModelProperties :: TestTree
traceModelProperties = testGroup "Model traces"
  [ testProperty "Weak bisimilarity (observed builder)" $
      withMaxSuccess 10000 $ Ix.prop_WeakBisimilarity  @Int @Int @Int Ix.modelConversion
  , testProperty "Weak bisimilarity (grammar builder)" $
      withMaxSuccess 300  $ Ix.prop_WeakBisimilarity' @Int @Int @Int Ix.modelConversion
  ]

traceIndexerProperties :: TestTree
traceIndexerProperties = testGroup "Implementation traces"
  [ testProperty "Weak bisimilarity (observed builder)" $
      withMaxSuccess 10000 $ Ix.prop_WeakBisimilarity  TS.observeTrace
  , testProperty "Weak bisimilarity (grammar builder)" $
      withMaxSuccess 300  $ Ix.prop_WeakBisimilarity' TS.observeTrace
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

tProperties :: TestTree
tProperties = testGroup "New traced index properties."
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int TS.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 1000 $ Ix.prop_sizeLEDepth @Int @Int TS.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 1000 $ Ix.prop_rewindDepth @Int @Int TS.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int TS.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int TS.conversion
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
  ]

main :: IO ()
main = do
  -- quickSpec ixSignature
  defaultMain tests

import Test.Tasty
import Test.Tasty.QuickCheck

import Plutus.HystericalScreams.Spec.Index qualified as Ix
import Plutus.HystericalScreams.Spec.Split qualified as S
import Plutus.HystericalScreams.Spec.Sqlite qualified as Sqlite
import Plutus.HystericalScreams.Spec.VSplit qualified as V
import Plutus.HystericalScreams.Spec.VSqlite qualified as VS

tests :: TestTree
tests = testGroup "Index" [ixProperties, siProperties, sqProperties, viProperties, vsProperties]

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

siProperties :: TestTree
siProperties = testGroup "Split index"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int S.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int S.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int S.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int @Int S.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int @Int S.conversion
  , testProperty "Notifications are accumulated as the fold runs" $
      withMaxSuccess 1000 $ Ix.prop_observeNotifications @Int @Int @Int S.conversion
  ,  testProperty "Notifications are not affected by rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindNotifications @Int @Int @Int S.conversion
  ]

sqProperties :: TestTree
sqProperties = testGroup "Sqlite index"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int Sqlite.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int Sqlite.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int Sqlite.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int @Int Sqlite.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int @Int Sqlite.conversion
  , testProperty "Notifications are accumulated as the fold runs" $
      withMaxSuccess 1000 $ Ix.prop_observeNotifications @Int @Int @Int Sqlite.conversion
  ,  testProperty "Notifications are not affected by rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindNotifications @Int @Int @Int Sqlite.conversion
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

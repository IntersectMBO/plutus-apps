module Plutus.HystericalScreams.Spec.Sqlite where

import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Maybe (catMaybes)
import Data.Sequence (Seq, (><))
import Data.Sequence qualified as Seq
import Database.SQLite.Simple (Only (..), execute, execute_, query)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, monadicIO)
import Test.QuickCheck.Monadic qualified as M

import Plutus.HystericalScreams.Index (Index, IndexView (..))
import Plutus.HystericalScreams.Index qualified as Ix
import Plutus.HystericalScreams.Index.Split (SplitIndex (..))
import Plutus.HystericalScreams.Index.Sqlite (SqliteIndex)
import Plutus.HystericalScreams.Index.Sqlite qualified as S
import Plutus.HystericalScreams.Spec.Index (Conversion (..))

conversion
  :: (Show e, Show n, Show a, Default a, ToField a, FromField a)
  => Conversion (PropertyM IO) a e n
conversion = Conversion
  { cView          = view
  , cHistory       = history
  , cNotifications = notifications
  , cMonadic       = monadic
  }

stateId :: Int
stateId = 1

view
  :: (Show a, Default a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe (IndexView a))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      v <- M.run $ S.view stateId ix'
      pure $ Just v

notifications
  :: (Show a, Default a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO [n]
notifications ix = do
  -- We should never call this on invalid indexes.
  Just ix' <- run ix
  liftIO $ S.getNotifications ix'

history
  :: (Show a, Default a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe [a])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      h <- S.getHistory stateId ix'
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

run
  :: forall a e n. (Show a, Default a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe (SqliteIndex e n Int a))
run (Ix.New f depth acc) = do
  sqliteIndex <- liftIO $ S.new fquery foninsert fstore depth ":memory:"
  case sqliteIndex of
    Nothing -> pure Nothing
    Just ix -> do
      let c = siHandle ix
      -- Initialise database
      liftIO $ do
        execute_ c "DROP TABLE IF EXISTS index_property_tests"
        execute_ c "CREATE TABLE index_property_tests (id INTEGER PRIMARY KEY, accumulator INT)"
        execute  c "INSERT INTO index_property_tests (id, accumulator) VALUES (?, ?)" (stateId, acc)
      pure . Just $ ix
  where
    fstore     :: SqliteIndex e n Int a -> IO ()
    fstore ix@SplitIndex{siHandle} = do
      currentStore <- fquery ix stateId Seq.empty
      execute siHandle "UPDATE index_property_tests SET accumulator = ? WHERE id = ?" (currentStore, stateId)
    fquery :: SqliteIndex e n Int a -> Int -> Seq e -> IO a
    fquery SplitIndex{siHandle, siBuffered} stateId' es = do
      [[storedState]] <- query siHandle "SELECT (accumulator) FROM index_property_tests WHERE id = ?" (Only stateId')
      pure . fst $ foldr convertIxF (storedState, []) (es >< siBuffered)
    foninsert :: e -> SqliteIndex e n Int a -> IO [n]
    foninsert e ix@SplitIndex{siEvents} = do
      currentState <- fquery ix stateId siEvents
      pure $ catMaybes [snd $ f currentState e]
    convertIxF :: e -> (a, [n]) -> (a, [n])
    convertIxF e (a, ns) =
      let (a', mn) = f a e
       in (a', catMaybes [mn] ++ ns)
run (Ix.Insert e ix) = do
  mix <- run ix
  case  mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      nix <- S.insert e ix'
      pure $ Just nix
run (Ix.Rewind n ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO . pure $ S.rewind n ix'

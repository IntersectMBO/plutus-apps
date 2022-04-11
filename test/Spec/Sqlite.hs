module Spec.Sqlite where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Maybe              (catMaybes)
import           Database.SQLite.Simple  (Connection, query, execute_, execute, Only(..))
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import           Test.QuickCheck         (Property)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO)
import qualified Test.QuickCheck.Monadic as M

import           Index                   (Index, IndexView (..))
import qualified Index                   as Ix
import           Index.Split             (SplitIndex(..))
import           Index.Sqlite            (SqliteIndex)
import qualified Index.Sqlite            as S
import           Spec.Index              (Conversion (..))

conversion :: (Show e, Show n, Show a, ToField a, FromField a) => Conversion (PropertyM IO) a e n
conversion = Conversion
  { cView          = view
  , cHistory       = history
  , cNotifications = notifications
  , cMonadic       = monadic
  }

view
  :: (Show a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe (IndexView a))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      v <- M.run $ S.view ix'
      pure $ Just v

notifications
  :: (Show a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO [n]
notifications ix = do
  -- We should never call this on invalid indexes.
  Just ix' <- run ix
  liftIO $ S.getNotifications ix'

history
  :: (Show a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe [a])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      h <- S.getHistory ix'
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

run
  :: forall a e n. (Show a, ToField a, FromField a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe (SqliteIndex a e n))
run (Ix.New f depth acc) = do
  sqliteIndex <- liftIO $ S.new findex fstore fload depth ":memory:"
  case sqliteIndex of
    Nothing -> pure Nothing
    Just ix -> do
      let c = siHandle ix
      -- Initialise database
      liftIO $ do
        execute_ c "DROP TABLE IF EXISTS index_property_tests"
        execute_ c "CREATE TABLE index_property_tests (id INTEGER PRIMARY KEY, accumulator INT)"
        execute  c "INSERT INTO index_property_tests (id, accumulator) VALUES (?, ?)" (1 :: Int, acc)
      pure . Just $ ix
  where
    findex :: a -> [e] -> (a, [n])
    findex a es = foldr convertIxF (a, []) es
    fstore :: Connection -> a -> IO ()
    fstore c a =
      execute c "UPDATE index_property_tests SET accumulator = ? WHERE id = ?" (a, 1 :: Int)
    fload  :: Connection -> IO a
    fload c = do
      [[a]] <- query c "SELECT (accumulator) FROM index_property_tests WHERE id = ?" (Only 1 :: Only Int)
      pure a
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

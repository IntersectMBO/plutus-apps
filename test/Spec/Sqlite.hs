module Spec.Sqlite where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Maybe              (catMaybes)
import           Database.SQLite.Simple  (execute_, execute, Only(..))
import           Test.QuickCheck         (Property)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO)
import qualified Test.QuickCheck.Monadic as M

import           Index                   (Index, IndexView (..))
import qualified Index                   as Ix
import           Index.Split             (SplitIndex(..))
import           Index.Sqlite            (SqliteIndex (..))
import qualified Index.Sqlite            as S
import           Spec.Index              (Conversion (..))

conversion :: (Show e, Show n, Show a) => Conversion (PropertyM IO) a e n
conversion = Conversion
  { cView          = view
  , cHistory       = undefined
  , cNotifications = undefined
  , cMonadic       = undefined
  }

-- conversion :: (Show a, Show e, Show n) => Conversion (PropertyM IO) a e n
-- conversion = Conversion
--   { cView          = view
--   , cHistory       = history
--   , cNotifications = notifications
--   , cMonadic       = monadic
--   }

view
  :: (Show a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe (IndexView a))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      v <- M.run $ S.view ix'
      pure $ Just v

-- notifications
--   :: (Show a, Show e, Show n)
--   => Index a e n
--   -> PropertyM IO [n]
-- notifications ix = do
--   -- We should never call this on invalid indexes.
--   Just ix' <- run ix
--   S.getNotifications ix'

-- history
--   :: (Show a, Show e, Show n)
--   => Index a e n
--   -> PropertyM IO (Maybe [a])
-- history ix = do
--   mix <- run ix
--   case mix of
--     Nothing  -> pure Nothing
--     Just ix' -> do
--       h <- S.getHistory ix'
--       pure $ Just h

-- monadic
--   :: PropertyM IO Property
--   -> Property
-- monadic = monadicIO

run
  :: forall a e n. (Show a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe (SqliteIndex a e n))
run (Ix.New f d a) = undefined
-- run (Ix.New f d a) = do
--   Just sqliteIndex <- liftIO $ S.new findex fstore d "sqlite-index-property-tests.sqlite"
--   let (PartialStore c _) = siStoredIx sqliteIndex
--   -- Initialise database
--   liftIO $ do
--     execute_ c "DROP TABLE IF EXISTS sqlite-index-property-tests"
--     execute_ c "CREATE TABLE sqlite-index-property-tests (id INTEGER PRIMARY KEY, accumulator TEXT)"
--     execute  c "INSERT INTO sqlite-index-property-tests (id, accumulator) VALUES ?" (1, a)
--   pure $ Just sqliteIndex
--   where
--     findex :: PartialStore e -> [e] -> IO (PartialStore e, [n])
--     -- TODO: Is es' always supposed to be empty when calling this function?
--     findex (PartialStore c es') es = do
--       -- TODO: Potential race condition (do we care?)
--       storedAcc <- execute c "SELECT (accumulator) from sqlite-index-property-tests WHERE id = ?" (Only 1)
--       pure $ foldr convertIxF (storedAcc, []) (es' ++ es)
--     fstore :: PartialStore e -> IO (PartialStore e)
--     fstore (PartialStore c es) = do
--       execute_ c "BEGIN"
--       storedAcc <- execute c "SELECT (accumulator) FROM sqlite-index-property-tests WHERE id = ?" (Only 1)
--       let nextAcc = fst $ foldr convertIxF (storedAcc, []) es
--       execute c "UPDATE sqlite-index-property-tests (accumulator = ?) WHERE id = ?" (nextAcc, 1)
--       execute c "COMMIT"
--       pure $ PartialStore c []
--     convertIxF :: e -> (PartialStore e, [n]) -> (PartialStore e, [n])
--     convertIxF e (a', ns) =
--       let (a'', mn) = f a' e
--        in (a'', catMaybes [mn] ++ ns)
run (Ix.Insert e ix) = undefined
-- run (Ix.Insert e ix) = do
--   mix <- run ix
--   case  mix of
--     Nothing  -> pure Nothing
--     Just ix' -> do
--       nix <- S.insert e ix'
--       pure $ Just nix
run (Ix.Rewind n ix) = undefined
-- run (Ix.Rewind n ix) = do
--   mix <- run ix
--   case mix of
--     Nothing  -> pure Nothing
--     Just ix' -> pure $ S.rewind n ix'

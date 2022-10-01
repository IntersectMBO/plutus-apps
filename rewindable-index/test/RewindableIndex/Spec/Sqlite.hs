module RewindableIndex.Spec.Sqlite where

import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as Lens
import Control.Monad (forM_, when)
import Data.Foldable (foldl')
import Data.Functor (void)
import Database.SQLite.Simple qualified as Sql
import Database.SQLite.Simple.FromRow (FromRow (fromRow))
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))

import RewindableIndex.Storable (Buffered (persistToStorage), QueryInterval (QEverything, QInterval),
                                 Queryable (queryStorage), Resumable (resumeFromStorage), Rewindable (rewindStorage),
                                 State, bufferSize, config, emptyState, handle, maxRewindLength)

newtype Point = Point Int

data Event = Event
  { _point :: Point
  , _slot  :: Int
  }
$(Lens.makeLenses ''Event)

data Config = Config
  { _state :: State Sql.Connection Point IO Event
  , _fn    :: Int -> Event -> (Int, Maybe ())
  }
$(Lens.makeLenses ''Config)

data Aggregate = Aggregate
  { _aggregateId    :: Int
  , _aggregateValue :: Int
  }
$(Lens.makeLenses ''Aggregate)

instance FromRow Event where
  fromRow = undefined

instance ToRow Event where
  toRow = undefined

instance ToField Point where
  toField = undefined

instance FromRow Aggregate where
  fromRow = undefined

stateId :: Int
stateId = 1

newSqliteIndexer
  :: (Int -> Event -> (Int, Maybe ()))
  -> Word
  -> Word
  -> IO Config
newSqliteIndexer accFn sz len = do
  h <- Sql.open ":memory:"
  c <- emptyState (fromIntegral sz) (fromIntegral len) h
  Sql.execute_ h "DROP TABLE IF EXISTS index_property_tests"
  -- On-disk cache
  Sql.execute_ h "CREATE TABLE index_property_cache (point INTEGER PRIMARY KEY, event INTEGER)"
  -- Aggregated state
  Sql.execute_ h "CREATE TABLE index_property_tests (id INTEGER PRIMARY KEY, accumulator INT)"
  -- Initial state
  Sql.execute  h "INSERT INTO index_property_tests (id, accumulator) VALUES (?, ?)" (stateId, 0 :: Int)
  pure $ Config c accFn

instance Buffered Config IO Event where
  persistToStorage :: Foldable f => f Event -> Config -> IO Config
  persistToStorage es st = do
    let scale     = 4
        diskLimit = st ^. state . config . maxRewindLength -
                    st ^. state . config . bufferSize
        h         = st ^. state . handle
        f         = st ^. fn

    -- Check if we need to fold on-disk state.
    Sql.execute_ h "BEGIN"
    [[cnt :: Int]] <-
      Sql.query_ h "SELECT count(*) FROM index_property_cache"
    when (cnt > diskLimit * scale) $ do
      es' :: [Event] <-
        Sql.query h "SELECT * FROM index_property_cache ORDER BY POINT ASC LIMIT ?"
          (Sql.Only $ diskLimit * (scale - 1))
      let aggregate = foldl' ((fst .) . f) 0 es'
      void $ Sql.execute h "DELETE FROM index_property_cache WHERE point < ?"
        (Sql.Only $ last es' ^. point)
      void $ Sql.execute h "UPDATE index_property_tests SET accumulator = ? WHERE id = ?"
        (aggregate, stateId)
    Sql.execute_ h "COMMIT"

    -- Append events to cache
    Sql.execute_ h "BEGIN"
    forM_ es $
      Sql.execute h "INSERT INTO index_property_cache (point, event) VALUES (?, ?)"
    Sql.execute_ h "COMMIT"
    pure st

instance Queryable Config Point IO () Int where
  queryStorage :: QueryInterval Point -> Config -> () -> IO Int
  queryStorage qi cfg _ = do
    let h = cfg ^. state . handle
        f = cfg ^. fn
    Sql.execute_ h "BEGIN"
    [aggregate] :: [Aggregate] <-
      Sql.query h "SELECT accumulator FROM index_property_tests WHERE id = ?"
        (Sql.Only stateId)
    es' :: [Event] <-
      case qi of
        -- We will only test this path for now.
        QEverything -> Sql.query_ h "SELECT * from index_property_cache ORDER BY point ASC"
        QInterval start end ->
          Sql.query h "SELECT * from index_property_cache WHERE point > ? AND point < ? ORDER BY point ASC" (start, end)
    Sql.execute_ h "COMMIT"
    pure $ foldl' ((fst .) . f) (aggregate ^. aggregateValue) es'

instance Rewindable Config IO Point where
  rewindStorage :: Point -> Config -> IO (Maybe Config)
  rewindStorage pt cfg = do
    let h = cfg ^. state . handle
    Sql.execute h "DELETE FROM index_property_cache WHERE point > ?" (Sql.Only pt)
    pure $ Just cfg

instance Resumable Config IO Point where
  resumeFromStorage :: Config -> IO [Point]
  resumeFromStorage cfg = do
    let h = cfg ^. state . handle
    es' :: [Event] <-
      Sql.query_ h "SELECT * FROM index_property_cache ORDER BY point DEC"
    pure $ fmap (^. point) es'

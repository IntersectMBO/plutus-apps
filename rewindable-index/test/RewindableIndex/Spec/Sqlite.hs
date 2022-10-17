module RewindableIndex.Spec.Sqlite where

import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as Lens
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (foldl', toList)
import Safe (atMay)
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.Vector qualified as V
import Database.SQLite.Simple qualified as Sql
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, monadicIO)

import RewindableIndex.Model (Conversion (Conversion, cHistory, cMonadic, cNotifications, cView), Index,
                              IndexView (IndexView, ixDepth, ixSize, ixView))
import RewindableIndex.Model qualified as Ix
import RewindableIndex.Storable (Buffered (getStoredEvents, persistToStorage, trimEventStore), HasPoint (getPoint),
                                 QueryInterval (QEverything, QInterval), Queryable (queryStorage),
                                 Resumable (resumeFromStorage), Rewindable (rewindStorage), State, StorableEvent,
                                 StorablePoint, StorableQuery, StorableResult, config, diskBufferSize, emptyState,
                                 memoryBufferSize)
import RewindableIndex.Storable qualified as Storable

import Debug.Trace qualified as Debug

newtype Handle = Handle Sql.Connection

newtype instance StorablePoint Handle = Point Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

data instance StorableEvent Handle = Event
  { sePoint :: StorablePoint Handle
  , seEvent :: Int
  } deriving (Eq, Generic)

type TestFn = Int
           -> Int
           -> (Int, Maybe ())

data instance StorableQuery Handle =
    QEvents { qF :: TestFn }
  | QAccumulator

newtype instance StorableResult Handle = Result
  { getResult :: Int
  }
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

instance HasPoint (StorableEvent Handle) (StorablePoint Handle) where
  getPoint (Event point _) = point

data Config = Config
  { _state :: State Handle IO
  , _fn    :: TestFn
  }
$(Lens.makeLenses ''Config)

data Aggregate = Aggregate
  { _aggId    :: Int
  , _aggValue :: StorableResult Handle
  } deriving (Eq, Generic)
$(Lens.makeLenses ''Aggregate)

instance ToRow (StorableEvent Handle) where
  toRow e = [ toField $ sePoint e
            , toField $ seEvent e
            ]

instance FromRow (StorableEvent Handle) where
  fromRow = Event <$> field <*> field

deriving newtype instance FromField (StorablePoint Handle)

deriving newtype instance ToField (StorablePoint Handle)

deriving newtype instance FromField (StorableResult Handle)

instance FromRow Aggregate where
  fromRow = Aggregate <$> field <*> field

stateId :: Int
stateId = 1

newSqliteIndexer
  :: TestFn
  -> Word
  -> Word
  -> Int
  -> IO (Maybe Config)
newSqliteIndexer accFn memBuf diskBuf ag0 = do
  h <- Sql.open ":memory:"
  c <- emptyState (fromIntegral memBuf) (fromIntegral diskBuf) (Handle h)
  Sql.execute_ h "DROP TABLE IF EXISTS index_property_tests"
  -- On-disk cache
  Sql.execute_ h "CREATE TABLE index_property_cache (point INTEGER PRIMARY KEY, event INTEGER)"
  -- Aggregated state
  Sql.execute_ h "CREATE TABLE index_property_tests (id INTEGER PRIMARY KEY, accumulator INT)"
  -- Initial state
  Sql.execute  h "INSERT INTO index_property_tests (id, accumulator) VALUES (?, ?)" (stateId, ag0)
  pure . Just $ Config c accFn

instance Buffered Handle IO where
  persistToStorage :: Foldable f => f (StorableEvent Handle) -> Handle -> IO Handle
  persistToStorage es (Handle h) = do
    -- Check if we need to fold on-disk state.
    -- Sql.execute_ h "BEGIN"
    -- [[cnt :: Int]] <-
    --   Sql.query_ h "SELECT count(*) FROM index_property_cache"
    -- when (cnt > diskLimit * scale) $ do
    --   es' :: [Event] <-
    --     Sql.query h "SELECT * FROM index_property_cache ORDER BY POINT ASC LIMIT ?"
    --       (Sql.Only $ diskLimit * (scale - 1))
    --   let aggregate = foldl' ((fst .) . f) 0 es'
    --   void $ Sql.execute h "DELETE FROM index_property_cache WHERE point < ?"
    --     (Sql.Only $ last es' ^. point)
    --   void $ Sql.execute h "UPDATE index_property_tests SET accumulator = ? WHERE id = ?"
    --     (aggregate, stateId)
    -- Sql.execute_ h "COMMIT"

    -- Append events to cache
    Sql.execute_ h "BEGIN"
    forM_ es $
      Sql.execute h "INSERT INTO index_property_cache (point, event) VALUES (?, ?)"
    Sql.execute_ h "COMMIT"
    pure $ Handle h

  getStoredEvents :: Handle -> IO [StorableEvent Handle]
  getStoredEvents (Handle h) = do
    Sql.query_ h "SELECT * FROM index_property_cache ORDER BY POINT ASC"

  -- TODO
  trimEventStore :: Handle -> Int -> IO Handle
  trimEventStore h _ = pure h

indexedFn
  :: (Int -> Int -> (Int, Maybe ()))
  -> StorableResult Handle
  -> StorableEvent Handle
  -> (StorableResult Handle, Maybe ())
indexedFn f (Result ag0) (Event _ e) =
  let (r, m) = f ag0 e in
    (Result r, m)

instance Queryable Handle IO where
  queryStorage
    :: Foldable f
    => QueryInterval (StorablePoint Handle)
    -> f (StorableEvent Handle)
    -> Handle
    -> StorableQuery Handle
    -> IO (StorableResult Handle)
  queryStorage _  _ (Handle h) QAccumulator = do
    [ag0] :: [Aggregate] <-
      Sql.query h "SELECT id, accumulator FROM index_property_tests WHERE id = ?"
        (Sql.Only stateId)
    pure $ ag0 ^. aggValue

  queryStorage qi memoryEs (Handle h) (QEvents f)  = do
    Sql.execute_ h "BEGIN"
    aggregate <- queryStorage qi memoryEs (Handle h) QAccumulator
    es' :: [StorableEvent Handle] <-
      case qi of
        -- We will only test this path for now.
        QEverything -> Sql.query_ h "SELECT * from index_property_cache ORDER BY point ASC"
        QInterval start end ->
          Sql.query h "SELECT * from index_property_cache WHERE point >= ? AND point <= ? ORDER BY point ASC" (start, end)
    Sql.execute_ h "COMMIT"
    pure $ foldl' ((fst .) . indexedFn f) aggregate $ toList memoryEs ++ es'

instance Rewindable Handle IO where
  rewindStorage :: StorablePoint Handle -> Handle -> IO (Maybe Handle)
  rewindStorage pt (Handle h) = do
    Sql.execute h "DELETE FROM index_property_cache WHERE point > ?" (Sql.Only pt)
    pure . Just $ Handle h

instance Resumable Handle IO where
  resumeFromStorage :: Handle -> IO [StorablePoint Handle]
  resumeFromStorage (Handle h) = do
    es' :: [StorableEvent Handle] <-
      Sql.query_ h "SELECT * FROM index_property_cache ORDER BY point DEC"
    pure $ fmap sePoint es'

-- * Conversions

type IndexT = Index Int Int ()

conversion
  :: Conversion (PropertyM IO) Int Int ()
conversion = Conversion
  { cView = getView
  , cHistory = getHistory
  , cNotifications = getNotifications
  , cMonadic = monadic
  }

getNotifications
  :: IndexT
  -> PropertyM IO [()]
getNotifications _ = pure []

getHistory
  :: IndexT
  -> PropertyM IO (Maybe [Int])
getHistory ix = do
  mix <- run ix
  case mix of
    Nothing       -> pure Nothing
    Just (ix', _) -> liftIO $ do
      let st = ix' ^. state
          f  = ix' ^. fn
      es <- Storable.getEvents st
      let qs = map (\p -> QInterval (Point p) (Point p))
             $ map seEvent es
      rs  <- fmap getResult <$> mapM (\qi -> Storable.query qi st (QEvents f)) qs
      Result ag0 <- Storable.query QEverything st QAccumulator
      pure . Just $ rs ++ [ag0]

getView
  :: IndexT
  -> PropertyM IO (Maybe (IndexView Int))
getView ix = do
  mix <- run ix
  case mix of
    Nothing       -> pure Nothing
    Just (ix', _) -> do
      Just rs <- getHistory ix
      pure . Just $
        IndexView { ixDepth = ix' ^. state . config . memoryBufferSize +
                              ix' ^. state . config . diskBufferSize
                  , ixView  = head rs
                  , ixSize  = length rs
                  }

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

lookupEvent
  :: Int
  -> Config
  -> MaybeT IO (StorableEvent Handle)
lookupEvent n (Config st _) = MaybeT $ do
  es <- Storable.getEvents st
  pure $ atMay es n

run
  :: IndexT
  -> PropertyM IO (Maybe (Config, Int))
run (Ix.New f depth ag0)
  | depth <= 0 = pure Nothing
  | otherwise = do
      indexer <- liftIO $ newSqliteIndexer f 1 (fromIntegral depth - 1) ag0
      pure $ (,0) <$> indexer
run (Ix.Insert e ix) = do
  mix <- run ix
  case mix of
    Nothing        -> pure Nothing
    Just (ix', sq) -> liftIO $ do
      nextState <- Storable.insert (Event (Point sq) e) (ix' ^. state)
      pure . Just . (, sq + 1) $ ix' { _state = nextState }
run (Ix.Rewind n ix) = do
  mix <- run ix
  case mix of
    Nothing        -> pure Nothing
    Just (ix', sq) -> liftIO . runMaybeT $ do
      e         <- lookupEvent n ix'
      nextState <- MaybeT . liftIO $ Storable.rewind (getPoint e) (ix' ^. state)
      pure . (,sq) $ ix' { _state = nextState }

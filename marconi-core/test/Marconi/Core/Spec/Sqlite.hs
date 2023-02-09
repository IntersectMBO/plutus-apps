module Marconi.Core.Spec.Sqlite where

import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as Lens
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import Database.SQLite.Simple qualified as Sql
import Database.SQLite.Simple.FromField (FromField (fromField))
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Safe (atMay)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, monadicIO)

import Marconi.Core.Model (Conversion (Conversion, cHistory, cMonadic, cNotifications, cView), Index,
                           IndexView (IndexView, ixDepth, ixSize, ixView), getFunction)
import Marconi.Core.Model qualified as Ix
import Marconi.Core.Storable (Buffered (getStoredEvents, persistToStorage), HasPoint (getPoint),
                              QueryInterval (QEverything, QInterval), Queryable (queryStorage),
                              Resumable (resumeFromStorage), Rewindable (rewindStorage), State, StorableEvent,
                              StorableMonad, StorablePoint, StorableQuery, StorableResult, config, emptyState,
                              filterWithQueryInterval, memoryBufferSize)
import Marconi.Core.Storable qualified as Storable

{-
   This is a simplified indexer implementation. The simplification is that all events,
   points, queries and results have type `Int`.

   The testing infrastructure is designed for the previous version of indexers. There
   is a concern that it does not cover properly the current version, but on further
   thinking it is good enough. The bugs found while implementing it seem to point to
   the same conclusion. However, a lot of the code deals with peculiarites that need
   to be considered when adapting the two models.
-}

-- This is the way to defined a new handle. Users of the API can include other information
-- in the handler type, that they need to implement the storable interface.
newtype Handle = Handle Sql.Connection

-- This point for this test will be representated by a point on some blockchain where
-- slot numbers are identified by integers.
data Point =
    Point Int
  | Genesis
  deriving (Eq, Show, Generic)

-- StorablePoints can be either Point (intergers) or the Genesis block.
type instance StorablePoint Handle = Point

-- Since we use sqlite we'll run everything in IO
type instance StorableMonad Handle = IO

-- We should be able to order points.
instance Ord Point where
  Genesis   <= _         = True
  (Point x) <= (Point y) = x <= y
  _         <= _         = False

-- Events are a tuple of the event data (an int) and the point where they were
-- generated.
data instance StorableEvent Handle = Event
  { sePoint :: StorablePoint Handle
  , seEvent :: Int
  } deriving (Show, Eq, Generic)

type TestFn = Int
           -> Int
           -> (Int, Maybe ())

-- We will need to types of queries. One that retrieves the accumulator and one that
-- computes that folding function over a list of events. When folding the events we
-- also pass the folding function.
data instance StorableQuery Handle =
    QEvents { qF :: TestFn }
  | QAccumulator

newtype instance StorableResult Handle = Result
  { getResult :: Int
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

-- We can extract points from events in the expected way.
instance HasPoint (StorableEvent Handle) Point where
  getPoint (Event point _) = point

data Config = Config
  { _state :: State Handle
  , _fn    :: TestFn
  }
$(Lens.makeLenses ''Config)

-- We also have an aggregate data type defined to be on par with the model for
-- previous indexers.
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

instance FromField Point where
  fromField f =
    fromField f <&> \p ->
      -- encode -1 as the genesis block.
      if p == -1
         then Genesis
         else Point p

instance ToField Point where
  -- encode -1 as the genesis block.
  toField Genesis   = toField (-1 :: Int)
  toField (Point p) = toField p

deriving newtype instance FromField (StorableResult Handle)

instance FromRow Aggregate where
  fromRow = Aggregate <$> field <*> field

stateId :: Int
stateId = 1

newSqliteIndexer
  :: TestFn
  -> Word
  -> Int
  -> IO (Maybe Config)
newSqliteIndexer accFn memBuf ag0 = do
  h <- Sql.open ":memory:"
  c <- emptyState (fromIntegral memBuf)  (Handle h)
  Sql.execute_ h "DROP TABLE IF EXISTS index_property_tests"
  -- On-disk cache
  Sql.execute_ h "CREATE TABLE index_property_cache (point INTEGER PRIMARY KEY, event INTEGER)"
  -- Aggregated state
  Sql.execute_ h "CREATE TABLE index_property_tests (id INTEGER PRIMARY KEY, accumulator INT)"
  -- Initial state
  Sql.execute  h "INSERT INTO index_property_tests (id, accumulator) VALUES (?, ?)" (stateId, ag0)
  pure . Just $ Config c accFn

instance Buffered Handle where
  persistToStorage :: Foldable f => f (StorableEvent Handle) -> Handle -> IO Handle
  persistToStorage es (Handle h) = do
    -- Append events to cache
    Sql.execute_ h "BEGIN"
    forM_ es $
      Sql.execute h "INSERT INTO index_property_cache (point, event) VALUES (?, ?)"
    Sql.execute_ h "COMMIT"
    pure $ Handle h

  getStoredEvents :: Handle -> IO [StorableEvent Handle]
  getStoredEvents (Handle h) = do
    Sql.query_ h "SELECT * FROM index_property_cache ORDER BY point ASC"

-- Adapt a function coming from the model to a function that works with data from
-- the new indexers.
indexedFn
  :: (Int -> Int -> (Int, Maybe ()))
  -> StorableResult Handle
  -> StorableEvent Handle
  -> (StorableResult Handle, Maybe ())
indexedFn f (Result ag0) (Event _ e) =
  let (r, m) = f ag0 e in
    (Result r, m)

instance Queryable Handle where
  queryStorage
    :: Foldable f
    => QueryInterval (StorablePoint Handle)
    -> f (StorableEvent Handle)
    -> Handle
    -> StorableQuery Handle
    -> IO (StorableResult Handle)
  -- Querying the acumulator only retrieves the value stored in the accumulator table.
  queryStorage _  _ (Handle h) QAccumulator = do
    [ag0] :: [Aggregate] <-
      Sql.query h "SELECT id, accumulator FROM index_property_tests WHERE id = ?"
        (Sql.Only stateId)
    pure $ ag0 ^. aggValue

  queryStorage qi memoryEs (Handle h) (QEvents f)  = do
    Sql.execute_ h "BEGIN"
    -- First we retrieve the accumulator value, by using a proper query.
    aggregate <- queryStorage qi memoryEs (Handle h) QAccumulator
    -- Fetch all events, ordered from oldest to newest.
    es' :: [StorableEvent Handle] <-
      case qi of
        QEverything -> Sql.query_ h "SELECT * from index_property_cache ORDER BY point ASC"
        QInterval _ end ->
          Sql.query h "SELECT * from index_property_cache WHERE point <= ? ORDER BY point ASC" (Sql.Only end)
    Sql.execute_ h "COMMIT"
    -- Filter all events.
    let es'' = filterWithQueryInterval qi (es' ++ toList memoryEs)
    -- Run a fold computing the final result.
    pure $ foldl' ((fst .) . indexedFn f) aggregate es''

instance Rewindable Handle where
  rewindStorage :: StorablePoint Handle -> Handle -> IO (Maybe Handle)
  rewindStorage pt (Handle h) = do
    Sql.execute h "DELETE FROM index_property_cache WHERE point > ?" (Sql.Only pt)
    pure . Just $ Handle h

instance Resumable Handle where
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

-- We don't really have any notification implementation available for the new indexers.
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
          f  = getFunction ix
          sz = ix' ^. state . config . memoryBufferSize
      -- Fetch all stored events.
      es <- Storable.getEvents st
      -- Create a list of QueryIntervals that select each of the stored events
      let qs = map (\p -> QInterval p p)
             $ map sePoint es
      -- And run a fold over all of them, giving us all the historical valus of the
      -- indexer across all the stored values.
      rs  <- fmap getResult <$> mapM (\qi -> Storable.query qi st (QEvents f)) qs
      if null rs
      then do
        -- If there are no results, then return a single element list with the
        -- accumulator.
        Result ag0 <- Storable.query QEverything st QAccumulator
        pure $ Just [ag0]
      else
        -- If there are results, return a list of all the values *without* the
        -- accumulator.
        pure . Just . take sz $ reverse rs

getView
  :: IndexT
  -> PropertyM IO (Maybe (IndexView Int))
getView ix = do
  mix <- run ix
  case mix of
    Nothing       -> pure Nothing
    Just (ix', _) -> do
      let maxSize = ix' ^. state . config . memoryBufferSize + 1
      es <- liftIO $ Storable.getEvents (ix' ^. state)
      Just rs <- getHistory ix
      let sz = if null es then 1 else length es + 1
      -- let sz = if length rs == 1 then 1 else length rs + 1
      pure . Just $
        IndexView { ixDepth = maxSize
                  , ixView  = head rs
                  , ixSize  = min maxSize sz
                  }

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

{-
   This function is needed to lookup a point from the model into the new version of
   indexers. This is needed as rewinds in the model take a number of blocks to rewind
   back, while in the new indexers they take a point that we want to rewind to.

   This is not trivial because it has to respect both the rules surrounding rewinds
   in the original model and the rules surrounding rewinds in the adapter version.
-}
lookupPoint
  :: Int
  -> Config
  -> MaybeT IO (StorablePoint Handle)
lookupPoint n (Config st _) = MaybeT $ do
  let depth = st ^. config . memoryBufferSize + 1
  es' <- reverse <$> Storable.getEvents st
  -- In our model we may get a lot more events than `depth` since we never actually
  -- remove them.
  es  <- take (depth - 1) . reverse <$> Storable.getEvents st
  -- Rewind past all stored events in the model.
  if length es == n
  then do
    if length es' > length es
       -- On this branch we know that we have more than `depth` events stored in the db, so
       -- we do a rewind up to the `depth` + 1 element. This is semantically identical to what
       -- happens in the model.
       then pure . Just $ sePoint $ es' !! n
       -- If we don't have more events stored in the database, then we simply rollback to
       -- Genesis.
       else pure . Just $ Genesis
  else if length es > n
  -- If the point we want to rollback to is less then the size of the list of stored
  -- events, then we want to rollback to the indexed event.
  then pure $ atMay es' n <&> sePoint
  -- Otherwise the point we need to rollback to was not found.
  else pure Nothing

-- The run function returns a tupple because it needs to both assign increasing slot
-- numbers and return the indexer produced by the previous computation.
run
  :: IndexT
  -> PropertyM IO (Maybe (Config, Int))
run (Ix.New f depth ag0)
  | depth <= 0 = pure Nothing
  | otherwise = do
      let d = fromIntegral depth
      -- In the model the K value is always `depth` - 1 to make place for the accumulator.
      -- The second parameter is not really that important.
      indexer <- liftIO $ newSqliteIndexer f (d  - 1) ag0
      -- On creation the slot number will always be 0.
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
      p         <- lookupPoint n ix'
      nextState <- MaybeT . liftIO $ Storable.rewind p (ix' ^. state)
      pure . (,sq) $ ix' { _state = nextState }

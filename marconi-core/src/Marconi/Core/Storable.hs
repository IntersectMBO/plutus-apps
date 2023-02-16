module Marconi.Core.Storable
  ( -- * State
    Config
  , memoryBufferSize
  , State
  , handle
  , config
  , emptyState
  , Storage
  , storage
  , events
  , cursor
  , getMemoryEvents
  , getEvents
  , filterWithQueryInterval
  , StorableEvent
  , StorablePoint
  , StorableQuery
  , StorableResult
  , StorableMonad
    -- * API
  , QueryInterval(..)
  , SyntheticEvent(..)
  , Buffered(..)
  , Queryable(..)
  , Resumable(..)
  , Rewindable(..)
  , HasPoint(..)
  , syntheticPoint
  , foldEvents
  , insert
  , checkpoint
  , insertMany
  , rewind
  , resume
  , query
  ) where

import Control.Lens.Operators ((%~), (.~), (^.))
import Control.Lens.TH qualified as Lens
import Control.Monad (void)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (foldl', foldlM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as VM
import GHC.Generics (Generic)

{-
   The extensible parts of the indexers are the way data is stored into some form
   of persistent storage. The interface used for working blockchain events is defined
   in this module.

   There are quite a bit of type variables that are involed in defining this interface.
   Some of them stand for the type of database connection (h), the type of events (e),
   the type of points along the blockchain (denoted by p), the type of queries (q) and
   results.

   The following data families implement the observation that most of the variables
   can be derived from the way information is stored in the database, so there is
   quite a bit of convenience in reducing the number of type variables to just two.
   One that stands for the database connection (which should be a newtype for each
   indexer) and one for the monad in which the indexer runs (usually IO).
-}
data family StorableEvent h

-- | The resume and query functionality requires a way to specify points on the chain from which we
-- want to resume, or points up to which we want to query.
type family StorablePoint h

data family StorableQuery h

data family StorableResult h

type family StorableMonad h :: * -> *

{-
   Query intervals are a necessary tool to make the queries a little safer. As we can
   assume that there will be multiple concurrent indexers running there is no guarantee
   that they are all synchronised upto the same block, so specifying a query validity
   will ensure that the queries data is acceptably synchronised across all queried
   indexers.
-}
data QueryInterval p =
    QEverything
  | QInterval p p
  deriving (Show, Eq, Generic)

{-
   The first, `Buffered` class explains what it means for an indexer to be accumulating
   a set of results that will be flushed to disk when the memory buffer is fully filled.

   The memory layout of the indexer has been simplified by a lot since the previous
   version and it looks something like this:

   |e|e|e|e|e|e|e|e|e|e|e|e|e|e|e|e|e|e|                                     |
   |-------------------|---------------|-------------------------------------|
   |   memory/buffer   |  disk/events  |       disk/aggregate (optional)     |

   When the buffer is filled with events, they all get flushed to disk. We need this
   operation for performance reasons. Most databases will have significantly improved
   performance for batch inserts.

   Rollbacks will only happen in the memory or disk/events storage area, so the
   developer has to make sure that the allocated space for the buffer and the
   disk/events is greater than the K parameter.

   The last part disk/aggregate represents some aggregated data. At some point the
   developer should write code to aggregate the disk/events into the aggregate
   section of the database. We did not provide any special handling of this for the
   following reasons:

     1) A lot of indexers will not require aggregated data. Only storing events should
        be enough for most applications.
     2) Data aggregation can be implemented at database level and probably be more
        efficient than what Haskell can do here.
     3) We can always extend the interface to include direct support for this pattern
        if there is a demand from the users of the API.
-}
class Buffered h where
  -- | This function persists the memory/buffer events to disk when the memory buffer is filled.
  persistToStorage :: Foldable f => f (StorableEvent h) -> h -> StorableMonad h h

  {- This function retrieves the events from the disk/events area.
     If the user chooses to only store events, without accumulating them, this function
     is expected to return the events over which rollbacks can occur in order to keep
     things performant -}
  getStoredEvents :: h -> StorableMonad h [StorableEvent h]

{-
   All information from indexers should be made accessible through queries. Queries
   act a lot like folds over the event stream. Each query introduces two indexed types:
   one for the type of requests (called StorableQuery) and one for responses (called
   StorableResult).

   All queries include a validity interval. If the data is not available for a specified
   interval, the returned result should specify that. It is also recommended that the result
   includes the slot number at which the query was ran.
-}
class Queryable h where
  queryStorage
    :: Foldable f
    => QueryInterval (StorablePoint h)
    -> f (StorableEvent h)
    -> h
    -> StorableQuery h
    -> StorableMonad h (StorableResult h)

{-
   One of the reasons why indexers were born was to solve one of the issues very specific
   to blockchains: rollbacks. The basic idea was simple, keep a history of the blockchain
   data and whenever we have a rollback restore the version which is the target of the
   rollback.

   This is what the next class is meant to do.
-}
class Rewindable h where
  rewindStorage :: StorablePoint h -> h -> StorableMonad h (Maybe h)

{-
   Another feature of indexers is the ability to resume from a previously stored event.
   One way of implementing this is to make sure that there is always at least one event
   present on disk, event which includes the slot number when it was generated.

   This function should return the most recent resume point. The plan is to support
   multiple resume points, but for now there is no way to ensure that no data duplication
   happens (the same events get reinserted).

   This will not be a problem if the indexer learns how to remove the `old` slots
   so there will be no duplication. This may be implemented later if the API users
   request it.
-}
class Resumable h where
  resumeFromStorage :: h -> StorableMonad h [StorablePoint h]

{-
   The next class is witnessing the fact that events contain enough information to
   retrieve the point when they were produced.
-}
class HasPoint e p where
  getPoint :: e -> p

{-
   The configuration includes hints about the amount of events stored in memory and
   on disk. This information is used by the storage engine to decide how much memory
   to allocated and when to flush the memory buffer or roll the disk events into
   an aggregate database structure.
-}
newtype Config = Config
  { _memoryBufferSize :: Int
  } deriving (Show, Eq)
$(Lens.makeLenses ''Config)

data SyntheticEvent e p =
    Event     !e
  | Synthetic !p

syntheticPoint :: HasPoint e p => SyntheticEvent e p -> p
syntheticPoint (Event e)     = getPoint e
syntheticPoint (Synthetic p) = p

foldEvents :: forall f h. Foldable f => f (SyntheticEvent (StorableEvent h) (StorablePoint h)) -> [StorableEvent h]
foldEvents = reverse . foldl' unwrap []
  where
    unwrap :: [StorableEvent h] -> SyntheticEvent (StorableEvent h) (StorablePoint h) -> [StorableEvent h]
    unwrap acc (Synthetic _) = acc
    unwrap acc (Event e)     = e : acc

data Storage h = Storage
  { _events :: !(VM.MVector (PrimState (StorableMonad h)) (SyntheticEvent (StorableEvent h) (StorablePoint h)))
  , _cursor :: !Int
  }
$(Lens.makeLenses ''Storage)

data State h = State
  { _config  :: !Config
  , _storage :: !(Storage h)
  , _handle  :: !h
  }
$(Lens.makeLenses ''State)

emptyState
  :: PrimMonad (StorableMonad h)
  => Int
  -> h
  -> StorableMonad h (State h)
emptyState memBuf hdl = do
  v <- VM.new memBuf
  pure $ State { _config = Config { _memoryBufferSize = memBuf
                                  }
               , _storage = Storage { _events = v
                                    , _cursor = 0
                                    }
               , _handle = hdl
               }

-- Get events from the memory buffer.
getMemoryEvents
  :: Storage h
  -> V.MVector (PrimState (StorableMonad h)) (SyntheticEvent (StorableEvent h) (StorablePoint h))
getMemoryEvents s = VM.slice 0 (s ^. cursor) (s ^. events)

-- Get events from memory buffer and disk buffer.
getEvents
  :: Buffered h
  => PrimMonad (StorableMonad h)
  => State h
  -> StorableMonad h [StorableEvent h]
getEvents s = do
  memoryEs <- getMemoryEvents (s ^. storage)
              & V.freeze <&> foldEvents . V.toList
  diskEs   <- getStoredEvents (s ^. handle)
  pure $ diskEs ++ memoryEs

{- This function is used to add a checkpoint to the in-memory part of event indexers.
   You can use checkpoints to add synthetic events to the memory buffer. Filling the
   memory buffer with synthetic events will cause the real events to be flushed to
   disk when the number of blocks have been received, regardless of whether they are
   filtered or not.

   Note that the developer will have to use the `checkpoint` function to add synthetic
   events manually whenever the indexer event is filtered out.

   This is useful if the number of events is small and you run the risk of never
   storing them on-disk. When that happens, if the indexer is restarted it will
   resume from the Genesis block, which is something that is not an expected
   behaviour.

   Possible future uses of checkpoints:

   * We currently assume that the indexer has processed blocks up to the latest event
     processed. In case there are very few events this assumption introduces a big
     error. We may eventually use checkpoints to signal to the indexer that the latest
     block is more recent than the latest event processed.

   * Currently synthetic events cannot be stored in the persistent database. We may
     decide to offer this option to implementers if there is interest. This would
     allow for faster resumes if the last stored event is a lot older than the
     currently processed block (similar to the in-memory description of checkpoints).
-}
checkpoint
  :: Buffered h
  => PrimMonad (StorableMonad h)
  => StorablePoint h
  -> State h
  -> StorableMonad h (State h)
checkpoint p s = do
  state'   <- flushBuffer s
  storage' <- appendEvent (Synthetic p) (state' ^. storage)
  pure $ state' { _storage = storage' }

insert
  :: Buffered h
  => PrimMonad (StorableMonad h)
  => StorableEvent h
  -> State h
  -> StorableMonad h (State h)
insert e s = do
  state'   <- flushBuffer s
  storage' <- appendEvent (Event e) (state' ^. storage)
  pure $ state' { _storage = storage' }

appendEvent
  :: PrimMonad (StorableMonad h)
  => SyntheticEvent (StorableEvent h) (StorablePoint h)
  -> Storage h
  -> StorableMonad h (Storage h)
appendEvent e s = do
  let cr = s ^. cursor
  VM.write (s ^. events) cr e
  pure $ s & cursor %~ (+1)

flushBuffer
  :: Buffered h
  => PrimMonad (StorableMonad h)
  => State h
  -> StorableMonad h (State h)
flushBuffer s = do
  let cr = s ^. storage . cursor
      es = getMemoryEvents $ s ^. storage
      mx = s ^. config . memoryBufferSize
  if mx == cr
  then do
    es' <- foldEvents <$> V.freeze es
    h' <- persistToStorage es' (s ^. handle)
    pure $ s & storage . cursor .~ 0
             & handle .~ h'
  else pure s

insertMany
  :: Foldable f
  => Buffered h
  => PrimMonad (StorableMonad h)
  => f (StorableEvent h)
  -> State h
  -> StorableMonad h (State h)
insertMany es s =
  foldlM (\s' e -> insert e s') s es

rewind
  :: forall h.
     Rewindable h
  => HasPoint (StorableEvent h) (StorablePoint h)
  => PrimMonad (StorableMonad h)
  => Ord (StorablePoint h)
  => StorablePoint h
  -> State h
  -> StorableMonad h (Maybe (State h))
rewind p s = if s ^. storage . cursor == 0
  -- Buffer is empty, rewind storage just in case:
  then do
    void $ rewindStorage p (s ^. handle)
    return $ Just s
  -- Something in buffer:
  else do
    v <- V.freeze $ VM.slice 0 (s ^. storage . cursor) (s ^. storage . events)
    case VG.findIndex (\e -> syntheticPoint e > p) v of
      -- All of buffer is later than p, reset memory and rewind storage
      Just 0 -> do
        void $ rewindStorage p (s ^. handle)
        return $ Just $ s & storage . cursor .~ 0
      -- Some of buffer is later than p, truncate memory to that
      Just ix -> return $ Just $ s & storage . cursor .~ ix
      -- No point is larger than p => everything is smaller. Since
      -- buffer was not empty then don't do anything:
      _       -> return $ Just s

resume
  :: Resumable h
  => State h
  -> StorableMonad h [StorablePoint h]
resume s = resumeFromStorage (s ^. handle)

{-
   This function is a bit non-trivial to think about. The question it is trying to
   answer is: Which events are valid for a given query interval.

   The answer is quite a bit more complicated than it seems at first sight. We would
   like to select the latest event within the interval and everything that goes
   before it (so we get a proper history). If we can't find any event less than the
   end, that means that the query interval has filtered all existing events.

   This functionality is important, because it should also be implemented at the
   database level to filter the on-disk events.
-}
filterWithQueryInterval
  :: forall h.
     HasPoint (StorableEvent h) (StorablePoint h)
  => Ord (StorablePoint h)
  => QueryInterval (StorablePoint h)
  -> [StorableEvent h]
  -> [StorableEvent h]
filterWithQueryInterval QEverything es = es
filterWithQueryInterval (QInterval start end) es =
  let es' = takeWhile (withPoint (\p -> p <= end)) es
   in if not (null es') && withPoint (\p -> p >= start) (last es')
      then es'
      else []
  where
    withPoint :: (StorablePoint h -> Bool) -> StorableEvent h -> Bool
    withPoint f e = let p = getPoint e in f p

query
  :: HasPoint (StorableEvent h) (StorablePoint h)
  => Ord (StorablePoint h)
  => Queryable h
  => PrimMonad (StorableMonad h)
  => QueryInterval (StorablePoint h)
  -> State h
  -> StorableQuery h
  -> StorableMonad h (StorableResult h)
query qi s q = do
  es  <- getMemoryEvents (s ^. storage) & V.freeze <&> foldEvents . V.toList
  queryStorage qi (filterWithQueryInterval qi es) (s ^. handle) q

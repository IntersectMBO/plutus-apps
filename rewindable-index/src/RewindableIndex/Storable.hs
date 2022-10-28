module RewindableIndex.Storable
  ( -- * State
    Config
  , memoryBufferSize
  , diskBufferSize
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
    -- * API
  , QueryInterval(..)
  , Buffered(..)
  , Queryable(..)
  , Resumable(..)
  , Rewindable(..)
  , HasPoint(..)
  , insert
  , insertMany
  , rewind
  , resume
  , query
  ) where

import Control.Applicative ((<|>))
import Control.Lens.Operators ((%~), (.~), (^.))
import Control.Lens.TH qualified as Lens
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as VM
import GHC.Generics (Generic)

data family StorableEvent h

data family StorablePoint h

data family StorableQuery h

data family StorableResult h

data QueryInterval p =
    QEverything
  | QInterval p p
  deriving (Show, Eq, Generic)

class Buffered h m where
  persistToStorage :: Foldable f => f (StorableEvent h) -> h -> m h
  getStoredEvents :: h -> m [StorableEvent h]
  trimEventStore :: h -> Int -> m h

class Queryable h m where
  queryStorage
    :: Foldable f
    => QueryInterval (StorablePoint h)
    -> f (StorableEvent h)
    -> h
    -> StorableQuery h
    -> m (StorableResult h)

class Rewindable h m where
  rewindStorage :: StorablePoint h -> h -> m (Maybe h)

class Resumable h m where
  resumeFromStorage :: h -> m [StorablePoint h]

class HasPoint e p where
  getPoint :: e -> p

data Config = Config
  { _memoryBufferSize :: Int
  , _diskBufferSize   :: Int
  } deriving (Show, Eq)
$(Lens.makeLenses ''Config)

data Storage h m = Storage
  { _events :: VM.MVector (PrimState m) (StorableEvent h)
  , _cursor :: Int
  }
$(Lens.makeLenses ''Storage)

data State h m = State
  { _config  :: Config
  , _storage :: Storage h m
  , _handle  :: h
  }
$(Lens.makeLenses ''State)

emptyState
  :: PrimMonad m
  => Int
  -> Int
  -> h
  -> m (State h m)
emptyState memBuf dskBuf hdl = do
  v <- VM.new memBuf
  pure $ State { _config = Config { _memoryBufferSize = memBuf
                                  , _diskBufferSize   = dskBuf
                                  }
               , _storage = Storage { _events = v
                                    , _cursor = 0
                                    }
               , _handle = hdl
               }

getMemoryEvents
  :: Storage h m
  -> V.MVector (PrimState m) (StorableEvent h)
getMemoryEvents s = VM.slice 0 (s ^. cursor) (s ^. events)

getEvents
  :: Buffered h m
  => Show (StorableEvent h)
  => PrimMonad m
  => State h m
  -> m [StorableEvent h]
getEvents s = do
  memoryEs <- getMemoryEvents (s ^. storage)
              & V.freeze <&> V.toList
  diskEs   <- getStoredEvents (s ^. handle)
  pure $ diskEs ++ memoryEs

insert
  :: Buffered h m
  => PrimMonad m
  => StorableEvent h
  -> State h m
  -> m (State h m)
insert e s = do
  state'   <- flushBuffer s
  storage' <- appendEvent e (state' ^. storage)
  pure $ state' { _storage = storage' }

appendEvent
  :: PrimMonad m
  => StorableEvent h
  -> Storage h m
  -> m (Storage h m)
appendEvent e s = do
  let cr = s ^. cursor
  VM.write (s ^. events) cr e
  pure $ s & cursor %~ (+1)

flushBuffer
  :: Buffered h m
  => PrimMonad m
  => State h m
  -> m (State h m)
flushBuffer s = do
  let cr = s ^. storage . cursor
      es = getMemoryEvents $ s ^. storage
      mx = s ^. config . memoryBufferSize
  if mx == cr
  then do
    v  <- V.freeze es
    h' <- persistToStorage v (s ^. handle)
    let storeSize = s ^. config . diskBufferSize
    trimEventStore (s ^. handle) storeSize
    pure $ s & storage . cursor .~ 0
             & handle .~ h'
  else pure s

insertMany
  :: Foldable f
  => Buffered h m
  => PrimMonad m
  => f (StorableEvent h)
  -> State h m
  -> m (State h m)
insertMany es s =
  foldlM (\s' e -> insert e s') s es

rewind
  :: forall h m.
     Rewindable h m
  => HasPoint (StorableEvent h) (StorablePoint h)
  => PrimMonad m
  => Eq (StorablePoint h)
  => StorablePoint h
  -> State h m
  -> m (Maybe (State h m))
rewind p s = do
  m' <- rewindMemory
  h' <- rewindStorage p (s ^. handle)
  pure $ m' <|> resetMemory s <$> h'
  where
    rewindMemory :: m (Maybe (State h m))
    rewindMemory = do
      v <- V.freeze $ VM.slice 0 (s ^. storage . cursor) (s ^. storage . events)
      pure $ do
        ix   <- VG.findIndex (\e -> getPoint e == p) v
        pure $ s & storage . cursor .~ (ix + 1)
    resetMemory :: State h m -> h -> State h m
    resetMemory s' h =
      s' & handle  .~ h
         & storage . cursor .~ 0

resume
  :: Resumable h m
  => State h m
  -> m [StorablePoint h]
resume s = resumeFromStorage (s ^. handle)

-- TODO: This needs explaining.
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
  => Show (StorableEvent h)
  => Show (StorablePoint h)
  => Ord (StorablePoint h)
  => Queryable h m
  => PrimMonad m
  => QueryInterval (StorablePoint h)
  -> State h m
  -> StorableQuery h
  -> m (StorableResult h)
query qi s q = do
  es  <- getMemoryEvents (s ^. storage) & V.freeze <&> V.toList
  queryStorage qi (filterWithQueryInterval qi es) (s ^. handle) q

module RewindableIndex.Storable
  ( -- * State
    Config
  , bufferSize
  , maxRewindLength
  , State
  , handle
  , config
  , emptyState
  , Storage
  , storage
  , events
  , cursor
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
  ) where

import Control.Applicative ((<|>))
import Control.Lens.Operators ((%~), (.~), (^.))
import Control.Lens.TH qualified as Lens
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as VM

data QueryInterval p =
    QEverything
  | QInterval p p

class Buffered c m e where
  persistToStorage :: Foldable f => f e -> c -> m c

class Queryable c p m q r where
  queryStorage :: QueryInterval p -> c -> q -> m r

class Rewindable c m p where
  rewindStorage :: p -> c -> m (Maybe c)

class Resumable c m p where
  resumeFromStorage :: c -> m [p]

class HasPoint e p where
  getPoint :: e -> p

data Config = Config
  { _bufferSize      :: Int
  , _maxRewindLength :: Int
  } deriving (Show, Eq)
$(Lens.makeLenses ''Config)

data Storage p m e = Storage
  { _events :: VM.MVector (PrimState m) e
  , _cursor :: Int
  }
$(Lens.makeLenses ''Storage)

data State h p m e = State
  { _config  :: Config
  , _storage :: Storage p m e
  , _handle  :: h
  }
$(Lens.makeLenses ''State)

emptyState
  :: PrimMonad m
  => Int
  -> Int
  -> h
  -> m (State h p m e)
emptyState bufferSz rewindLen hdl = do
  v <- VM.new bufferSz
  pure $ State { _config = Config { _bufferSize = bufferSz
                                  , _maxRewindLength = rewindLen
                                  }
               , _storage = Storage { _events = v
                                    , _cursor = 0
                                    }
               , _handle = hdl
               }

getBufferedEvents
  :: Storage p m e
  -> V.MVector (PrimState m) e
getBufferedEvents s = VM.slice 0 (s ^. cursor) (s ^. events)

insert
  :: Buffered (State h p m e) m e
  => PrimMonad m
  => e
  -> State h p m e
  -> m (State h p m e)
insert e s = do
  state'   <- flushBuffer s
  storage' <- appendEvent e (state' ^. storage)
  pure $ state' { _storage = storage' }

appendEvent
  :: PrimMonad m
  => e
  -> Storage p m e
  -> m (Storage p m e)
appendEvent e s = do
  let cr = s ^. cursor
  VM.write (s ^. events) cr e
  pure $ s & cursor %~ (+1)

flushBuffer
  :: Buffered (State h p m e) m e
  => PrimMonad m
  => State h p m e
  -> m (State h p m e)
flushBuffer s = do
  let cr = s ^. storage . cursor
      es = getBufferedEvents $ s ^. storage
  if VM.length es == cr
  then do
    v  <- V.freeze es
    s' <- persistToStorage v s
    pure $ s' & storage . cursor .~ 0
  else pure s

insertMany
  :: Foldable f
  => Buffered (State h p m e) m e
  => PrimMonad m
  => f e
  -> State h p m e
  -> m (State h p m e)
insertMany es s =
  foldlM (\s' e -> insert e s') s es

rewind
  :: forall h p m e.
     Rewindable (State h p m e) m p
  => HasPoint e p
  => PrimMonad m
  => Eq p
  => p
  -> State h p m e
  -> m (Maybe (State h p m e))
rewind p s = do
  m' <- rewindMemory
  s' <- rewindStorage p s
  pure $ m' <|> s'
  where
    rewindMemory :: m (Maybe (State h p m e))
    rewindMemory = do
      v <- V.freeze $ s ^. storage . events
      pure $ do
        ix   <- VG.findIndex (\e -> getPoint e == p) v
        pure $ s & storage . cursor .~ (ix + 1)

resume
  :: Resumable (State h p m e) m p
  => State h p m e
  -> m [p]
resume = resumeFromStorage

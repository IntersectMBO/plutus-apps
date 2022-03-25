module Control.Concurrent.STM.TBMQueue
  ( TBMQueue
  , newTBMQueue
  , newTBMQueueIO
  , writeTBMQueue
  , isFullTBMQueue
  , flushTBMQueue
  , sizeTBMQueue
  ) where

import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueue, newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Numeric.Natural (Natural)


-- | 'TBMQueue' is an abstract type representing a bounded FIFO channel with a custom measure function.
-- 'TBMQueue' with a measure function 'const 1' should be equivalent to 'TBQueue'.
data TBMQueue a = TBMQueue !(TQueue a)
                           {-# UNPACK #-} !(TVar Natural) -- Current size
                           !Natural -- Max size
                           !(a -> Natural) -- Measure function

-- | Builds and returns a new instance of 'TBMQueue'.
newTBMQueue :: Natural -> (a -> Natural) -> STM (TBMQueue a)
newTBMQueue maxSize measure = do
  queue <- newTQueue
  currentSize <- newTVar 0
  return $ TBMQueue queue currentSize maxSize measure

-- | 'IO' version of 'newTBMQueue'.
newTBMQueueIO :: Natural -> (a -> Natural) -> IO (TBMQueue a)
newTBMQueueIO maxSize measure = do
  queue <- newTQueueIO
  currentSize <- newTVarIO 0
  return $ TBMQueue queue currentSize maxSize measure

-- | Write a value to a 'TBMQueue'; blocks if the queue is full.
writeTBMQueue :: TBMQueue a -> a -> STM ()
writeTBMQueue (TBMQueue q currentSize maxSize measure) item = do
  size <- readTVar currentSize
  if size > maxSize then retry
  else do
    writeTQueue q item
    writeTVar currentSize (measure item + size)

-- | Returns 'True' if the supplied 'TBMQueue' is full.
isFullTBMQueue :: TBMQueue a -> STM Bool
isFullTBMQueue (TBMQueue _ currentSize maxSize _) = do
  size <- readTVar currentSize
  return $ size > maxSize

-- | Efficiently read the entire contents of a 'TBMQueue' into a list. This
-- function never retries.
flushTBMQueue :: TBMQueue a -> STM [a]
flushTBMQueue (TBMQueue q currentSize _ _) = do
  items <- flushTQueue q
  writeTVar currentSize 0
  return items

-- | Returns current size of the queue.
sizeTBMQueue :: TBMQueue a -> STM Natural
sizeTBMQueue (TBMQueue _ currentSize _ _) = readTVar currentSize

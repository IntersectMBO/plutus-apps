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
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, isEmptyTQueue, newTQueue, newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (when)
import Numeric.Natural (Natural)


-- | 'TBMQueue' is an abstract type representing a bounded FIFO channel with a custom measure function.
-- 'TBMQueue' with a measure function 'const 1' should be equivalent to 'TBQueue'.
data TBMQueue a = TBMQueue !(TQueue a)
                           {-# UNPACK #-} !(TVar Natural) -- Current size
                           {-# UNPACK #-} !(TVar Bool) -- Is the queue full? Is the last item too big?
                           !Natural -- Max size
                           !(a -> Natural) -- Measure function

-- | Builds and returns a new instance of 'TBMQueue'.
newTBMQueue :: Natural -> (a -> Natural) -> STM (TBMQueue a)
newTBMQueue maxSize measure = do
  queue <- newTQueue
  isFull <- newTVar False
  currentSize <- newTVar 0
  return $ TBMQueue queue currentSize isFull maxSize measure

-- | 'IO' version of 'newTBMQueue'.
newTBMQueueIO :: Natural -> (a -> Natural) -> IO (TBMQueue a)
newTBMQueueIO maxSize measure = do
  queue <- newTQueueIO
  isFull <- newTVarIO False
  currentSize <- newTVarIO 0
  return $ TBMQueue queue currentSize isFull maxSize measure

-- | Write a value to a 'TBMQueue'; blocks if the queue is full.
writeTBMQueue :: TBMQueue a -> a -> STM ()
writeTBMQueue (TBMQueue q currentSize isFull maxSize measure) item = do
  full <- readTVar isFull
  if full then retry
  else do
    current <- readTVar currentSize
    let newCurrentSize = measure item + current
    if newCurrentSize <= maxSize then do
      writeTQueue q item
      writeTVar currentSize newCurrentSize
    else do
      isQueueEmpty <- isEmptyTQueue q
      -- if queue is empty and an item is too big to insert
      -- we still insert it alone to avoid an infinite loop
      when isQueueEmpty $ do
        writeTQueue q item
        writeTVar currentSize newCurrentSize
      writeTVar isFull True

-- | Returns 'True' if the supplied 'TBMQueue' is full.
isFullTBMQueue :: TBMQueue a -> STM Bool
isFullTBMQueue (TBMQueue _ _ isFull _ _) = readTVar isFull

-- | Efficiently read the entire contents of a 'TBMQueue' into a list. This
-- function never retries.
flushTBMQueue :: TBMQueue a -> STM [a]
flushTBMQueue (TBMQueue q currentSize isFull _ _) = do
  items <- flushTQueue q
  writeTVar currentSize 0
  writeTVar isFull False
  return items

-- | Returns current size of the queue.
sizeTBMQueue :: TBMQueue a -> STM Natural
sizeTBMQueue (TBMQueue _ currentSize _ _ _) = readTVar currentSize

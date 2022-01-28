
-- | FIXME: @bwbush, outrageously unsafe functions that should be Contract effects.


module Plutus.Contract.Unsafe (
-- * Slot to time configuration.
  setSlotConfig
, getSlotConfig
, unsafeGetSlotConfig
) where


import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Ledger.TimeSlot (SlotConfig (..))
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE slotConfigRef #-}

slotConfigRef :: IORef SlotConfig
slotConfigRef = unsafePerformIO $ newIORef def


setSlotConfig :: SlotConfig
              -> IO ()
setSlotConfig = writeIORef slotConfigRef


getSlotConfig :: IO SlotConfig
getSlotConfig = readIORef slotConfigRef


{-# NOINLINE unsafeGetSlotConfig #-}

unsafeGetSlotConfig :: SlotConfig
unsafeGetSlotConfig = unsafePerformIO getSlotConfig

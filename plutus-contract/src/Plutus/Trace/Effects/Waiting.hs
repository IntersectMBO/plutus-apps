{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Waiting for things to happen
module Plutus.Trace.Effects.Waiting(
    Waiting(..)
    , waitUntilSlot
    , waitUntilTime
    , nextSlot
    , waitNSlots
    , waitNMilliSeconds
    , handleWaiting
    ) where

import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Coroutine (Yield)
import Control.Monad.Freer.TH (makeEffect)
import Ledger.Slot (Slot)
import Ledger.Time (DiffMilliSeconds, POSIXTime, fromMilliSeconds)
import Ledger.TimeSlot qualified as TimeSlot
import Numeric.Natural (Natural)
import Plutus.Trace.Emulator.Types (EmulatorMessage (NewSlot))
import Plutus.Trace.Scheduler (EmSystemCall, Priority (Sleeping), sleep)

data Waiting r where
    WaitUntilSlot :: Slot -> Waiting Slot
    GetSlotConfig :: Waiting TimeSlot.SlotConfig

makeEffect ''Waiting

-- | Wait until the slot where the given time falls into and return latest time
-- we know has passed.
waitUntilTime :: Member Waiting effs => POSIXTime -> Eff effs POSIXTime
waitUntilTime time = do
    slotConfig <- getSlotConfig
    slot <- waitUntilSlot (TimeSlot.posixTimeToEnclosingSlot slotConfig time)
    return $ TimeSlot.slotToEndPOSIXTime slotConfig slot

-- | Wait until the beginning of the next slot, returning
--   the new slot number.
nextSlot :: Member Waiting effs => Eff effs Slot
nextSlot = waitUntilSlot 0

-- | Wait for a number of slots
waitNSlots ::
    forall effs.
    ( Member Waiting effs )
    => Natural
    -> Eff effs Slot
waitNSlots n
    | n > 1 = nextSlot >> waitNSlots (n - 1)
    | otherwise = nextSlot

-- | Convert the given 'n' milliseconds to a number of slots to wait.
--
-- Note: Currently, if n < length of a slot, then 'waitNMilliSeconds' has no
-- effect.
waitNMilliSeconds ::
    forall effs.
    ( Member Waiting effs )
    => DiffMilliSeconds
    -> Eff effs Slot
waitNMilliSeconds n = do
    slotConfig <- getSlotConfig
    waitNSlots (fromIntegral $ TimeSlot.posixTimeToEnclosingSlot slotConfig $ fromMilliSeconds n)

handleWaiting ::
    forall effs effs2 a.
    ( Member (Yield (EmSystemCall effs2 EmulatorMessage a) (Maybe EmulatorMessage)) effs
    )
    => TimeSlot.SlotConfig
    -> Waiting
    ~> Eff effs
handleWaiting slotConfig = \case
    GetSlotConfig -> pure slotConfig
    WaitUntilSlot s -> go where
        go = sleep @effs2 @_ @_ @a Sleeping >>= \case { Just (NewSlot _ sl) | sl >= s -> pure sl; _ -> go }

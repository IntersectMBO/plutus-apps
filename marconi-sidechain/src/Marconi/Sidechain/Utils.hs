module Marconi.Sidechain.Utils where

import Control.Concurrent.STM (STM, TMVar, putTMVar, tryTakeTMVar)

-- | Non-blocking write of a new value to a 'TMVar'
-- Puts if empty. Replaces if populated.
--
-- Only exists in GHC9, but we're on GHC8.
-- TODO: Remove once we migrate to GHC9.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new

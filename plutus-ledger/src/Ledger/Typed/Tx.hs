-- | Typed transaction inputs and outputs. This module defines typed versions
--   of various ledger types. The ultimate goal is to make sure that the script
--   types attached to inputs and outputs line up, to avoid type errors at
--   validation time.
module Ledger.Typed.Tx
  {-# DEPRECATED "Use Plutus.Script.Utils.V1.Typed.Scripts instead" #-}
  ( module Plutus.Script.Utils.V1.Typed.Scripts,
  )
where

import Plutus.Script.Utils.V1.Typed.Scripts

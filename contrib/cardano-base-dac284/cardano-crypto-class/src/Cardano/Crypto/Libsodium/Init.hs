module Cardano.Crypto.Libsodium.Init (
  sodiumInit,
) where

import Control.Monad (unless)

import Cardano.Crypto.Libsodium.C

-- @sodiumInit@ initializes the library and should be called before any other
-- function provided by Sodium. It is safe to call this function more than once
-- and from different threads -- subsequent calls won't have any effects.
--
-- <https://libsodium.gitbook.io/doc/usage>
sodiumInit :: IO ()
sodiumInit = do
    res <- c_sodium_init
    -- sodium_init() returns 0 on success, -1 on failure, and 1 if the library
    -- had already been initialized.
    unless (res == 0 || res == 1) $ fail "sodium_init failed"



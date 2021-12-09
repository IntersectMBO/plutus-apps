{-# LANGUAGE CApiFFI             #-}
module Cardano.Crypto.Libsodium.UnsafeC (
    c_sodium_compare_unsafe,
    ) where

import Foreign.C.Types (CSize (..))
import Foreign.Ptr (Ptr)

-- | Unsafe variant of 'c_sodium_compare'.
foreign import capi unsafe "sodium.h sodium_compare" c_sodium_compare_unsafe :: Ptr a -> Ptr a -> CSize -> IO Int

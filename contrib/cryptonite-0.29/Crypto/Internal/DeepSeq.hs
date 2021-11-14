-- |
-- Module      : Crypto.Internal.DeepSeq
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Simple abstraction module to allow compilation without deepseq
-- by defining our own NFData class if not compiling with deepseq
-- support.
--
{-# LANGUAGE CPP #-}
module Crypto.Internal.DeepSeq
    ( NFData(..)
    ) where

#ifdef WITH_DEEPSEQ_SUPPORT
import Control.DeepSeq
#else
import Data.Word
import Data.ByteArray

class NFData a where rnf :: a -> ()

instance NFData Word8 where rnf w = w `seq` ()
instance NFData Word16 where rnf w = w `seq` ()
instance NFData Word32 where rnf w = w `seq` ()
instance NFData Word64 where rnf w = w `seq` ()

instance NFData Bytes where rnf b = b `seq` ()
instance NFData ScrubbedBytes where rnf b = b `seq` ()

instance NFData Integer  where rnf i = i `seq` ()

#endif

{-

Define timeouts for IO actions

-}
module Plutus.PAB.Timeout (Timeout(..), startTimeout) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TMVar)
import Control.Concurrent.STM qualified as STM
import Data.Default (Default (..))
import Data.Foldable (traverse_)
import Data.Time.Units (Second, toMicroseconds)

newtype Timeout = Timeout { unTimeout :: Maybe Second }

instance Default Timeout where
    def = Timeout Nothing

-- | Create a 'TMVar' that is filled when the timeout expires. If the timeout
--   is 'Nothing', the 'TMVar' is never filled.
startTimeout :: Timeout -> IO (TMVar ())
startTimeout (Timeout t) = do
    tmv <- STM.newEmptyTMVarIO
    flip traverse_ t $ \s -> do
        forkIO $ do
            threadDelay $ fromIntegral $ toMicroseconds s
            STM.atomically $ STM.putTMVar tmv ()
    pure tmv

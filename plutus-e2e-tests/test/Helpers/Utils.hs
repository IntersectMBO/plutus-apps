module Helpers.Utils where

import Cardano.Api qualified as C
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Data.Time.Clock.POSIX qualified as Time
import GHC.Stack qualified as GHC
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as HE
import Hedgehog.Extras.Stock.CallStack qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO

-- | Right from Either or throw Left error
unsafeFromRight :: Show l => Either l r -> r
unsafeFromRight (Left err)    = error (show err)
unsafeFromRight (Right value) = value

-- | Maybe throw error
unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe Nothing  = error "not just, nothing."
unsafeFromMaybe (Just a) = a

-- | This is a copy of the workspace from
-- hedgehog-extras:Hedgehog.Extras.Test.Base, which for darwin sets
-- the systemTemp folder to /tmp.
--
-- It creates a temporary folder with @prefixPath@, which is removed
-- after the supplied function @f@ returns.
workspace :: (MonadTest m, MonadIO m, GHC.HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- case IO.os of
    "darwin" -> pure "/tmp"
    _        -> H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  let systemPrefixPath = systemTemp <> "/" <> prefixPath
  H.evalIO $ IO.createDirectoryIfMissing True systemPrefixPath
  ws <- H.evalIO $ IO.createTempDirectory systemPrefixPath "test"
  H.annotate $ "Workspace: " <> ws
  liftIO $ IO.writeFile (ws <> "/module") H.callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    H.evalIO $ IO.removeDirectoryRecursive ws

-- | Read file text envelope as a specific type (e.g. C.VerificationKey C.GenesisUTxOKey)
--   and throw error on failure
readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = do
  path' <- H.note path
  H.leftFailM . liftIO $ C.readFileTextEnvelope as path'

-- | Same as readAs but return Nothing on error
maybeReadAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m (Maybe a)
maybeReadAs as path = do
  path' <- H.note path
  maybeEither . liftIO $ C.readFileTextEnvelope as path'
  where
    maybeEither m = m >>= return . either (const Nothing) Just

-- | Convert a 'POSIXTime' to the number of milliseconds since the Unix epoch.
posixToMilliseconds :: Time.POSIXTime -> Integer
posixToMilliseconds posixTime = round $ 1000 * (realToFrac posixTime :: Double)

-- | Fails any Left returning Rights.
anyLeftFail :: (MonadTest m, Show e) => m [Either e a] -> m [a]
anyLeftFail es = mapM (HE.leftFailM . return) =<< es

-- | Fails any Left returning unit.
anyLeftFail_ :: (MonadTest m, Show e) => m [Either e a] -> m ()
anyLeftFail_ = void . anyLeftFail

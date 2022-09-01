module RewindableIndex.Spec.VSplit where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive (PrimMonad)
import Data.Default (Default)
import Data.List (tails)
import Data.Maybe (catMaybes)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, monadicIO)

import Control.Lens ((^.))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM

import RewindableIndex.Index.VSplit (SplitIndex)
import RewindableIndex.Index.VSplit qualified as S
import RewindableIndex.Model (Conversion (Conversion, cHistory, cMonadic, cNotifications, cView), Index,
                              IndexView (IndexView, ixDepth, ixSize, ixView))
import RewindableIndex.Model qualified as Ix

conversion
  :: Show s
  => Show e
  => Show n
  => Default s
  => Conversion (PropertyM IO) s e n
conversion = Conversion
  { cView          = view
  , cHistory       = history
  , cNotifications = notifications
  , cMonadic       = monadic
  }

getNotifications
  :: SplitIndex m h v e n q r
  -> [n]
getNotifications ix = ix ^. S.notifications

getHistory
  :: PrimMonad m
  => VGM.MVector (VG.Mutable v) e
  => Show e
  => SplitIndex m h v e n q r
  -> q
  -> m [r]
getHistory ix q = do
  es <- S.getEvents (ix ^. S.storage)
  traverse ((ix ^. S.query) ix q) $ tails es

getView
  :: PrimMonad m
  => VGM.MVector (VG.Mutable v) e
  => Show e
  => SplitIndex m h v e n q r
  -> q
  -> m (IndexView r)
getView ix q = do
  hs <- getHistory ix q
  pure $ IndexView { ixDepth = ix ^. S.storage . S.k + 1
                   , ixView  = head hs
                   , ixSize  = S.size ix
                   }
view
  :: (Show s, Show e, Show n, Default s)
  => Index s e n
  -> PropertyM IO (Maybe (IndexView s))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      v <- getView ix' ()
      pure $ Just v

notifications
  :: (Show a, Show e, Show n, Default a)
  => Index a e n
  -> PropertyM IO [n]
notifications ix = do
  -- We should never call this on invalid indexes.
  Just ix' <- run ix
  pure $ getNotifications ix'

history
  :: (Show s, Show e, Show n, Default s)
  => Index s e n
  -> PropertyM IO (Maybe [s])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      h <- getHistory ix' ()
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

run
  :: forall s e n. (Show s, Show e, Show n, Default s)
  => Index s e n
  -> PropertyM IO (Maybe (SplitIndex IO (MVar s) V.Vector e n () s))
run (Ix.New f depth store) = do
  let k' = depth - 1
  liftIO $ do
    mstore <- newMVar store
    S.newBoxed fquery fstore foninsert k' ((k' + 1) * 2) mstore
  where
    fquery :: SplitIndex IO (MVar s) V.Vector e n () s -> () -> [e] -> IO s
    fquery ix () es = do
      oldState <- readMVar $ ix ^. S.handle
      bufferedEvents <- S.getBuffer $ ix ^. S.storage
      pure . fst $ foldr convertIxF (oldState, []) (es ++ bufferedEvents)
    fstore  :: SplitIndex IO (MVar s) V.Vector e n () s -> IO ()
    fstore ix = do
      newState <- fquery ix () []
      _ <- swapMVar (ix ^. S.handle) newState
      pure ()
    foninsert :: SplitIndex IO (MVar s) V.Vector e n () s -> e -> IO [n]
    foninsert ix e = do
      es <- S.getEvents $ ix ^. S.storage
      oldState <- fquery ix () es
      pure $ catMaybes [snd $ f oldState e]
    convertIxF :: e -> (s, [n]) -> (s, [n])
    convertIxF e (a', ns) =
      let (a'', mn) = f a' e
       in (a'', catMaybes [mn] ++ ns)
run (Ix.Insert e ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      nix <- S.insert e ix'
      pure $ Just nix
run (Ix.Rewind n ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO . pure $ S.rewind n ix'


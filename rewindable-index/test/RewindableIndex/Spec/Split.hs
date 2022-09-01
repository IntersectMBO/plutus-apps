module RewindableIndex.Spec.Split where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq, (><))
import Data.Sequence qualified as Seq
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, monadicIO)

import RewindableIndex.Index.Split (SplitIndex (SplitIndex, siBuffered, siDepth, siEvents, siHandle, siNotifications, siQuery),
                                    size)
import RewindableIndex.Index.Split qualified as S
import RewindableIndex.Model (Conversion (Conversion, cHistory, cMonadic, cNotifications, cView), Index,
                              IndexView (IndexView, ixDepth, ixSize, ixView))
import RewindableIndex.Model qualified as Ix

conversion :: (Show s, Show e, Show n, Default s) => Conversion (PropertyM IO) s e n
conversion = Conversion
  { cView          = view
  , cHistory       = history
  , cNotifications = notifications
  , cMonadic       = monadic
  }

view' :: (Monad m, MonadFail m) => q -> SplitIndex m h e n q r -> m (IndexView r)
view' query ix@SplitIndex{siDepth} = do
  h : _ <- getHistory' query ix
  pure $ IndexView { ixDepth = siDepth
                   , ixView  = h
                   , ixSize  = size ix
                   }

getNotifications' :: Monad m => SplitIndex m h e n q r -> m [n]
getNotifications' SplitIndex{siNotifications} = pure siNotifications

getHistory' :: forall m h e n q r. Monad m => q -> SplitIndex m h e n q r -> m [r]
getHistory' query ix@SplitIndex{siQuery, siEvents} = do
  xs <- traverse (siQuery ix query) $ Seq.tails siEvents
  pure $ toList xs

view
  :: (Show s, Show e, Show n, Default s)
  => Index s e n
  -> PropertyM IO (Maybe (IndexView s))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      v <- view' () ix'
      pure $ Just v

notifications
  :: (Show a, Show e, Show n, Default a)
  => Index a e n
  -> PropertyM IO [n]
notifications ix = do
  -- We should never call this on invalid indexes.
  Just ix' <- run ix
  liftIO $ getNotifications' ix'

history
  :: (Show s, Show e, Show n, Default s)
  => Index s e n
  -> PropertyM IO (Maybe [s])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      h <- getHistory' () ix'
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

{- | TODO: Make the case why this interpretation tests something useful.
-}
run
  :: forall s e n. (Show s, Show e, Show n, Default s)
  => Index s e n
  -> PropertyM IO (Maybe (SplitIndex IO (MVar s) e n () s))
run (Ix.New f depth store) = do
  liftIO $ do
    mstore <- newMVar store
    S.new fquery foninsert fstore depth mstore
  where
    fquery :: SplitIndex IO (MVar s) e n () s -> () -> Seq e -> IO s
    fquery SplitIndex{siHandle, siBuffered} () es = do
      oldState <- readMVar siHandle
      pure . fst $ foldr convertIxF (oldState, []) (es >< siBuffered)
    fstore  :: SplitIndex IO (MVar s) e n () s -> IO ()
    fstore ix@SplitIndex{siHandle} = do
      newState <- fquery ix () Seq.empty
      _ <- swapMVar siHandle newState
      pure ()
    foninsert :: e -> SplitIndex IO (MVar s) e n () s -> IO [n]
    foninsert e ix@SplitIndex{siEvents} = do
      oldState <- fquery ix () siEvents
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


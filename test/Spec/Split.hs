{-# LANGUAGE NamedFieldPuns #-}

module Spec.Split where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import           Data.Maybe              (catMaybes)
import           Test.QuickCheck         (Property)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO)

import           Index                   (Index, IndexView (..))
import qualified Index                   as Ix
import           Index.Split             (SplitIndex (..))
import qualified Index.Split             as S
import           Spec.Index              (Conversion (..))

conversion :: (Show s, Show e, Show n) => Conversion (PropertyM IO) s e n
conversion = Conversion
  { cView          = view
  , cHistory       = history
  , cNotifications = notifications
  , cMonadic       = monadic
  }

view
  :: (Show s, Show e, Show n)
  => Index s e n
  -> PropertyM IO (Maybe (IndexView s))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      v <- S.view ix'
      pure $ Just v

notifications
  :: (Show a, Show e, Show n)
  => Index a e n
  -> PropertyM IO [n]
notifications ix = do
  -- We should never call this on invalid indexes.
  Just ix' <- run ix
  liftIO $ S.getNotifications ix'


history
  :: (Show a, Show e, Show n)
  => Index a e n
  -> PropertyM IO (Maybe [a])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      h <- S.getHistory ix'
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

run
  :: forall s e n. (Show s, Show e, Show n)
  => Index s e n
  -> PropertyM IO (Maybe (SplitIndex IO (MVar s) s e n))
run (Ix.New f depth store) = do
  liftIO $ do
    mstore <- newMVar store
    S.new findex fstore fload depth mstore
  where
    findex :: s -> [e] -> (s, [n])
    findex s es = foldr convertIxF (s, []) es
    fstore :: MVar s -> s -> IO ()
    fstore mv s = swapMVar mv s >> pure ()
    fload  :: MVar s -> IO s
    fload  = readMVar
    convertIxF :: e -> (s, [n]) -> (s, [n])
    convertIxF e (a', ns) =
      let (a'', mn) = f a' e
       in (a'', catMaybes [mn] ++ ns)
run (Ix.Insert e ix) = do
  mix <- run ix
  case  mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO $ do
      nix <- S.insert e ix'
      pure $ Just nix
run (Ix.Rewind n ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> liftIO . pure $ S.rewind n ix'


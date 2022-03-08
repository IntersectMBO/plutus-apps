{-# LANGUAGE NamedFieldPuns #-}

module Spec.Split where

import           Control.Monad           ((>=>))
import           Data.Foldable           (foldl')
import           Data.Functor            ((<&>))
import           Test.QuickCheck         (Property)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO)

import           Index                   (Index, IndexView (..))
import qualified Index                   as Ix
import           Index.Split             (SplitIndex (..))
import qualified Index.Split             as S
import           Spec.Index              (Conversion (..))

conversion :: Conversion (PropertyM IO) a e
conversion = Conversion
  { cView    = view
  , cHistory = history
  , cMonadic = monadic
  }

view
  :: Index a e
  -> PropertyM IO (Maybe (IndexView a))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      v <- S.view ix'
      pure $ Just v

history
  :: Index a e
  -> PropertyM IO (Maybe [a])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      h <- S.getHistory ix'
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

run
  :: forall m a e. Monad m
  => Index a e
  -> m (Maybe (SplitIndex m a e))
run (Ix.New f d a) = pure $ S.new findex fstore d (pure a)
  where
    findex :: a -> [e] -> a
    findex a' es = foldl' f a' es
    fstore :: a -> m a
    fstore a' = pure a'
run (Ix.Insert e ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> Just <$> S.insert e ix'
run (Ix.Rewind n ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> pure $ S.rewind n ix'

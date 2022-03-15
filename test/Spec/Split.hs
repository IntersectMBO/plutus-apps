{-# LANGUAGE NamedFieldPuns #-}

module Spec.Split where

import           Data.Foldable           (foldl')
import           Test.QuickCheck         (Property)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO)

import           Index                   (Index, IndexView (..))
import qualified Index                   as Ix
import           Index.Split             (SplitIndex (..))
import qualified Index.Split             as S
import           Spec.Index              (Conversion (..))

import qualified Debug.Trace as Debug

conversion :: (Show a, Show e) => Conversion (PropertyM IO) a e
conversion = Conversion
  { cView    = view
  , cHistory = history
  , cMonadic = monadic
  }

view
  :: (Show a, Show e)
  => Index a e
  -> PropertyM IO (Maybe (IndexView a))
view ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      v <- S.view ix'
      pure $ Just v

history
  :: (Show a, Show e)
  => Index a e
  -> PropertyM IO (Maybe [a])
history ix = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      h' <- S.getHistory ix'
      -- h <- Debug.trace ("Getting history of " <> show ix' <> " as " <> show h') $ S.getHistory ix'
      h <- S.getHistory ix'
      pure $ Just h

monadic
  :: PropertyM IO Property
  -> Property
monadic = monadicIO

run
  :: forall m a e. (Show a, Show e, Monad m)
  => Index a e
  -> m (Maybe (SplitIndex m a e))
run ix@(Ix.New f d a) = 
  let nix = S.new findex fstore d (pure a)
   -- in Debug.trace ("\n Result of interpreting " <> show ix <> " => " <> show nix <> "\n") $ pure $ S.new findex fstore d (pure a)
   in pure $ S.new findex fstore d (pure a)
  where
    findex :: a -> [e] -> a
    -- findex a' es = foldl' f a' es
    findex a' es = foldr (flip f) a' es
    fstore :: a -> m a
    fstore a' = pure a'
run ix0@(Ix.Insert e ix) = do
  mix <- run ix
  case  mix of
    Nothing  -> pure Nothing
    Just ix' -> do
      nix <- S.insert e ix'
      -- Debug.trace ("Result of interpreting " <> show ix0 <> " => " <> show nix <> "\n") $ Just <$> pure nix
      pure $ Just nix
run ix0@(Ix.Rewind n ix) = do
  mix <- run ix
  case mix of
    Nothing  -> pure Nothing
    -- Just ix' -> pure $ S.rewind n ix'
    Just ix' -> do
      let nix = S.rewind n ix'
      -- Debug.trace ("Result of interpreting " <> show ix0 <> " => " <> show nix <> "\n") $ pure $ S.rewind n ix'
      pure $ S.rewind n ix'


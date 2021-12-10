{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Gauge.Monad
-- Copyright   : (c) 2009 Neil Brown
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The environment in which most gauge code executes.
module Gauge.Monad
    (
      Gauge
    , Crit (..)
    , askCrit
    , askConfig
    , gaugeIO
    , withConfig
    , finallyGauge
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad (ap)
import Data.IORef (IORef, newIORef)
import Gauge.Main.Options (Config)
import Gauge.Measurement (initializeTime)
import System.Random.MWC (GenIO)
import Prelude -- Silence redundant import warnings

data Crit = Crit
    { config   :: !Config
    , gen      :: !(IORef (Maybe GenIO))
    }

-- | 'Gauge' is essentially a reader monad to make the benchmark configuration
-- available throughout the code.
newtype Gauge a = Gauge { runGauge :: Crit -> IO a }

instance Functor Gauge where
    fmap f a = Gauge $ \r -> f <$> runGauge a r
instance Applicative Gauge where
    pure = Gauge . const . pure
    (<*>) = ap
instance Monad Gauge where
    return    = pure
    ma >>= mb = Gauge $ \r -> runGauge ma r >>= \a -> runGauge (mb a) r

-- | Retrieve the configuration from the 'Gauge' monad.
askConfig :: Gauge Config
askConfig = Gauge (pure . config)

askCrit :: Gauge Crit
askCrit = Gauge pure

-- | Lift an IO action into the 'Gauge' monad.
gaugeIO :: IO a -> Gauge a
gaugeIO = Gauge . const

finallyGauge :: Gauge a -> Gauge b -> Gauge a
finallyGauge f g = Gauge $ \crit -> do
    finally (runGauge f crit) (runGauge g crit)

-- | Run a 'Gauge' action with the given 'Config'.
withConfig :: Config -> Gauge a -> IO a
withConfig cfg act = do
  initializeTime
  g <- newIORef Nothing
  runGauge act (Crit cfg g)

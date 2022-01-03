{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Plutus.Contract.Test.ContractModel.CrashTolerance
  ( -- * Extending contract models with a model of
    -- crashing and restarting contracts
    WithCrashTolerance
  , CrashTolerance(..)
  ) where

import Control.Lens
import Control.Monad.State
import Data.Functor.Compose
import Data.Typeable
import Plutus.Contract.Test.ContractModel.Internal
import Plutus.Trace.Effects.EmulatorControl
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types
import Test.QuickCheck as QC

-- | This derived state is used to derive a new `ContractModel` on top of the `state` contract model
-- that also specifies how the contract(s) behave when contract instances crash and restart.
data WithCrashTolerance state = WithCrashTolerance { _underlyingModelState   :: state
                                                   , _aliveContractInstances :: [ContractInstanceSpec state]
                                                   , _deadContractInstances  :: [ContractInstanceSpec state] }
makeLenses ''WithCrashTolerance

deriving instance (Show state, Show (ContractInstanceSpec state)) => Show (WithCrashTolerance state)
deriving instance (Eq state, Eq (ContractInstanceSpec state)) => Eq (WithCrashTolerance state)

class ContractModel state => CrashTolerance state where
  -- | Specifiy what happens when a contract instance crashes
  crash :: ContractInstanceSpec state -> Spec state ()
  crash _ = return ()
  -- | Specify what happens when a contract instance is restarted
  restart :: ContractInstanceSpec state -> Spec state ()
  restart _ = return ()
  -- | Check if an action is available given a list of alive
  -- contract instances.
  available :: Action state -> [ContractInstanceSpec state] -> Bool

instance (Show (ContractInstanceSpec state), Show (Action state)) => Show (Action (WithCrashTolerance state)) where
  showsPrec p (Crash cis)          = showParen (p >= 11) $ showString "Crash " . showsPrec 11 cis
  showsPrec p (Restart cis)        = showParen (p >= 11) $ showString "Restart " . showsPrec 11 cis
  showsPrec p (UnderlyingAction a) = showsPrec p a
deriving instance (Typeable state, forall w s e. Eq (ContractInstanceKey state w s e), Eq (Action state)) => Eq (Action (WithCrashTolerance state))

deriving instance Show (ContractInstanceKey state w s e) => Show (ContractInstanceKey (WithCrashTolerance state) w s e)
deriving instance Eq (ContractInstanceKey state w s e) => Eq (ContractInstanceKey (WithCrashTolerance state) w s e)

liftContractInstance :: ContractInstanceSpec state -> ContractInstanceSpec (WithCrashTolerance state)
liftContractInstance (ContractInstanceSpec k w c) = ContractInstanceSpec (UnderlyingContractInstanceKey k) w c

lowerContractInstance :: ContractInstanceSpec (WithCrashTolerance state) -> ContractInstanceSpec state
lowerContractInstance (ContractInstanceSpec (UnderlyingContractInstanceKey k) w c) = ContractInstanceSpec k w c

instance ( Typeable state
         , Show (ContractInstanceSpec state)
         , Eq (ContractInstanceSpec state)
         , CrashTolerance state) => ContractModel (WithCrashTolerance state) where

  data Action (WithCrashTolerance state) = Crash (ContractInstanceSpec state)
                                         | Restart (ContractInstanceSpec state)
                                         | UnderlyingAction (Action state)

  data ContractInstanceKey (WithCrashTolerance state) w s e where
    UnderlyingContractInstanceKey :: ContractInstanceKey state w s e -> ContractInstanceKey (WithCrashTolerance state) w s e

  initialState = WithCrashTolerance initialState initialHandleSpecs []

  initialHandleSpecs = liftContractInstance <$> initialHandleSpecs

  -- We piggy-back on the underlying mechanism for starting contract instances that we
  -- get from
  startInstances _ (Restart cis)        = [liftContractInstance cis]
  startInstances s (UnderlyingAction a) = liftContractInstance <$> startInstances (_underlyingModelState <$> s) a
  startInstances _ _                    = []

  perform h s a = case a of
    Crash (ContractInstanceSpec key _ _) -> do
      -- I'm not sure why this has to take two slots but if you don't make it take
      -- two slots the crash doesn't happen if its the first action
      Trace.waitNSlots 1
      -- This turns out to be enough. Restarting a contract instance overrides the handle
      -- for the contract instance and the existing instance becomes garbage. This does
      -- leak memory, but only relatively little and only during a test.
      freezeContractInstance . chInstanceId $ h (UnderlyingContractInstanceKey key)
      void $ Trace.waitNSlots 1
    Restart _ -> do
      void $ Trace.waitNSlots 1
    UnderlyingAction a -> do
      perform (h . UnderlyingContractInstanceKey) (_underlyingModelState <$> s) a

  precondition s a = case a of
    -- Only crash alive contract instances
    Crash cis -> cis `elem` (s ^. contractState . aliveContractInstances)
    -- Only restart dead contract instances
    Restart cis -> cis `elem` (s ^. contractState . deadContractInstances)
    -- Run an underlying action if its available
    UnderlyingAction a -> precondition (_underlyingModelState <$> s) a
                       && available a (s ^. contractState . aliveContractInstances)

  nextState a = case a of
    Crash cis -> do
      embed $ crash cis
      deadContractInstances %= (cis:)
      aliveContractInstances %= filter (/=cis)
      wait 2
    Restart cis -> do
      embed $ restart cis
      deadContractInstances %= filter (/=cis)
      aliveContractInstances %= (cis:)
      wait 1
    UnderlyingAction a -> do
      embed $ nextState a
      s <- Spec get
      -- An action may start its own contract instances and we need to keep track of them
      aliveContractInstances %= ((lowerContractInstance <$> startInstances s (UnderlyingAction a)) ++)
    where
      embed :: Spec state a -> Spec (WithCrashTolerance state) a
      embed (Spec comp) = Spec (zoom (liftL _contractState underlyingModelState) comp)

  arbitraryAction s = frequency [ (10, UnderlyingAction <$> arbitraryAction (_underlyingModelState <$> s))
                                , (1, Crash <$> QC.elements (s ^. contractState . aliveContractInstances))
                                , (1, Restart <$> QC.elements (s ^. contractState . deadContractInstances)) ]

  shrinkAction s (UnderlyingAction a) = UnderlyingAction <$> shrinkAction (_underlyingModelState <$> s) a
  shrinkAction _ _                    = []

liftL :: Functor t => (forall a. t a -> a) -> Lens' s a -> Lens' (t s) (t a)
liftL extr l ft ts = getCompose . l (Compose . ft . (<$ ts)) $ extr ts


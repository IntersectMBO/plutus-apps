{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Freer.Extras.Modify (
    -- * change the list of effects
    mapEffs

    -- * under functions
    , UnderN(..)
    , under

    -- * weaken functions
    , CanWeakenEnd(..)
    , weakenUnder
    , weakenNUnder
    , weakenMUnderN

    -- * raise functions
    , raiseEnd
    , raiseUnder
    , raiseUnder2
    , raise2Under
    , raiseNUnder
    , raiseMUnderN

    -- * zoom functions
    , handleZoomedState
    , handleZoomedError
    , handleZoomedWriter
    , handleZoomedReader

    -- * manipulation
    , writeIntoState
    , stateToMonadState
    , monadStateToState
    , errorToMonadError
    , wrapError
    ) where

import Control.Lens hiding (under)
import Control.Monad.Except qualified as MTL
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Control.Monad.State qualified as MTL

mapEffs :: (Union effs ~> Union effs') -> Eff effs ~> Eff effs'
mapEffs f = loop where
    loop = \case
        Val a -> pure a
        E u q -> E (f u) (tsingleton $ qComp q loop)


under :: (Union effs ~> Union effs') -> Union (a ': effs) ~> Union (a ': effs')
under f u = case decomp u of
    Left u' -> weaken (f u')
    Right t -> inj t

class UnderN as where
    underN :: (Union effs ~> Union effs') -> Union (as :++: effs) ~> Union (as :++: effs')
instance UnderN '[] where
    underN f = f
instance UnderN as => UnderN (a ': as) where
    underN f = under (underN @as f)


{- Note [Various raising helpers]
These are all to help with the issue where you have something of type

Eff effs a

where effs is some *fixed* list of effects. You may then need to insert
more effects *under* effs to interpret them in terms of. It turns out that
inserting effects at the *end* of the list is tricky.

I have no idea what I'm doing, these are partially stolen from freer-simple/polysemy
with a lot of hacking around.

The first instance of CanWeakenEnd is for the case where the fixed list has length 1.
The second instance is for cases where the fixed list has a length of 2 or more,
hence the double cons in the types to prevent overlap with the first instance.
-}
class CanWeakenEnd as effs where
    weakenEnd :: Union as ~> Union effs
instance effs ~ (a ': effs') => CanWeakenEnd '[a] effs where
    weakenEnd u = inj (extract u)
instance (effs ~ (a ': effs'), CanWeakenEnd (b ': as) effs') => CanWeakenEnd (a ': b ': as) effs where
    weakenEnd = under weakenEnd

weakenUnder :: forall effs a b . Union (a ': effs) ~> Union (a ': b ': effs)
weakenUnder = under weaken

weakenNUnder :: forall effs' effs a . Weakens effs' => Union (a ': effs) ~> Union (a ': (effs' :++: effs))
weakenNUnder = under (weakens @effs' @effs)

-- basically applies `under` n times to `weaken` composed m times, n = length as, m = length effs'
weakenMUnderN :: forall effs' as effs . (UnderN as, Weakens effs') => Union (as :++: effs) ~> Union (as :++: (effs' :++: effs))
weakenMUnderN = underN @as (weakens @effs' @effs)


raiseEnd :: forall effs as. CanWeakenEnd as effs => Eff as ~> Eff effs
raiseEnd = mapEffs weakenEnd

raiseUnder :: forall effs a b . Eff (a ': effs) ~> Eff (a ': b ': effs)
raiseUnder = mapEffs weakenUnder

raiseUnder2 :: forall effs a b c . Eff (a ': b ': effs) ~> Eff (a ': b ': c ': effs)
raiseUnder2 = mapEffs (under $ under weaken)

raise2Under :: forall effs a b c . Eff (a ': effs) ~> Eff (a ': b ': c ': effs)
raise2Under = mapEffs (under $ weaken . weaken)

raiseNUnder :: forall effs' effs a . Weakens effs' => Eff (a ': effs) ~> Eff (a ': (effs' :++: effs))
raiseNUnder = mapEffs (weakenNUnder @effs' @effs @a)

-- | Raise m effects under the top n effects
raiseMUnderN :: forall effs' as effs . (UnderN as, Weakens effs') => Eff (as :++: effs) ~> Eff (as :++: (effs' :++: effs))
raiseMUnderN = mapEffs (weakenMUnderN @effs' @as @effs)


-- | Handle a 'State' effect in terms of a "larger" 'State' effect from which we have a lens.
handleZoomedState :: Member (State s2) effs => Lens' s2 s1 -> (State s1 ~> Eff effs)
handleZoomedState l = \case
    Get   -> view l <$> get
    Put v -> modify (set l v)

-- | Handle a 'Writer' effect in terms of a "larger" 'Writer' effect from which we have a review.
handleZoomedWriter :: Member (Writer s2) effs => AReview s2 s1 -> (Writer s1 ~> Eff effs)
handleZoomedWriter p = \case
    Tell w -> tell (review p w)

-- | Handle an 'Error' effect in terms of a "larger" 'Error' effect from which we have a review.
handleZoomedError :: Member (Error s2) effs => AReview s2 s1 -> (Error s1 ~> Eff effs)
handleZoomedError p = \case
    Error e -> throwError (review p e)

-- | Handle a 'Reader' effect in terms of a "larger" 'Reader' effect from which we have a getter.
handleZoomedReader :: Member (Reader s2) effs => Getter s2 s1 -> (Reader s1 ~> Eff effs)
handleZoomedReader g = \case
    Ask -> view g <$> ask

-- | Handle a 'Writer' effect in terms of a "larger" 'State' effect from which we have a setter.
writeIntoState
    :: (Monoid s1, Member (State s2) effs)
    => Setter' s2 s1
    -> (Writer s1 ~> Eff effs)
writeIntoState s = \case
    Tell w -> modify (\st -> st & s <>~ w)

-- | Handle a 'State' effect in terms of a monadic effect which has a 'MTL.MonadState' instance.
stateToMonadState
    :: (MTL.MonadState s m)
    => (State s ~> m)
stateToMonadState = \case
    Get   -> MTL.get
    Put v -> MTL.put v

monadStateToState
    :: (Member (State s) effs)
    => MTL.State s a
    -> Eff effs a
monadStateToState a = do
    s1 <- get
    let (r, s2) = MTL.runState a s1
    put s2
    return r

-- | Handle an 'Error' effect in terms of a monadic effect which has a 'MTL.MonadError' instance.
errorToMonadError
    :: (MTL.MonadError e m)
    => (Error e ~> m)
errorToMonadError = \case
    Error e -> MTL.throwError e

-- | Transform an error type
wrapError
    :: forall e f effs. Member (Error f) effs
    => (e -> f)
    -> Eff (Error e ': effs)
    ~> Eff effs
wrapError f = flip handleError (throwError @f . f)

-- This is a simple state modelling library for use with Haskell
-- QuickCheck.

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Test.QuickCheck.StateModel(
    StateModel(..)
  , Any(..)
  , Step(..)
  , LookUp, Var(..) -- we export the constructors so that users can construct test cases
  , Actions(..)
  , pattern Actions
  , EnvEntry(..)
  , Env
  , stateAfter
  , runActions
  , runActionsInState
  , lookUpVar
  , lookUpVarMaybe
) where

import Control.Monad

import Data.Data

import Test.QuickCheck as QC
import Test.QuickCheck.DynamicLogic.SmartShrinking
import Test.QuickCheck.Monadic

class (forall a. Show (Action state a),
       Monad (ActionMonad state),
       Show state,
       Typeable state) =>
        StateModel state where
  data Action state a
  type ActionMonad state :: * -> *
  actionName      :: Action state a -> String
  actionName = head . words . show
  arbitraryAction :: state -> Gen (Any (Action state))
  shrinkAction    :: (Typeable a, Show a) => state -> Action state a -> [Any (Action state)]
  shrinkAction _ _ = []
  initialState    :: state
  nextState       :: state -> Action state a -> Var a -> state
  nextState s _ _ = s
  precondition    :: state -> Action state a -> Bool
  precondition _ _ = True
  perform         :: state -> Action state a -> LookUp -> ActionMonad state a
  perform _ _ _ = return undefined
  postcondition   :: state -> Action state a -> LookUp -> a -> Bool
  postcondition _ _ _ _ = True
  monitoring      :: (state,state) -> Action state a -> LookUp -> a -> Property -> Property
  monitoring _ _ _ _ = id

type LookUp = forall a. Typeable a => Var a -> a

type Env = [EnvEntry]

data EnvEntry where
  (:==) :: (Show a, Typeable a) => Var a -> a -> EnvEntry

infix 5 :==

deriving instance Show EnvEntry

lookUpVarMaybe :: Typeable a => Env -> Var a -> Maybe a
lookUpVarMaybe [] _ = Nothing
lookUpVarMaybe ((v' :== a) : env) v =
  case cast (v',a) of
    Just (v'',a') | v==v'' -> Just a'
    _                      -> lookUpVarMaybe env v
lookUpVar :: Typeable a => Env -> Var a -> a
lookUpVar env v = case lookUpVarMaybe env v of
  Nothing -> error $ "Variable "++show v++" is not bound!"
  Just a  -> a

data Any f where
  Some :: (Show a, Typeable a, Eq (f a)) => f a -> Any f
  Error :: String -> Any f

deriving instance (forall a. Show (Action state a)) => Show (Any (Action state))

instance Eq (Any f) where
  Some (a :: f a) == Some (b :: f b) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing   -> False
  Error s == Error s' = s == s'
  _ == _ = False

data Step state where
  (:=) :: (Show a, Typeable a, Eq (Action state a), Typeable (Action state a), Show (Action state a)) =>
            Var a -> Action state a -> Step state

infix 5 :=

deriving instance (forall a. Show (Action state a)) => Show (Step state)

newtype Var a = Var Int
  deriving (Eq, Ord, Show, Typeable, Data)

instance Eq (Step state) where
  (Var i := act) == (Var j := act') =
    (i==j) && Some act == Some act'

-- Action sequences use Smart shrinking, but this is invisible to
-- client code because the extra Smart constructor is concealed by a
-- pattern synonym.

-- We also collect a list of names of actions which were generated,
-- but were then rejected by their precondition.

data Actions state = Actions_ [String] (Smart [Step state])

pattern Actions :: [Step state] -> Actions state
pattern Actions as <- Actions_ _ (Smart _ as) where
  Actions as = Actions_ [] (Smart 0 as)

{-# COMPLETE Actions #-}

instance Semigroup (Actions state) where
  Actions_ rs (Smart k as) <> Actions_ rs' (Smart _ as') = Actions_ (rs++rs') (Smart k (as <> as'))

instance Eq (Actions state) where
  Actions as == Actions as' = as == as'

instance (forall a. Show (Action state a)) => Show (Actions state) where
  showsPrec d (Actions as)
    | d>10      = ("("++).showsPrec 0 (Actions as).(")"++)
    | null as   = ("Actions []"++)
    | otherwise = (("Actions \n [")++) .
                  foldr (.) (showsPrec 0 (last as) . ("]"++))
                    [showsPrec 0 a . (",\n  "++) | a <- init as]

instance (Typeable state, StateModel state) => Arbitrary (Actions state) where
  arbitrary = do (as,rejected) <- arbActions initialState 1
                 return $ Actions_ rejected (Smart 0 as)
    where
      arbActions :: state -> Int -> Gen ([Step state],[String])
      arbActions s step = sized $ \n ->
        let w = n `div` 2 + 1 in
          frequency [(1, return ([], [])),
                     (w, do (mact, rej) <- satisfyPrecondition
                            case mact of
                              Just (Some act) -> do
                                (as,rejected) <- arbActions (nextState s act (Var step)) (step+1)
                                return ((Var step := act):as, rej++rejected)
                              Just Error{} -> error "impossible"
                              Nothing ->
                                return ([], []))]
        where satisfyPrecondition = sized $ \n -> go n (2*n) []  -- idea copied from suchThatMaybe
              go m n rej
                | m > n = return (Nothing, rej)
                | otherwise = do
                    a <- resize m $ arbitraryAction s
                    case a of
                      Some act ->
                        if precondition s act then return (Just (Some act), rej)
                        else go (m+1) n (actionName act:rej)
                      Error _ ->
                        go (m+1) n rej

  shrink (Actions_ rs as) =
    map (Actions_ rs) (shrinkSmart (map (prune . map fst) . shrinkList shrinker . withStates) as)
    where shrinker ((Var i := act),s) = [((Var i := act'),s) | Some act' <- shrinkAction s act]

prune :: StateModel state => [Step state] -> [Step state]
prune = loop initialState
  where loop _s [] = []
        loop s ((var := act):as)
          | precondition s act
            = (var := act):loop (nextState s act var) as
          | otherwise
            = loop s as


withStates :: StateModel state => [Step state] -> [(Step state,state)]
withStates = loop initialState
  where
    loop _s [] = []
    loop s ((var := act):as) =
      ((var := act),s):loop (nextState s act var) as

stateAfter :: StateModel state => Actions state -> state
stateAfter (Actions actions) = loop initialState actions
  where
    loop s []                  = s
    loop s ((var := act) : as) = loop (nextState s act var) as

runActions :: StateModel state =>
                Actions state -> PropertyM (ActionMonad state) (state,Env)
runActions = runActionsInState initialState

runActionsInState :: StateModel state =>
                    state -> Actions state -> PropertyM (ActionMonad state) (state,Env)
runActionsInState state (Actions_ rejected (Smart _ actions)) = loop state [] actions
  where
    loop _s env [] = do
      when (not . null $ rejected) $
        monitor (tabulate "Actions rejected by precondition" rejected)
      return (_s,reverse env)
    loop s env ((Var n := act):as) = do
      pre $ precondition s act
      ret <- run (perform s act (lookUpVar env))
      let name = actionName act
      monitor (tabulate "Actions" [name])
      let s'   = nextState s act (Var n)
          env' = (Var n :== ret):env
      monitor (monitoring (s,s') act (lookUpVar env') ret)
      assert $ postcondition s act (lookUpVar env) ret
      loop s' env' as

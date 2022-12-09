{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Plutus.Contract.Test.Coverage.Analysis.Types where
import Control.Arrow (first)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Stack
import PlutusCore.DeBruijn hiding (DeBruijn)
import PlutusTx.Coverage

import Plutus.Contract.Test.Coverage.Analysis.Common

infixl 5 :>
data SnocList a = Nil | SnocList a :> a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Generic)

instance Semigroup (SnocList a) where
  xs <> Nil       = xs
  xs <> (ys :> y) = (xs <> ys) :> y

instance Monoid (SnocList a) where
  mempty  = Nil
  mappend = (<>)

zipWithSnoc :: (a -> b -> c) -> SnocList a -> SnocList b -> SnocList c
zipWithSnoc _ Nil _               = Nil
zipWithSnoc _ _ Nil               = Nil
zipWithSnoc f (xs :> x) (ys :> y) = zipWithSnoc f xs ys :> f x y

-- *** Domain definitions
-- data DCon ctx = DCon [DTyp ctx] deriving Show
data DCon = DCon [DTyp]
  deriving (Show, Eq, Generic)

-- data DDat ctx = DDat (isRec : Bool) (x : NamedTyDeBruijn) (k : Kin) (pars : [NamedTyDeBruijn])
--                      [DCon (ctx :> (if rec then Nil else x ::: k) :> (pars ::: _someKinds)]
data DDat = DDat Bool NamedTyDeBruijn Kin [NamedTyDeBruijn] [DCon]
  deriving (Show, Eq, Generic)

data TyCtxEntry = NamedTyDeBruijn ::: Kin     -- db index always 0 (only used for the name for printing)
                | TyCtxRecDat (SnocList DDat) -- (Mutually) recursive data types
                | TyCtxDat DDat               -- Non-recursive data type
    deriving (Show, Eq, Generic)

-- Γ :> TyCtxRecDat ds  ==>  ds : Dat (Γ :> TyCtxRecDat ds)
-- Γ :> TyCtxDat d      ==>  d  : Dat Γ
type TyCtx = SnocList TyCtxEntry

type Subst a = SnocList a

data Dom = DTop { ty         :: DTyp
                , depth      :: Int
                , _locations :: Set CoverageAnnotation }

         | DError

         | DSusp { _locations :: Set CoverageAnnotation
                 , inner      :: Dom }

         | DTySusp { suspName   :: NamedTyDeBruijn
                   , kind       :: Kin
                   , _locations :: Set CoverageAnnotation
                   , inner      :: Dom }

         | DIf { argTy      :: DTyp
               , _locations :: Set CoverageAnnotation }

         | DTrace { argTy      :: DTyp
                  , _locations :: Set CoverageAnnotation }

         | DLoc { location :: CoverageAnnotation }

         | DLam { lamName    :: NamedDeBruijn
                , argTy      :: DTyp        -- DTyp ctx
                , substD     :: Subst Dom   -- Subst _ctx (Dom ctx)
                , substT     :: Subst DTyp  -- Subst _ctx (DTyp ctx)
                , body       :: Trm         -- Trm (_ctx, x : argTy) -- remember that _ctx is really mixed type and term context - very confusing...
                , _locations :: Set CoverageAnnotation }

         | DConstr { dat        :: DTyp     -- Target type once fully applied
                   , constr     :: Int
                   , argsD      :: SnocList Dom
                   , _locations :: Set CoverageAnnotation }

         | DMatch { dat        :: DTyp
                  , _locations :: Set CoverageAnnotation }

         | DUnion [Dom] -- These are never DWeaken

         | DWeaken { wk    :: Weakening
                   , inner :: Dom }
  deriving (Show, Generic)

data DTyp = DTVar NamedTyDeBruijn [DTyp]
          | DTFun DTyp DTyp
          | DTLam { dtName :: NamedTyDeBruijn, dtKind :: Kin, dtBody :: DTyp }
          | DTForall { dtName :: NamedTyDeBruijn, dtKind :: Kin, dtBody :: DTyp }
          | DTWk { dtWk :: Weakening , dtBody :: DTyp }
          | DTyBuiltin Kin -- we don't care which
  deriving (Show, Eq, Generic)

data DArg = TyArg DTyp | DArg Dom
  deriving (Show)

-- strictness?
newtype Weakening = Wk [(Index, Index)] -- increasing in k, (k, n) means weaken by n at index k (cumulative)
  deriving (Show, Eq, Generic)

wkIndex :: HasCallStack => Weakening -> Index -> Index
wkIndex (Wk w) i = i + sum [ n | (k, n) <- w, i >= k ]

wkBy :: HasCallStack => Index -> Weakening
wkBy n | n == 0    = Wk []
       | otherwise = Wk [(0, n)]

shiftWeakening :: HasCallStack => Weakening -> Weakening
shiftWeakening (Wk w) = Wk $ map (first succ) w

instance Semigroup Weakening where
  Wk w <> (Wk w') = Wk $ foldr merge w' (map (unweaken w') w)
    where
      unweaken [] (k, i)           = [(k, i)]
      unweaken ((k', i'):w) (k, i)
        | k' <= k   = unweaken w (if i' > k then k' else max k' (k - i'), i)
        | otherwise = [(k, i)]

      merge w1@((k1, n1) : w1') w2@((k2, n2) : w2')
        | k1 == k2  = (k1, n1 + n2) : merge w1' w2'
        | k1 < k2   = (k1, n1) : merge w1' w2
        | otherwise = (k2, n2) : merge w1 w2'
      merge [] w2 = w2
      merge w1 [] = w1

instance Monoid Weakening where
  mempty = Wk []
  mappend = (<>)

-- Some helper functions
topLevelLocations :: HasCallStack => Dom -> Set CoverageAnnotation
topLevelLocations (DUnion ds)   = foldMap topLevelLocations ds
topLevelLocations (DWeaken _ d) = topLevelLocations d
topLevelLocations DError        = mempty
topLevelLocations DLoc{}        = mempty
topLevelLocations d             = _locations d

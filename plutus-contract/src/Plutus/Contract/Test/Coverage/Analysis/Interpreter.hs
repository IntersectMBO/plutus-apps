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

{-# OPTIONS_GHC -fno-warn-name-shadowing
                -fno-warn-redundant-constraints
                -fno-warn-incomplete-record-updates
                -fno-warn-incomplete-uni-patterns
                -fno-warn-unused-top-binds
                #-}
-- TODO: the final -fno-warn here will be removed when we
-- merge the test harness (after PIR generators are merged into core)
-- and know what needs to be exported to make that make as much sense as possible.

module Plutus.Contract.Test.Coverage.Analysis.Interpreter (allNonFailLocations) where
import Control.Arrow hiding ((<+>))
import Data.Foldable
import Data.List hiding (insert)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Stack
import PlutusCore.Builtin
import PlutusCore.DeBruijn hiding (DeBruijn)
import PlutusCore.Default
import PlutusIR
import PlutusTx.Code
import PlutusTx.Coverage
import Text.PrettyPrint hiding (integer, (<>))

import Plutus.Contract.Test.Coverage.Analysis.Common
import Plutus.Contract.Test.Coverage.Analysis.DeBruijn
import Plutus.Contract.Test.Coverage.Analysis.Pretty
import Plutus.Contract.Test.Coverage.Analysis.Types

{- Note [Static analysis for non-fail-paths in plutus programs]
  In this module we implement at static analysis tool losely based
  on some form of abstract interpretation for finding program locatations
  (represented as PlutusTx coverage annotations) that are on the success
  paths of a plutus program. For example, in the following program:

    foo x y = if <Program Location 0> x == y then
                <Program Location 1> error "Fail"
              else
                <Program Location 2> ()

  Program locations 0 and 2 are on the success path (that doesn't result in error)
  and program location 1 is on the fail path. This information can be used (and is
  used in `Plutus.Contract.Test.Coverage.Analysis`) to improve coverage annotations
  to make the result of coverage checking significantly more readable.

  The way this works is we basically do abstract interpretation. That is to say we
  interpret PIR programs in a domain of "abstract programs" `Dom`. This domain track
  of possible program locations, unions of possible results,
  representations of datatypes and suspended computations, and a few escape hatches for
  embedding full PIR programs. Abstract interpretation then works by short-cutting recursion
  (by introducing DTop top values in the abstract domain) and aggressively expanding branches
  (case expressions) as unions.

  Most of the complexity in this module is related to the fact that we need to keep track
  of types in order to expand branches. Note that a number of functions have dependent
  types in comments above the type signature. THESE ARE IMPORTANT: not adhering to these
  specifications will most likely break the analysis and because they are not type-checked
  changes in this module have to be made with care.
-}

cost :: DDat -> Int
cost (DDat _ _ _ _ constrs)
    | length constrs < 2 = 0
    | otherwise = 1

idDTySubst :: TyCtx -> Subst DTyp
idDTySubst ctx = go ctx 0
  where
    go Nil _ = Nil
    go (ctx :> e) !i =
      case e of
        x ::: _                   -> go ctx (i + 1) :> DTVar (setDbIndex x i) []
        TyCtxDat (DDat _ x _ _ _) -> go ctx (i + 1) :> DTVar (setDbIndex x i) []
        TyCtxRecDat ds            -> go ctx (i + fromIntegral (length ds)) <> goDats ds i

    goDats Nil _                    = Nil
    goDats (ds :> DDat _ x _ _ _) i = goDats ds (i + 1) :> DTVar (setDbIndex x i) []

-- wkDat :: {ctx : TyCtx, ctx' : TyCtx}
--       -> (wk : Subst ctx ctx')
--       -> DDat ctx
--       -> DDat ctx'
wkDat :: Weakening -> DDat -> DDat
wkDat w (DDat rec nm k pars cons) =
  DDat rec (wkDbIndex w nm) k pars [ DCon (wkT (shiftsWeakening (length pars + if rec then 0 else 1) w) <$> args)
                                   | DCon args <- cons ]
  where
    shiftsWeakening n = foldr (.) id (replicate n shiftWeakening)

-- lookupDat :: (ctx : TyCtx)
--           -> Index
--           -> DDat ctx
lookupDat :: HasCallStack
          => TyCtx
          -> NamedTyDeBruijn
          -> DDat
lookupDat ctx nm =
  case lookupCtx ctx nm of
    Left{} -> errorDoc $
          "lookupDat: " <+> vcat [ "ctx =" <+> pretty ctx
                                 , "nm =" <+> pretty nm]
    Right dat -> dat

lookupCtx :: HasCallStack => TyCtx -> NamedTyDeBruijn -> Either Kin DDat
lookupCtx ctx0 nm = go ctx0 (getDbIndex nm) mempty
  where
    go :: TyCtx -> Index -> Weakening -> Either Kin DDat
    go Nil _ _ = errorDoc $
        "lookupCtx Nil:" <+> vcat [ "ctx0 =" <+> pretty ctx0
                                  , "nm =" <+> pretty nm ]

    go (ctx :> _ ::: k) i w
      | i == 0    = Left k
      | otherwise = go ctx (i - 1) (wkBy 1 <> w)

    go (ctx :> TyCtxRecDat ds) i w
      | i < len ds = Right $ wkDat w (lookupSubst ds i)
      | otherwise  = go ctx (i - len ds) (wkBy (len ds) <> w)

    go (ctx :> TyCtxDat d) i w
      | i == 0    = Right $ wkDat (wkBy 1 <> w) d
      | otherwise = go ctx (i-1) (wkBy 1 <> w)

dUnion :: Dom -> Dom -> Dom
dUnion DError d      = d
dUnion d DError      = d
dUnion (DUnion ds) d = DUnion (insert (pushWeaken d) ds)
dUnion l r           = DUnion (insert (pushWeaken r) [pushWeaken l])

dUnions :: [Dom] -> Dom
dUnions [] = errorDoc "dUnions []"
dUnions ds = foldr1 dUnion ds

insert :: Dom -> [Dom] -> [Dom]
insert d@DLam{} ds     = d : ds
insert (DUnion ds') ds = foldr insert ds ds'
insert d []            = [d]
insert d (d':ds)       = case (d, d') of
  (DTop ty depth locs , DTop ty' depth' locs')
    | not debug || normTy ty == normTy ty' ->
      DTop ty (max depth depth') (locs <> locs') : ds
    | otherwise -> errorDoc $ "insert DTop type error:" <?> vcat ["ty =" <+> pretty ty, "ty' =" <+> pretty ty']
  (DSusp locs inner , DSusp locs' inner')
    | locs == locs' -> DSusp locs (dUnion inner inner') : ds
    | [inner''] <- insert inner [inner'] -> DSusp (locs <> locs') inner'' : ds
  (DTySusp nm k locs inner , DTySusp _ _ locs' inner')
    | locs == locs' -> DTySusp nm k locs (dUnion inner inner') : ds
    | [inner''] <- insert inner [inner'] -> DTySusp nm k (locs <> locs') inner'' : ds
  (DIf ty locs , DIf ty' locs')
    | not debug || normTy ty == normTy ty' ->
      DIf ty (locs <> locs') : ds
    | otherwise -> errorDoc $ "insert DIf type error:" <?> vcat ["ty =" <+> pretty ty, "ty' =" <+> pretty ty']
  (DTrace ty locs , DTrace ty' locs')
    | not debug || normTy ty == normTy ty' ->
      DTrace ty (locs <> locs') : ds
    | otherwise -> errorDoc $ "insert DTrace type error:" <?> vcat ["ty =" <+> pretty ty, "ty' =" <+> pretty ty']
  (DLoc l , DLoc l')
    | l == l' ->
      DLoc l : ds
  (DConstr dat idx argsD locs , DConstr dat' idx' argsD' locs')
    | not debug || (normTy dat == normTy dat' && length argsD == length argsD')
    , idx == idx' ->
      DConstr dat idx (zipWithSnoc dUnion argsD argsD') (locs <> locs') : ds
    | idx == idx' -> errorDoc $ "insert DConstr type error:"
      <?> vcat ["idx =" <+> pretty idx
               ,"dat =" <+> pretty dat, "dat' =" <+> pretty dat'
               ,"argsD =" <+> pretty argsD, "argsD' =" <+> pretty argsD']
  (DMatch dat locs , DMatch dat' locs')
    | dat == dat' ->
      DMatch dat (locs <> locs') : ds
  _ -> d' : insert d ds

aggro :: Int
aggro = 100

addLocations :: HasCallStack => Set CoverageAnnotation -> Dom -> Dom
addLocations locs d | null locs = d
addLocations locs (DUnion ds) = DUnion (addLocations locs <$> ds)
addLocations locs (DWeaken n d) = DWeaken n (addLocations locs d)
addLocations _    DError = DError
addLocations _    (DLoc l) = DLoc l
addLocations locs d = d { _locations = _locations d <> locs }

allLocations :: HasCallStack => TyCtx -> Dom -> Set CoverageAnnotation
allLocations ctx d = case pushWeaken d of
  DSusp locs d                      -> locs <> allLocations ctx d
  DTySusp x k locs d                -> locs <> allLocations (ctx :> x ::: k) d
  DLam _ ty substD substT body locs -> locs <> (uncurry allLocations . first (ctx <>)
                                               $ interp ctx (substD :> dTop ty aggro mempty) substT body [])
  DUnion ds                         -> foldMap (allLocations ctx) ds
  DWeaken{}                         -> error "allLocations: DWeaken"
  DConstr _ _ args locs             -> locs <> foldMap (allLocations ctx) (toList args)
  DLoc _                            -> mempty
  _                                 -> topLevelLocations d

normalize_ :: HasCallStack => TyCtx -> Dom -> Dom
normalize_ ctx d = nf
  where (Nil, nf) = normalize ctx d

normalize :: HasCallStack => TyCtx -> Dom -> (TyCtx, Dom)
normalize ctx d = let d' = pushWeaken d in
  case d' of
    DSusp locs d                      -> second (DSusp locs) (normalize ctx d)
    DTySusp x k locs d                -> (Nil, DTySusp x k locs $ normalize_ (ctx :> x ::: k) d)
    DLam _ ty substD substT body locs -> (ctx' <> ctx'', DSusp locs nf)
      where
        (ctx', d'') = interp ctx (substD :> dTop ty aggro mempty) substT body []
        (ctx'', nf) = normalize (ctx <> ctx') d''
    DUnion ds ->
      case unzip $ normalize ctx <$> ds of
        (ctx : ctxs, ds) | all (ctx ==) ctxs -> (ctx, dUnions ds)
        (ctxs, ds)                           -> errorDoc $ "normalize DUnion:" <+> pretty (zip ctxs ds)
    DConstr dat con args locs         ->
      (Nil, DConstr (normTy dat) con (normalize_ ctx <$> args) locs)
    DTop ty d locs -> (Nil, DTop (normTy ty) d locs)
    _ -> (Nil, d')

wkD :: HasCallStack => Weakening -> Dom -> Dom
wkD (Wk []) d        = d
wkD w (DWeaken wk d) = DWeaken (w <> wk) d
wkD w (DUnion ds)    = DUnion (wkD w <$> ds)
wkD w d              = DWeaken w d

wkT :: HasCallStack => Weakening -> DTyp -> DTyp
wkT (Wk []) a     = a
wkT w (DTWk wk a) = DTWk (w <> wk) a
wkT w a           = DTWk w a

wkArg :: HasCallStack => Weakening -> DArg -> DArg
wkArg w (TyArg t) = TyArg (wkT w t)
wkArg w (DArg d)  = DArg  (wkD w d)

--lookupSubst :: {_ctx ctx}
--            -> Subst _ctx (f ctx)
--            -> Index
--            -> f ctx
lookupSubst :: HasCallStack => Subst a -> Index -> a
lookupSubst (_   :> x) 0 = x
lookupSubst (sub :> _) i = lookupSubst sub (i - 1)
lookupSubst Nil        _ = error "lookupSubst: out of bounds"

len :: (Functor f, Foldable f, Integral i) => f a -> i
len = sum . fmap (const 1)

ctxLen :: HasCallStack => TyCtx -> Index
ctxLen = sum . fmap entryLen
  where
    entryLen (_ ::: _)        = 1
    entryLen TyCtxDat{}       = 1
    entryLen (TyCtxRecDat ds) = len ds

pushWeaken :: HasCallStack => Dom -> Dom
pushWeaken (DWeaken w d) = case d of
  DTop ty dep locs                  -> dTop (wkT w ty) dep locs
  DError                            -> DError
  DSusp locs d                      -> DSusp locs (wkD w d)
  DTySusp x k locs d                -> DTySusp x k locs (wkD (shiftWeakening w) d)
  DIf ty locs                       -> DIf (wkT w ty) locs
  DTrace ty locs                    -> DTrace (wkT w ty) locs
  DLoc{}                            -> d
  DLam x ty substD substT body locs -> DLam x (wkT w ty) (wkD w <$> substD) (wkT w <$> substT) body locs
  DConstr dat con args locs         -> DConstr (wkT w dat) con (wkD w <$> args) locs
  DMatch dat locs                   -> DMatch (wkT w dat) locs
  DUnion{}                          -> error "pushWeaken: DWeaken/DUnion"
  DWeaken{}                         -> error "pushWeaken: DWeaken/DWeaken"
pushWeaken (DUnion ds) = DUnion (pushWeaken <$> ds)
pushWeaken d = d

wkDbIndex :: IsDbName i => Weakening -> i -> i
wkDbIndex w x = setDbIndex x (wkIndex w (getDbIndex x))

pushWeakenTy :: HasCallStack => DTyp -> DTyp
pushWeakenTy (DTWk w a) = case a of
  DTVar x ts     -> DTVar (wkDbIndex w x) (wkT w <$> ts)
  DTFun s t      -> DTFun (wkT w s) (wkT w t)
  DTLam x k t    -> DTLam x k (wkT (shiftWeakening w) t)
  DTForall x k t -> DTForall x k (wkT (shiftWeakening w) t)
  DTyBuiltin k   -> DTyBuiltin k
  DTWk _ _       -> error "pushWeakenTy: DTWk"
pushWeakenTy a = a

normTy :: DTyp -> DTyp
normTy a = case pushWeakenTy a of
  DTVar x ts     -> DTVar x (normTy <$> ts)
  DTFun s t      -> DTFun (normTy s) (normTy t)
  DTLam x k t    -> DTLam x k (normTy t)
  DTForall x k t -> DTForall x k (normTy t)
  DTyBuiltin k   -> DTyBuiltin k
  DTWk{}         -> error "normTy: DTWk"

tyCheck :: TyCtx -> DTyp -> Dom -> Bool
tyCheck ctx a d = case pushWeaken d of
  DTop b _ _ -> normTy a == normTy b
  DSusp _ d  ->
    case pushWeakenTy a of
      DTFun _ b -> tyCheck ctx b d
      _         -> False
  DError{}   -> True
  DTySusp x k _ d ->
    case pushWeakenTy a of
      DTForall y k' b | x == y, k == k' -> tyCheck (ctx :> x ::: k) b d
      _                                 -> False
  DLam _ ty _ _ _ _ ->
    case pushWeakenTy a of
      DTFun argTy _ -> normTy ty == normTy argTy
      _             -> False
  DConstr (normTy -> ddat@(DTVar dat pars)) i args _
    | length pars /= length xs       -> False
    | i >= length cs                 -> False
    | length args > length conArgTys -> False
    | otherwise                      ->
      and $ (normTy a == normTy (foldr DTFun ddat conArgTys2))
          : zipWith (tyCheck ctx) conArgTys1 (toList args)
    where
      -- DDat ctx, so cs : [DCon (ctx :> (if rec then Nil else dat ::: _) :> xs ::: _)]
      DDat rec _ _ xs cs       = lookupDat ctx dat
      DCon conArgTys           = cs !! i
      (conArgTys1, conArgTys2) = splitAt (length args) $ map inst conArgTys
      inst ty | rec       = tyInsts ty pars
              -- inst : DTyp (ctx :> dat ::: _ :> xs ::: _) -> DTyp ctx
              | otherwise = tyInsts ty (DTVar dat [] : pars)

  DConstr{}  -> False

  DMatch (normTy -> (DTVar dat pars)) _ -> case normTy a of
    DTFun (DTVar dat' pars') (DTForall r Star body)
      | dat' /= dat                         -> False
      | map normTy pars /= map normTy pars' -> False
      | length pars /= length xs            -> False
      | otherwise                           ->
        and $ (normTy bodyRes == DTVar r []) : [ normTy b == normTy (foldr DTFun (DTVar r []) (map (wkT (wkBy 1) .  inst) conArgs))
                                               | (b, DCon conArgs) <- zip bodyArgs cs ]
                                    where (bodyArgs, bodyRes) = view body
    _ -> False
    where DDat rec _ _ xs cs = lookupDat ctx dat
          inst ty | rec       = tyInsts ty pars
                  | otherwise = tyInsts ty (DTVar dat [] : pars)
          view (DTFun a b) = first (a:) (view b)
          view a           = ([], a)

  DMatch{} -> False

  DIf ty _    -> normTy (DTFun (DTyBuiltin Star) (DTFun ty (DTFun ty ty))) == normTy a
  DTrace ty _ -> normTy (DTFun (DTyBuiltin Star) (DTFun ty ty)) == normTy a
  DUnion ds   -> all (tyCheck ctx a) ds
  DLoc{}      -> normTy a == DTyBuiltin Star
  DWeaken{}   -> error "tyCheck: DWeaken"

-- dTop :: {ctx :: TyCtx} -> DTyp ctx -> Int -> Set CoverageAnnotation -> Dom ctx
dTop :: DTyp -> Int -> Set CoverageAnnotation -> Dom
dTop (pushWeakenTy -> DTForall x k dt) i locs = DTySusp x k locs $ dTop dt i mempty
dTop ty i locs                                = DTop ty i locs

-- tyInsts :: DTyp (ctx :> ctx')
--         -> { xs : [DTyp ctx] | length xs = length ctx', xs !! i : toList ctx' !! i }
--         -> DTyp ctx
tyInsts :: DTyp -> [DTyp] -> DTyp
tyInsts a []     = a
tyInsts a (x:xs) = tyInsts (tyInst (fromIntegral $ length xs) a x) xs

domApp :: HasCallStack
       => TyCtx   -- (ctx : TyCtx)
       -> Dom     -- Dom ctx
       -> Dom     -- Dom ctx
       -> Dom     -- Dom ctx
domApp _ DError _ = DError -- DONT MOVE! The order of this case shortcuts unions in the argument
domApp ctx d (pushWeaken -> DUnion ds) = dUnions (domApp ctx d <$> ds)
domApp _ _ DError = DError
domApp ctx d arg = addLocations (topLevelLocations arg) $ case d of
  DTop (pushWeakenTy -> DTFun argT b) dep locs
    | not debug || tyCheck ctx argT arg ->
      dTop b dep (locs <> allLocations ctx arg)
    | otherwise -> errorDoc $ "domApp - type error - DTop:"
                                <?> vcat ["d =" <+> pretty d
                                         ,"arg =" <+> pretty arg]

  DError -> DError

  DSusp locs d -> addLocations (locs <> allLocations ctx arg) d

  DIf ty locs -> case arg of
    DTop (pushWeakenTy -> DTyBuiltin Type{}) _ _ ->
      let x = NamedDeBruijn "x" 0
          y = NamedDeBruijn "y" 0
          a = TyVar () (NamedTyDeBruijn (NamedDeBruijn "a" 0))
          substT' = Nil :> ty
          -- Here _ctx = a{0}
          -- and under the LamAbs it's (in the mixed ctx format) _ctx = a{0} :> y : a{0}
          dTrue  = DLam x ty mempty substT' (LamAbs () y a (Var () (setDbIndex x 1))) locs
          dFalse = DLam x ty mempty substT' (LamAbs () y a (Var () y)) locs
      in dUnion dTrue dFalse
    _ -> errorDoc $ "domApp: DIf" <?> ("arg =" <+> pretty arg)

  DTrace ty locs -> case pushWeaken arg of
    DLoc l              ->
      let x = NamedDeBruijn "x" 0
      in DLam x ty mempty mempty (Var () x) (locs <> Set.singleton l)
    DTop (pushWeakenTy -> DTyBuiltin Type{}) _ _ ->
      let x = NamedDeBruijn "x" 0
      in DLam x ty mempty mempty (Var () x) locs
    _                  -> errorDoc $ "domApp: DTrace" <?> vcat [ "arg =" <+> pretty arg ]

  DLam _ ty substD substT body locs
    | not debug || tyCheck ctx ty arg ->
      addLocations locs $ interp_ ctx (substD :> arg) substT body []
    | otherwise -> errorDoc $ "domApp - type error - DLam:"
                                <?> vcat ["ty =" <+> pretty (normTy ty)
                                         ,"body =" <+> pretty body
                                         ,"arg =" <+> pretty arg]

  DConstr dat con args locs -> DConstr dat con (args :> arg) locs

  match@(DMatch (pushWeakenTy -> DTVar d pars) locs) -> case pushWeaken arg of

    DTop (pushWeakenTy -> ty) depth alocs
      | DTVar{} <- ty
      , dat@(DDat False nm _ _ constrs) <- lookupDat ctx d
      , depth >= cost dat ->
         -- We are matching on datatype `C` and the argument is `T[C p0 ... pn]`
         traceDoc Med ("domApp - lookupDat" <?> vcat ["ctx =" <+> pretty ctx, "d =" <+> pretty d]) $
         -- Here we are working in `ctx`
         let topArgs (DCon args) = foldl (:>) Nil
              [ dTop (tyInsts a (DTVar nm [] : pars)) (depth-cost dat) mempty
              | a <- args ]
         in traceDoc Unions (("expand" <> brackets (pretty $ length constrs)) <+> pretty ty)
            $ foldl1 dUnion [ domApp ctx match $ DConstr ty i (topArgs c) alocs
                            | (i, c) <- zip [0..] constrs ]
      -- TODO: unuglyfy
      | DTVar{} <- ty
      , (DDat True _ _ _ _) <- lookupDat ctx d ->
        let ty = DTForall r Star $ foldr DTFun (DTVar r []) argTypes
        in dTop ty aggro (locs <> allLocations ctx arg)
      | otherwise ->
        let ty = DTForall r Star $ foldr DTFun (DTVar r []) argTypes
        in dTop ty 0 (locs <> allLocations ctx arg)

    -- Constructor argument
    -- TODO: refactor lambdas (maybe have named (DLam) and un-named (DSusp) abstraction in the language
    -- and push closures over terms to their own constructor?)
    DConstr _ conIdx argsD alocs ->
      let n = length constrs
          m = length argsD
          -- c_0{0} , ... , c_(n-1){0}
          xCargs = [ NamedDeBruijn ("c" <> Text.pack (show i)) 0
                   | i <- [0..n-1] ]
          -- a_0{n} , ... , a_(m-1){n+m-1} :: Trm (_ctx :> c_0 ... c_(n-1))
          xAargs = [ Var () $ NamedDeBruijn ("a" <> Text.pack (show i)) $ fromIntegral (i + n)
                   | i <- [0..m-1] ]
          -- a_m :> ... :> a_0
          -- argsD :: SnocList (DTyp ctx)
          -- means we need to shift it into ctx'
          substD = wkD (wkBy 1) <$> foldl (:>) Nil (reverse $ toList argsD)
          -- t_1{0} , ... , t_(n-1){n-2}
          targs = [ TyVar () $ NamedTyDeBruijn
                             $ NamedDeBruijn ("t" <> Text.pack (show (i+1)))
                             $ fromIntegral i
                  | i <- [0..n - 2] ]
          -- t_n :> ... :> t_1
          substT = foldl (:>) Nil (reverse $ tail argTypes)
          -- c_conIdx{(n-1)-conIdx}
          con = Var () $ NamedDeBruijn ("c" <> Text.pack (show conIdx)) (fromIntegral $ (n - 1) - conIdx)
      in DTySusp r Star (locs <> alocs) $
          -- \ c_0 : ta_0 ->
          DLam (head xCargs) (head argTypes)
               substD -- a_m :> ... :> a_0
               substT -- t_n :> ... :> t_1
               -- \ c_1 : t_1{0} , ... , c_n : t_n{n-1} ->
               (flip (foldr (uncurry (LamAbs ()))) (zip (tail xCargs) targs) $
                -- c_conIdx{n-conIdx} a_0{0} ... a_m{m}
                foldl (Apply ()) con xAargs)
               mempty

    _ -> error $ "domApp: DMatch\narg = " ++ show arg ++ "\n\nctx = " ++ show ctx

    where
      r    = NamedTyDeBruijn $ NamedDeBruijn "r" 0
      ctx' = ctx :> r ::: Star
      -- pars :: [DTyp ctx]
      -- pars' :: [DTyp ctx']
      pars' = wkT (wkBy 1) <$> pars
      -- d' :: DTyp ctx'
      d'    = wkDbIndex (wkBy 1) d
      -- constrs :: [DCon (ctx' :> _nm ::: _k :> (_pars ::: _someKinds))]
      -- _nm = d'
      DDat _r _nm _k _pars constrs = lookupDat ctx' d'
      -- mkConTy :: DCon (ctx' :> _nm{0} ::: _k :> (_pars ::: _someKinds)) -> DTyp ctx'
      mkConTy (DCon args) = foldr DTFun (DTVar r []) $ flip tyInsts (if _r then pars' else DTVar d' [] : pars') <$> args
      -- argTypes :: [DTyp ctx']
      argTypes = map mkConTy constrs

  _                                 -> errorDoc $ "domApp: " <?> vcat [ "d =" <+> pretty d
                                                                      , "arg =" <+> pretty arg
                                                                      , "ctx =" <+> pretty ctx]

domTyApp :: HasCallStack
         => TyCtx   -- (ctx : TyCtx)
         -> Dom     -- Dom ctx
         -> DTyp    -- DTyp ctx
         -> Dom     -- Dom ctx
domTyApp ctx DTySusp{..} a = addLocations _locations $ domTyInst ctx inner a
domTyApp _ DError        _ = DError
domTyApp _ d t             = errorDoc $ "domTyApps: " <?> vcat [ "d =" <+> pretty d
                                                                 , "t =" <+> pretty t ]

-- tyInst :: {ctx ctx' : TyCtx}
--        -> (i : Index | i == length ctx')
--        -> DTyp (ctx :> a ::: k :> ctx')
--        -> DTyp ctx
--        -> DTyp (ctx :> ctx')
tyInst :: HasCallStack => Index -> DTyp -> DTyp -> DTyp
tyInst i a b = case pushWeakenTy a of
  DTVar x ts | getDbIndex x > i  -> DTVar (setDbIndex x (getDbIndex x - 1)) ts'
             | getDbIndex x == i -> tyApps (wkT (wkBy i) b) ts'
             | otherwise         -> DTVar x ts'
             where ts' = [ tyInst i a b | a <- ts ]
  DTFun s t      -> DTFun (tyInst i s b) (tyInst i t b)
  DTLam x k t    -> DTLam x k (tyInst (i+1) t b)
  DTForall x k t -> DTForall x k (tyInst (i+1) t b)
  DTyBuiltin k   -> DTyBuiltin k
  DTWk _ _       -> error "tyInst: DTWk"

domTyInst :: HasCallStack
          => TyCtx  -- (ctx : TyCtx)
          -> Dom    -- Dom (ctx :> x ::: k)
          -> DTyp   -- DTyp ctx
          -> Dom    -- Dom ctx
domTyInst _ d t = go 0 d
  where
    -- go :: {ctx' : TyCtx}
    --    -> (i : Index | i == length ctx')
    --    -> Dom (ctx :> a ::: k :> ctx')
    --    -> Dom (ctx :> ctx')
    go !i d = case pushWeaken d of
      DTop ty dep locs                  -> dTop (tyInst i ty t) dep locs
      DError                            -> DError
      DTySusp x k locs body             -> DTySusp x k locs (go (i + 1) body)
      DSusp locs d                      -> DSusp locs (go i d)
      DIf ty locs                       -> DIf (tyInst i ty t) locs
      DTrace ty locs                    -> DTrace (tyInst i ty t) locs
      DLam x ty substD substT body locs -> DLam x (tyInst i ty t) (go i <$> substD)
                                                                  (flip (tyInst i) t <$> substT)
                                                                  body locs
      DConstr dat con args locs         -> DConstr (tyInst i dat t) con (go i <$> args) locs
      DMatch dat locs                   -> DMatch (tyInst i dat t) locs
      DLoc{}                            -> d
      DUnion ds                         -> dUnions (go i <$> ds)
      DWeaken{}                         -> error "domTyInst: DWeaken"

tyApps :: HasCallStack => DTyp -> [DTyp] -> DTyp
tyApps t []                              = t
tyApps t@DTWk{} args                     = tyApps (pushWeakenTy t) args
tyApps (DTVar x args0) args              = DTVar x (args0 <> args)
tyApps (DTLam _ _ body) (arg : args)     = tyApps (tyInst 0 body arg) args
tyApps (DTyBuiltin (_ :-> k)) (_ : args) = tyApps (DTyBuiltin k) args
tyApps t args                            = errorDoc $ "tyApps:" <?> vcat [ "t =" <+> pretty t
                                                                         , "args =" <+> pretty args ]

domApps :: HasCallStack
        => TyCtx
        -> Dom
        -> [DArg] -> Dom
domApps ctx = foldl (app . pushWeaken)
  where
    app d@(DUnion ds) a =
      let res0 = (`app` a) <$> ds
          res = dUnions res0 in
      traceDoc High ("dUnionsApp:" <?> vcat [ "ctx =" <+> pretty ctx
                                            , "d =" <+> pretty d
                                            , "a =" <+> pretty a
                                            , "ds app a=" <+> pretty res0
                                            , "res =" <+> pretty res ])
            res
    app d a@(TyArg t)   =
      let res = domTyApp ctx d t in
      traceDoc High ("domTyApp:" <?> vcat [ "ctx =" <+> pretty ctx
                                          , "d =" <+> pretty d
                                          , "a =" <+> pretty a
                                          , "res =" <+> pretty res ])
            res
    app d a@(DArg d')      =
      let res = domApp ctx d (pushWeaken d') in
      traceDoc High ("domApp:" <?> vcat [ "ctx =" <+> pretty ctx
                                        , "d =" <+> pretty d
                                        , "a =" <+> pretty a
                                        , "res =" <+> pretty res ])
            res

interpTy :: HasCallStack
         => TyCtx         -- (ctx : TyCtx) {_ctx : TyCtx}
         -> Subst DTyp    -- Subst _ctx (DTyp ctx)
         -> Typ           -- Typ _ctx
         -> [DTyp]        -- [DTyp ctx]
         -> DTyp          -- DTyp ctx
interpTy ctx substT ty args = case ty of
  TyVar _ x     -> lookupSubst substT (getDbIndex x) `tyApps` args
  TyBuiltin _ c -> DTyBuiltin (builtinKind c `kindApp` args)
    where
      kindApp k         []         = k
      kindApp (_ :-> k) (_ : args) = kindApp k args
      kindApp Star      _          = errorDoc $ "interpTy TyBuiltin:" <+> pretty ty
  TyFun _ a b | [] <- args -> DTFun (interpTy ctx substT a []) (interpTy ctx substT b [])
              | otherwise  -> error "interpTy: TyFun"
  TyForall _ x k b
    | [] <- args -> DTForall x k $ interpTy (ctx :> x ::: k) ((wkT (wkBy 1) <$> substT) :> DTVar x []) b []
    | otherwise  -> error "interpTy: TyForall"
  TyLam _ x k b ->
    case args of
      []         -> DTLam x k $ interpTy (ctx :> x ::: k) ((wkT (wkBy 1) <$> substT) :> DTVar x []) b []
      arg : args -> interpTy ctx (substT :> arg) b args
  TyApp _ a b   -> interpTy ctx substT a (interpTy ctx substT b [] : args)
  TyIFix _ _ _  -> error "interpTy: TyIFix"

-- interpDat :: {_ctx : TyCtx} (ctx : TyCtx)
--           -> Subst _ctx (DTyp ctx)
--           -> Dat _ctx
--           -> DDat ctx
interpDat :: HasCallStack
          => TyCtx
          -> Subst DTyp
          -> Dat
          -> Bool
          -> DDat
interpDat ctx substT (Datatype _ (TyVarDecl _ n k) pars _ constrs) rec =
  DDat rec n k [ n | TyVarDecl _ n _ <- pars ] [ mkDCon c | c <- constrs ]
  where
    ctxExts = [ n ::: k | not rec ] ++ [ n ::: k | TyVarDecl _ n k <- pars ]
    ctx' = foldl (:>) ctx ctxExts
    wkAmt = genericLength ctxExts

    substExts = [ DTVar n [] | not rec ] ++
                [ DTVar (setDbIndex n $ fromIntegral i) []
                | (i, TyVarDecl _ n _) <- zip (reverse [0..length pars - 1]) pars]

    substT' = foldl (:>) ((wkT (wkBy wkAmt) <$> substT)) substExts

    mkDCon (VarDecl _ _ ty) = DCon [ interpTy ctx' substT' a []
                                   | a <- funArgs ty [] ]

    funArgs (TyFun _ a b) args = funArgs b (a : args)
    funArgs _ args             = reverse args

-- interp_ :: {_ctx : TyCtx}
--         -> (ctx : TyCtx)
--         -> Subst _ctx (Dom ctx)
--         -> Subst _ctx (Typ ctx)
--         -> Trm _ctx
--         -> [DArg ctx]
--         -> Dom ctx
interp_ :: HasCallStack
        => TyCtx
        -> Subst Dom
        -> Subst DTyp
        -> Trm
        -> [DArg]
        -> Dom
interp_ ctx substD substT trm args =
  case interp ctx substD substT trm args of
    (Nil, d)  -> d
    (ctx', _) -> errorDoc $ "interp_: " <+> pretty ctx'

-- interp :: {_ctx : TyCtx}
--        -> (ctx : TyCtx)
--        -> Subst _ctx (Dom ctx)
--        -> Subst _ctx (Typ ctx)
--        -> Trm _ctx
--        -> [DArg ctx]
--        -> (ctx' : TyCtx) * Dom (ctx <> ctx')
interp :: HasCallStack
       => TyCtx
       -> Subst Dom
       -> Subst DTyp
       -> Trm
       -> [DArg]
       -> (TyCtx, Dom)
interp ctx substD substT trm args =
  traceDoc High ("interp:" <?> vcat [ "ctx =" <+> pretty ctx
                                    , "substD =" <+> pretty substD
                                    , "substT =" <+> pretty substT
                                    , "trm =" <+> pretty trm
                                    , "args =" <+> pretty args
                                    , "res =" <+> pretty (snd res)])
        res
  where res = case trm of
                BIF_Trace -> (Nil, domApps ctx (DTySusp a Star mempty (DTrace (DTVar a []) mempty)) args)
                  where a = NamedTyDeBruijn $ NamedDeBruijn "a" 0

                BIF_If -> (Nil, domApps ctx (DTySusp a Star mempty (DIf (DTVar a []) mempty)) args)
                  where a = NamedTyDeBruijn $ NamedDeBruijn "a" 0

                LIT_Loc l
                  | [] <- args -> (Nil, DLoc l)
                  | otherwise  -> error "interp: DLoc"

                Let _ Rec binds body -> first (dctx <>) $ go substDwk (toList binds)
                  where
                    fv              = wkBy (len dats)
                    substDwk        = wkD fv <$> substD
                    (dats, substT') = buildTSubst Nil (wkT fv <$> substT) (toList binds)
                    dctx | Nil <- dats = Nil
                         | otherwise   = Nil :> TyCtxRecDat dats
                    ctx'            = ctx <> dctx
                    substDR         = buildDSubst substDwk (toList binds)

                    buildTSubst dats substT [] = (dats, substT)
                    buildTSubst dats substT (b:binds) = case b of
                      TypeBind _ (TyVarDecl _ _ _) ty ->
                        -- This is subtle! We use the ctx' and substT' that we are in the process of computing
                        -- (laziness ftw). This is fine (insert dog meme) because type synonyms can't be recursive.
                        buildTSubst dats (substT :> interpTy ctx' substT' ty []) binds
                      DatatypeBind _ dat@(Datatype _ (TyVarDecl _ n _) pars _ constrs) ->
                        let normDat      = interpDat ctx' substT' dat True -- :: Dat ctx'
                            (dDat, _, _) = mkDat n pars constrs binds
                        in buildTSubst (dats :> normDat) (substT :> dDat) binds
                      _ -> buildTSubst dats substT binds

                    buildDSubst substD [] = substD
                    buildDSubst substD (b:binds) = case b of                                    -- TODO: locations?? (should be all locations in the letrec!) really use aggro here?
                      TermBind _ _ (VarDecl _ _ ty) _ -> buildDSubst (substD :> dTop (interpTy ctx' substT' ty []) aggro mempty)
                                                                     binds
                      DatatypeBind _ (Datatype _ (TyVarDecl _ n _) pars _ constrs) ->
                        let (_, dMatch, dConstrs) = mkDat n pars constrs binds
                            substD'   = foldl (:>) substD (dMatch : dConstrs)
                        in buildDSubst substD' binds
                      _ -> buildDSubst substD binds

                    go substD [] = interp ctx' substD substT' body $ wkArg fv <$> args
                    go substD (b:binds) = case b of
                      TermBind _ s (VarDecl _ _ _) body ->
                        let result = interp_ ctx' substDR substT' body []
                            locs result = if s == Strict then topLevelLocations result else mempty
                        in case result of
                            -- TODO: the compiler bug also affects this line below!
                            DError | s == Strict -> second (const DError) $ go (substD :> result) binds
                            DUnion ds            ->
                              let rs = [ second (addLocations (locs result))
                                           $ go (substD :> result) binds | result <- ds ]
                              in (fst (head rs), dUnions $ map snd rs)
                            _                    -> second (addLocations (locs result))
                                                      $ go (substD :> result) binds

                      DatatypeBind _ (Datatype _ (TyVarDecl _ n _) pars _ constrs) ->
                        let (_, dMatch, dConstrs) = mkDat n pars constrs binds
                            substD'  = foldl (:>) substD (dMatch : dConstrs)
                        in go substD' binds

                      _ -> go substD binds

                Let _ _ binds body -> go Nil substD substT (toList binds)
                  where
                    -- go :: {_ctx : Ctx} (ctx' : Ctx)
                    --    -> Subst _ctx (Dom (ctx <> ctx'))
                    --    -> Subst _ctx (Typ (ctx <> ctx'))
                    --    -> Binds _ctx
                    --    -> (ctx'' : Ctx) * Dom (ctx <> ctx'')
                    go ctx' substD substT [] = first (ctx' <>) $ interp (ctx <> ctx') substD substT body
                                                                  $ wkArg (wkBy $ ctxLen ctx') <$> args
                    go ctx' substD substT (b:binds) = case b of
                      TermBind _ s (VarDecl _ _ _) body ->
                        let result = interp_ (ctx <> ctx') substD substT body []
                            locs result = if s == Strict then topLevelLocations result else mempty
                        in case result of
                            -- TODO: not the fastest way to do this (datatypes in the continuation?)
                            DError | s == Strict -> second (const DError)
                                                    $ go ctx' (substD :> result) substT binds
                            DUnion ds            ->
                              let rs = [second (addLocations (locs result))
                                                                $ go ctx' (substD :> result) substT binds
                                                            | result <- ds ]
                              in (fst (head rs), dUnions (map snd rs))
                            _                    -> second (addLocations (locs result))
                                                    $ go ctx' (substD :> result) substT binds

                      -- TODO: remove (also wrong?)
                      TypeBind _ (TyVarDecl _ _ _) ty    ->
                        go ctx' substD (substT :> interpTy (ctx <> ctx') substT ty []) binds

                      DatatypeBind _ dat@(Datatype _ (TyVarDecl _ n _) pars _ constrs) ->
                        let (dDat, dMatch, dConstrs) = mkDat n pars constrs Nil  -- dDat == Var 0
                            normDat = interpDat (ctx <> ctx') substT dat False -- :: DDat (ctx <> ctx')
                            substD' = foldl (:>) (wkD (wkBy 1) <$> substD) (dMatch : dConstrs)
                        in go (ctx' :> TyCtxDat normDat) substD' (fmap (wkT (wkBy 1)) substT :> dDat) binds

                Error{} -> (Nil, DError)

                Var _ nm@(NamedDeBruijn _ idx) ->
                  traceDoc Low ("interp: lookupVar" <+> pretty nm)
                    (Nil, domApps ctx (lookupSubst substD idx) args)

                TyAbs _ x k t
                  | TyArg a : args' <- args -> (Nil, interp_ ctx substD (substT :> a) t args')

                  | [] <- args              -> (Nil, DTySusp x k mempty $ interp_ (ctx :> x ::: k)
                                                                                  (wkD (wkBy 1) <$> substD)
                                                                                  (fmap (wkT $ wkBy 1) substT :> DTVar x [])
                                                                                  t []) -- x is guaranteed to be 0 + a name

                  | otherwise              -> error "interp: TyAbs"

                (LamAbs _ x a t)
                  | DArg (DUnion ds) : args' <- args -> (Nil, dUnions [ app d args' | d <- ds ])

                  | DArg d : args' <- args -> (Nil, app d args')

                  | [] <- args -> (Nil, DLam x (interpTy ctx substT a []) substD substT t mempty)

                  | otherwise -> error "interp: LamAbs"
                  where app d args' = addLocations (topLevelLocations d) $ interp_ ctx (substD :> d) substT t args'

                Apply _ t t' -> case interp_ ctx substD substT t' [] of
                  DError -> (Nil, DError)
                  d      -> (Nil, interp_ ctx substD substT t (DArg d : args))

                TyInst _ t a -> (Nil, interp_ ctx substD substT t (TyArg (interpTy ctx substT a []) : args))

                Constant _ _
                  | [] <- args -> (Nil, dTop (DTyBuiltin $ Star) aggro mempty)
                  | otherwise   -> error "interp: Constant"

                Builtin _ b -> (Nil, domApps ctx (dTop (interpTy ctx substT
                                                                 (toDeBruijn_Typ [] $ typeOfBuiltinFunction b) [])
                                                       aggro mempty) args)

                IWrap{} -> error "interp: IWrap"
                Unwrap{} -> error "interp: Unwrap"
                where
                  mkDat n pars constrs binds = ( dDat 0 []
                                               , susp $ DMatch (dDat (length pars) args) mempty
                                               , [ susp $ DConstr (dDat (length pars) args) i Nil mempty
                                                 | i <- [0..length constrs - 1] ])
                    where
                      susp = foldr (.) id [ DTySusp x k mempty | TyVarDecl _ x k <- pars ]
                      args = reverse [ DTVar (setDbIndex x i) [] | (i, TyVarDecl _ x _ ) <- zip [0..] (reverse pars) ]
                      -- Compute db index for a datatype by counting number of data type binds remaining.
                      dDat k = DTVar (setDbIndex n $ fromIntegral $ k + length [() | DatatypeBind{} <- toList binds])

interpCode :: HasCallStack => CompiledCodeIn DefaultUni DefaultFun a -> (TyCtx, Dom)
interpCode cc = interp Nil Nil Nil (getTrm cc) []

allNonFailLocations :: HasCallStack => CompiledCodeIn DefaultUni DefaultFun a -> Set CoverageAnnotation
allNonFailLocations = uncurry allLocations . interpCode

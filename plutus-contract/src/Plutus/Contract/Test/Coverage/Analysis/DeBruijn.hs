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
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutus.Contract.Test.Coverage.Analysis.DeBruijn where

import Control.Arrow hiding ((<+>))
import Data.List hiding (insert)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import GHC.Stack
import PlutusCore.DeBruijn hiding (DeBruijn)
import PlutusCore.Name
import PlutusIR
import PlutusTx.Code
import PlutusTx.Code qualified as PlutusTx

import Plutus.Contract.Test.Coverage.Analysis.Common

-- *** Conversion to DeBruijn

type DBCtx nm = [nm]

class Eq n => IsName n where
  type DeBruijn n
  mkDeBruijn :: n -> Index -> DeBruijn n

instance IsName Name where
  type DeBruijn Name = NamedDeBruijn
  mkDeBruijn (Name t _) i = NamedDeBruijn t i

instance IsName TyName where
  type DeBruijn TyName = NamedTyDeBruijn
  mkDeBruijn (TyName n) i = NamedTyDeBruijn (mkDeBruijn n i)

class IsDbName n where
  setDbIndex :: n -> Index -> n
  getDbIndex :: n -> Index

instance IsDbName NamedDeBruijn where
  setDbIndex (NamedDeBruijn x _) i = NamedDeBruijn x i
  getDbIndex (NamedDeBruijn _ i) = i

instance IsDbName NamedTyDeBruijn where
  setDbIndex (NamedTyDeBruijn x) i = NamedTyDeBruijn (setDbIndex x i)
  getDbIndex (NamedTyDeBruijn x) = getDbIndex x

deBruijn :: HasCallStack => IsName n => DBCtx n -> n -> DeBruijn n
deBruijn ctx n = case findIndex (==n) ctx of
  Nothing -> error "no dumb here - this is a no dumb area"
  Just i  -> mkDeBruijn n $ fromIntegral i

extendDBCtx :: HasCallStack => DBCtx n -> n -> DBCtx n
extendDBCtx = flip (:)

toDeBruijn_Trm :: HasCallStack => DBCtx TyName -> DBCtx Name -> Trm' -> Trm
toDeBruijn_Trm tyCtx trmCtx trm = case trm of
  Let _ rec binds body
    | Rec <- rec ->
      let (tyCtx', trmCtx') = foldl bindCtx_Bind (tyCtx, trmCtx) binds in
      Let () rec (toDeBruijn_Bind True tyCtx' trmCtx' <$> binds) (toDeBruijn_Trm tyCtx' trmCtx' body)
    | otherwise  ->
      let (binds', (tyCtx', trmCtx')) = go' (tyCtx, trmCtx) binds in
      Let () rec binds' (toDeBruijn_Trm tyCtx' trmCtx' body)
      where
        go' ctxs@(tyCtx, trmCtx) (bind :| binds) = first (toDeBruijn_Bind False tyCtx trmCtx bind :|)
                                                         (go (bindCtx_Bind ctxs bind) binds)
        go ctxs []                           = ([], ctxs)
        go ctxs@(tyCtx, trmCtx) (bind:binds) = first (toDeBruijn_Bind False tyCtx trmCtx bind :)
                                                     (go (bindCtx_Bind ctxs bind) binds)

  Error _ ty -> Error () (toDeBruijn_Typ tyCtx ty)

  Var _ x -> Var () $ deBruijn trmCtx x

  TyAbs _ x k t -> TyAbs () (mkDeBruijn x 0) k $ toDeBruijn_Trm (extendDBCtx tyCtx x) trmCtx t

  LamAbs _ x a t -> LamAbs () (mkDeBruijn x 0) (toDeBruijn_Typ tyCtx a) $
                                toDeBruijn_Trm tyCtx (extendDBCtx trmCtx x) t

  Apply _ t t' -> Apply () (toDeBruijn_Trm tyCtx trmCtx t) (toDeBruijn_Trm tyCtx trmCtx t')

  TyInst _ t a -> TyInst () (toDeBruijn_Trm tyCtx trmCtx t) (toDeBruijn_Typ tyCtx a)

  Constant _ c -> Constant () c

  Builtin _ b  -> Builtin () b

  IWrap{}      -> error "toDeBruijn_Trm: IWrap"
  Unwrap{}     -> error "toDeBruijn_Trm: Unwrap"

toDeBruijn_Typ :: HasCallStack => DBCtx TyName -> Typ' -> Typ
toDeBruijn_Typ tyCtx a = case a of
  TyVar _ x        -> TyVar () (deBruijn tyCtx x)
  TyBuiltin _ b    -> TyBuiltin () b
  TyFun _ a b      -> TyFun () (toDeBruijn_Typ tyCtx a) (toDeBruijn_Typ tyCtx b)
  TyForall _ x k a -> TyForall () (mkDeBruijn x 0) k (toDeBruijn_Typ (extendDBCtx tyCtx x) a)
  TyLam _ x k a    -> TyLam () (mkDeBruijn x 0) k (toDeBruijn_Typ (extendDBCtx tyCtx x) a)
  TyApp _ a b      -> TyApp () (toDeBruijn_Typ tyCtx a) (toDeBruijn_Typ tyCtx b)
  TyIFix _ _ _     -> error "normalizeType: TyIFix"

bindCtx_Dat :: HasCallStack => (DBCtx TyName, DBCtx Name) -> Dat' -> (DBCtx TyName, DBCtx Name)
bindCtx_Dat (tyCtx, trmCtx) (Datatype _ (TyVarDecl _ n _) _ match constrs) =
  (extendDBCtx tyCtx n, foldl extendDBCtx trmCtx $ match : [ x | VarDecl _ x _ <- constrs])

toDeBruijn_Dat :: HasCallStack => Bool -> DBCtx TyName -> Dat' -> Dat
toDeBruijn_Dat rec tyCtx (Datatype _ (TyVarDecl _ n k) args match constrs) =
  let tyCtx' = foldl extendDBCtx tyCtx ([ n | not rec ] ++ [ n | TyVarDecl _ n _ <- args ]) in
  Datatype () (TyVarDecl () (mkDeBruijn n 0) k)
              [TyVarDecl () (mkDeBruijn n 0) k | TyVarDecl _ n k <- args]
              (mkDeBruijn match 0)
              [VarDecl () (mkDeBruijn c 0) (toDeBruijn_Typ tyCtx' ty) | VarDecl _ c ty <- constrs]

bindCtx_Bind :: HasCallStack => (DBCtx TyName, DBCtx Name) -> Bind' -> (DBCtx TyName, DBCtx Name)
bindCtx_Bind (tyCtx, trmCtx) (TermBind _ _ (VarDecl _ x _) _) = (tyCtx, extendDBCtx trmCtx x)
bindCtx_Bind (tyCtx, trmCtx) (TypeBind _ (TyVarDecl _ x _) _) = (extendDBCtx tyCtx x, trmCtx)
bindCtx_Bind ctxs (DatatypeBind _ dat)                        = bindCtx_Dat ctxs dat

toDeBruijn_Bind :: HasCallStack => Bool -> DBCtx TyName -> DBCtx Name -> Bind' -> Bind
toDeBruijn_Bind _ tyCtx trmCtx (TermBind _ s (VarDecl _ x ty) body) =
  TermBind () s (VarDecl () (mkDeBruijn x 0) (toDeBruijn_Typ tyCtx ty)) (toDeBruijn_Trm tyCtx trmCtx body)
toDeBruijn_Bind _ tyCtx _ (TypeBind _ (TyVarDecl _ x k) ty) =
  TypeBind () (TyVarDecl () (mkDeBruijn x 0) k) (toDeBruijn_Typ tyCtx ty)
toDeBruijn_Bind r tyCtx _ (DatatypeBind _ dat) =
  DatatypeBind () (toDeBruijn_Dat r tyCtx dat)

getTrm :: HasCallStack => CompiledCode a -> Trm
getTrm cc = let Program _ t = fromJust $ PlutusTx.getPir cc in toDeBruijn_Trm [] [] t

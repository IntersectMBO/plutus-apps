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

module Plutus.Contract.Test.Coverage.Analysis.Pretty where

import Control.Arrow hiding ((<+>))
import Control.Lens ((^.))
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import PlutusCore.Core (TyDecl (..))
import PlutusCore.DeBruijn hiding (DeBruijn)
import PlutusCore.Default
import PlutusCore.Name
import PlutusIR
import PlutusTx.Code qualified as PlutusTx
import PlutusTx.Coverage
import Prettyprinter qualified as Pp
import Test.QuickCheck
import Text.PrettyPrint hiding (integer, (<>))
import Text.Read (readMaybe)

import UntypedPlutusCore qualified as UPLC

import Plutus.Contract.Test.Coverage.Analysis.Types

{- Note [Prettyprinting of PIR in plutus-apps]
   This code will be migrated to plutus-core in a future release. However, in order to make
   the static anylsis code in `Plutus.Contract.Test.Coverage.Analysis.Interpreter` debuggable
   now we need this here until that is done. Therefore, we will have to accept the slight
   inconvenience of this being in the wrong place (and using the wrong prettyprinting library)
   for now.
-}

-- Sane pretty printer for Plutus IR

class Pretty a where
  pretty     :: a -> Doc
  prettyPrec :: Int -> a -> Doc

  pretty = prettyPrec 0
  prettyPrec _ = pretty

  {-# MINIMAL pretty | prettyPrec #-}

(<?>) :: Doc -> Doc -> Doc
a <?> b = hang a 2 b

pParen :: Bool -> Doc -> Doc
pParen False = id
pParen True  = parens

type PrettyTm tyname name uni fun = (Eq tyname, Pretty tyname
                                    , Pretty name, Pretty (SomeTypeIn uni)
                                    , Pretty (Some (ValueOf uni)), Pretty fun)
type PrettyTy tyname uni = (Eq tyname, Pretty tyname, Pretty (SomeTypeIn uni))

instance Pretty Text.Text where
  pretty = text . Text.unpack

instance Pretty (PlutusTx.CompiledCode a) where
  pretty = maybe "Nothing" pretty . PlutusTx.getPir

instance PrettyTm tyname name uni fun => Pretty (Program tyname name uni fun ann) where
  prettyPrec p (Program _ t) = prettyPrec p t

instance Pretty (SomeTypeIn DefaultUni) where
  pretty = text . show . Pp.pretty

instance Pretty (Some (ValueOf DefaultUni)) where
  pretty c = case readMaybe =<< readMaybe s of
      Just ann -> pretty (ann :: CoverageAnnotation)
      Nothing  -> text s
    where s = show (Pp.pretty c)

instance Pretty DefaultFun where
  pretty = text . show . Pp.pretty

instance Pretty Name where
  pretty (Name x u)
    | isDead x  = "_"
    | otherwise = text . (++ ("{" ++ show (unUnique u) ++ "}")) . show . Pp.pretty $ x
    where
      isDead x = show (Pp.pretty x) == "dead"

instance Pretty NamedDeBruijn where
  pretty (NamedDeBruijn x idx)
    | isDead x  = "_"
    | otherwise = text . (++ ("{" ++ show idx ++ "}")) . show . Pp.pretty $ x
    where
      isDead x = show (Pp.pretty x) == "dead"

instance Pretty TyName where
  pretty (TyName x) = pretty x

instance Pretty NamedTyDeBruijn where
  pretty (NamedTyDeBruijn x) = pretty x

instance Pretty CoverageAnnotation where
  pretty (CoverLocation loc) = hcat ["\"", pretty loc, "\""]
  pretty (CoverBool loc b)   = hcat ["\"", pretty loc, if b then "/True" else "/False", "\""]

instance Pretty CovLoc where
  pretty (CovLoc _ l1 l2 c1 c2)
    | l1 Prelude.== l2 = text $ concat [show l1, ":", show c1, "-", show c2]
    | otherwise        = text $ concat [show l1, ":", show c1, "-", show l2, ":", show c2]

instance Pretty (Kind ann) where
  prettyPrec _ (Type _)           = "*"
  prettyPrec p (KindArrow _ k k') = pParen (p > 1) $ sep [prettyPrec 2 k, "->" <+> prettyPrec 1 k']

ppTyBind :: Pretty tyname => (tyname, Kind ann) -> Doc
ppTyBind (x, Type{}) = pretty x
ppTyBind (x, k)      = parens (pretty x <+> ":" <+> pretty k)

ppAbstr :: Pretty b => Int -> (arg -> Doc) -> Doc -> ([arg], b) -> Doc
ppAbstr p ppBind binder (binds, body) = pParen (p > 0)  $ (binder <+> (fsep (map ppBind binds) <> ".")) <?> pretty body

instance PrettyTy tyname uni => Pretty (Type tyname uni ann) where
  prettyPrec p a = case a of
    TyVar _ x     -> pretty x
    TyBuiltin _ c -> pretty c
    TyFun _ a b   -> pParen (p > 1)  $ sep [prettyPrec 2 a, "->" <+> prettyPrec 1 b]
    TyIFix _ a b  -> pParen (p > 10) $ "Fix" <+> sep [prettyPrec 11 a, prettyPrec 11 b]
    -- TyForall _ x Type{} (TyVar _ x') | x == x' -> "⊥"
    TyForall{}    -> ppAbstr p ppTyBind "∀" (view a)
      where
        view (TyForall _ x k b) = first ((x, k):) $ view b
        view a                  = ([], a)
    TyLam{}     -> ppAbstr p ppTyBind "Λ" (viewLam a)
      where
        viewLam (TyLam _ x k b) = first ((x, k):) $ viewLam b
        viewLam b               = ([], b)
    TyApp{} -> pParen (p > 10) $ prettyPrec 10 hd <?> fsep (map (prettyPrec 11) args)
      where
        (hd, args) = viewApp a []
        viewApp (TyApp _ a b) args = viewApp a (b : args)
        viewApp a args             = (a, args)

-- data Binding tyname name uni fun a = TermBind a Strictness (VarDecl tyname name uni fun a) (Term tyname name uni fun a)
--                            | TypeBind a (TyVarDecl tyname a) (Type tyname uni a)
--                            | DatatypeBind a (Datatype tyname name uni fun a)

instance PrettyTm tyname name uni fun => Pretty (Binding tyname name uni fun ann) where
  pretty bind = case bind of
    TermBind _ s vdec t -> (pretty vdec <+> eq) <?> pretty t
      where
        eq | PlutusIR.Strict <- s = "[!]="
           | otherwise            = "[~]="
    TypeBind _ tydec a -> (pretty tydec <+> "=") <?> pretty a
    DatatypeBind _ dt -> pretty dt

-- data Datatype tyname name uni fun a = Datatype a (TyVarDecl tyname a) [TyVarDecl tyname a] name [VarDecl tyname name uni fun a]

instance PrettyTy tyname uni => Pretty (TyDecl tyname uni ann) where
  prettyPrec p (TyDecl _ x k) = pParen (p > 0) $ pretty x <+> ":" <+> pretty k

instance Pretty tyname => Pretty (TyVarDecl tyname ann) where
  prettyPrec p (TyVarDecl _ x k) = pParen (p > 0) $ ppTyBind (x, k)

instance (PrettyTy tyname uni, Pretty name) => Pretty (VarDecl tyname name uni fun ann) where
  prettyPrec p (VarDecl _ x a) = pParen (p > 0) $ pretty x <+> ":" <+> pretty a

instance PrettyTm tyname name uni fun => Pretty (Datatype tyname name uni fun ann) where
  pretty (Datatype _ tydec pars name cs) =
    vcat [ "data" <+> pretty tydec <+> fsep (map pretty pars) <+> "/" <+> pretty name <+> "where"
         , nest 2 $ vcat $ map pretty cs ]

instance PrettyTm tyname name uni fun => Pretty (Term tyname name uni fun ann) where
  prettyPrec p t = case t of
    Let _ rec binds body -> pParen (p > 0) $ sep [kw <+> vcat (map pretty $ toList binds), "in" <+> pretty body]
      where
        kw | Rec <- rec = "letrec"
           | otherwise  = "let"
    Var _ x              -> pretty x
    TyAbs{}              -> ppAbstr p ppTyBind "Λ" (viewLam t)
      where
        viewLam (TyAbs _ x k b) = first ((x, k):) $ viewLam b
        viewLam b               = ([], b)
    LamAbs{}             -> ppAbstr p (prettyPrec 1) "λ" (viewLam t)
      where
        viewLam (LamAbs _ x a t) = first (VarDecl undefined x a:) $ viewLam t
        viewLam t                = ([], t)
    Apply{}              -> ppApp p t
    TyInst{}             -> ppApp p t
    Constant _ c         -> pretty c
    Builtin _ b          -> pretty b
    Error _ ty           -> pParen (p > 0) $ "error" <+> ":" <+> pretty ty
    IWrap _ a b t        -> ppApp' p "Wrap" [Left a, Left b, Right t]
    Unwrap _ t           -> ppApp' p "unwrap" [Right t]

instance Pretty a => Pretty (Set a) where
  pretty = braces . fsep . punctuate comma . map pretty . Set.toList

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty = brackets . fsep . punctuate comma . map pretty

instance Pretty a => Pretty (SnocList a) where
  pretty = angles . fsep . punctuate comma . map pretty . toList

instance Pretty TyCtxEntry where
  pretty (n ::: k)        = ppTyBind (n, k)
  pretty (TyCtxDat d)     = pretty d
  pretty (TyCtxRecDat ds) = pretty ds

instance Pretty DDat where
  pretty (DDat r n k pars cons) = (if r then "recdata" else "data") <+> ppTyBind (n, k) <+> fsep (map pretty pars) <+> "where" <?> (braces . fsep . punctuate comma $ map pretty cons)

instance Pretty DCon where
  pretty (DCon ds) = pretty ds

ppApp :: PrettyTm tyname name uni fun => Int -> Term tyname name uni fun ann -> Doc
ppApp p t = uncurry (ppApp' p . prettyPrec 10) (viewApp t)

ppApp' :: PrettyTm tyname name uni fun => Int -> Doc -> [Either (Type tyname uni ann) (Term tyname name uni fun ann)] -> Doc
ppApp' p hd args = pParen (p > 10) $ hd <?> fsep (map ppArg args)
  where
    ppArg (Left a)  = "@" <> prettyPrec 11 a
    ppArg (Right t) = prettyPrec 11 t

viewApp :: Term tyname name uni fun ann -> (Term tyname name uni fun ann, [Either (Type tyname uni ann) (Term tyname name uni fun ann)])
viewApp t = go t []
  where
    go (Apply _ t s)  args = go t (Right s : args)
    go (TyInst _ t a) args = go t (Left a : args)
    go t args              = (t, args)

ppSubst :: Pretty a => Subst a -> Doc
ppSubst subst = braces $ fsep $ punctuate comma [ pretty d | d <- toList subst ]

angles :: Doc -> Doc
angles d = hcat ["<", d, ">"]

instance Pretty DTyp where
  prettyPrec p t = case t of
    DTVar x []     -> pretty x
    DTVar x ts     -> pParen (p > 10) $ pretty x <?> fsep (map (prettyPrec 11) ts)
    DTFun s t      -> pParen (p > 5) $ prettyPrec 6 s <?> ("->" <+> prettyPrec 5 t)
    DTLam x k t    -> pParen (p > 0) $ "Λ" <+> (ppTyBind (x, k) <> ".") <+> pretty t
    DTForall x Type{} (DTVar x' [])
      | x == x'    -> "⊥"
    DTForall x k t -> pParen (p > 0) $ "∀" <+> (ppTyBind (x, k) <> ".") <+> pretty t
    DTWk w t       -> pParen (p > 10) $ "Wk" <+> pretty w <+> prettyPrec 11 t
    DTyBuiltin k   -> pParen (p > 10) $ "Builtin" <+> prettyPrec 11 k

instance Pretty DArg where
  prettyPrec _ (TyArg t) = "@" <> prettyPrec 11 t
  prettyPrec p (DArg d)  = prettyPrec p d

instance Pretty Int where
  pretty = text . show

instance Pretty Index where
  pretty (Index i) = text . show $ i

instance Pretty Weakening where
  pretty (Wk w) = pretty w

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = parens $ sep [pretty a <> comma, pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = parens $ sep [pretty a <> comma, pretty b <> comma, pretty c]

instance Pretty Dom where
  prettyPrec p0 d = locs $ case d of
    DTop{..}   -> ("T" <> brackets ((pretty ty <> ",") <+> pretty depth))
    DError{}   -> "error"
    DTySusp{}  -> pParen (p > 0) $ hsep ["Λ", hcat [fsep args, "."]] <?> prettyPrec 11 body
      where
        (args, body) = view d
        ppBind x Type{} = pretty x
        ppBind x k      = parens $ pretty x <+> ":" <+> pretty k
        view (DTySusp x k _ b) = first (ppBind x k:) $ view b
        view b                 = ([], b)
    DSusp{..} -> hcat ["~", prettyPrec 11 inner]
    DTrace{} -> "trace"
    DLoc{..}   -> pretty location
    DLam x ty substD substT body _ -> locs $ angles $ sep [ hcat [ppSubst substD, ","], hcat [ppSubst substT, ","]
                                                          , ("λ" <+> hcat [parens $ pretty x <+> ":" <+> pretty ty, "."]) <?> pretty body ]
    DConstr{..} -> pParen (p > 10) $ (text ("Con" ++ show constr) <> brackets (pretty dat)) <+> fsep (map (prettyPrec 11) $ toList argsD)
    DUnion ds    -> pParen (p > 10) $ brains "|" $ map (prettyPrec 10) ds
    DIf{} -> "DIf"
    DMatch t _ -> "DMatch" <> brackets (pretty t)
    DWeaken w t -> pParen (p > 10) $ "Wk" <+> pretty w <+> prettyPrec 11 t
    where
      p = if null ls then p0 else 0
      ls | DWeaken{} <- d = []
         | DUnion{} <- d  = []
         | otherwise      = Set.toList $ topLevelLocations d
      brains s (x : xs) = sep (x : map (s <+>) xs)
      brains _ []       = error "impossible"
      locs doc
        | null ls   = doc
        | otherwise = pParen (p0 > 10) $ hsep [doc, "@", braces $ fsep $ punctuate comma $ map pretty ls]
        where

instance Pretty CoverageIndex where
  pretty covidx = pretty . map fst . Map.toList $ covidx ^. coverageMetadata

deriving via a instance Pretty a => Pretty (NonNegative a)

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = pretty . Map.toList

instance Pretty Doc where
  pretty = id

instance {-# OVERLAPPING #-} Pretty String where
  pretty = text

instance Pretty Bool where
  pretty = text . show

instance Pretty a => Pretty (Maybe a) where
  prettyPrec _ Nothing  = "Nothing"
  prettyPrec p (Just x) = pParen (p > 10) $ "Just" <+> prettyPrec 11 x

instance Pretty () where
  pretty () = "()"

instance (Pretty name, Pretty (Some (ValueOf uni)), Pretty fun) => Pretty (UPLC.Program name uni fun ann) where
  prettyPrec p (UPLC.Program _ _ t) = prettyPrec p t

instance (Pretty name, Pretty (Some (ValueOf uni)), Pretty fun) => Pretty (UPLC.Term name uni fun ann) where
  prettyPrec p t = case t of
    UPLC.Var _ x      -> pretty x
    UPLC.Constant _ c -> pretty c
    UPLC.Builtin _ b  -> pretty b
    UPLC.Error _      -> text "error"
    UPLC.LamAbs{}     -> pParen (p Prelude.> 0) $ ("λ" <+> fsep (map pretty args) <+> "->") <?> pretty body
      where
        (args, body) = viewLam t
        viewLam (UPLC.LamAbs _ x b) = first (x:) $ viewLam b
        viewLam b                   = ([], b)
    UPLC.Apply{} -> pParen (p Prelude.> 10) $ prettyPrec 10 hd <?> fsep (map (prettyPrec 11) args)
      where
        (hd, args) = viewApp t []
        viewApp (UPLC.Apply _ a b) args = viewApp a (b : args)
        viewApp a args                  = (a, args)
    _ -> hcat (fds ++ [prettyPrec 11 body])
      where
        (fds, body) = viewForce t
        viewForce (UPLC.Force _ t) = first ("!":) $ viewForce t
        viewForce (UPLC.Delay _ t) = first ("~":)  $ viewForce t
        viewForce t                = ([], t)



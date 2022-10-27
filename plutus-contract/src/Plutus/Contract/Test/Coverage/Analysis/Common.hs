{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Contract.Test.Coverage.Analysis.Common where
import Control.DeepSeq
import Data.Text qualified as Text
import Debug.Trace
import GHC.Stack
import PlutusCore.DeBruijn hiding (DeBruijn)
import PlutusCore.Default
import PlutusCore.Name
import PlutusIR
import PlutusIR.Compiler
import PlutusTx.Coverage
import Text.PrettyPrint hiding (integer, (<>))
import Text.Read (readMaybe)

type Trm = Term NamedTyDeBruijn NamedDeBruijn DefaultUni DefaultFun ()
type Typ = Type NamedTyDeBruijn DefaultUni ()
type Kin = Kind ()
type Dat = Datatype NamedTyDeBruijn NamedDeBruijn DefaultUni DefaultFun ()
type Bind = Binding NamedTyDeBruijn NamedDeBruijn DefaultUni DefaultFun ()

type Trm'  = Term TyName Name DefaultUni DefaultFun ()
type Typ'  = Type TyName DefaultUni ()
type Dat'  = Datatype TyName Name DefaultUni DefaultFun ()
type Bind' = Binding TyName Name DefaultUni DefaultFun ()
type Err'  = Error DefaultUni DefaultFun ()

pattern BIF_Trace :: Term tyname name uni DefaultFun ()
pattern BIF_Trace = Builtin () Trace

pattern BIF_If :: Term tyname name uni DefaultFun ()
pattern BIF_If = Builtin () IfThenElse

pattern LIT_Loc :: CoverageAnnotation -> Term tyname name DefaultUni fun ()
pattern LIT_Loc l <- Constant _ (Some (ValueOf DefaultUniString (readMaybe . Text.unpack -> Just l)))
  where LIT_Loc l = Constant () (Some (ValueOf DefaultUniString (Text.pack (show l))))

pattern Const :: DefaultUni (Esc a) -> a -> Term tyname name DefaultUni fun ()
pattern Const b a = Constant () (Some (ValueOf b a))

builtinKind :: SomeTypeIn DefaultUni -> Kin
builtinKind (SomeTypeIn t) = case t of
  DefaultUniProtoList -> Star :-> Star
  DefaultUniProtoPair -> Star :-> Star :-> Star
  DefaultUniApply f _ -> let _ :-> k = builtinKind (SomeTypeIn f) in k
  _                   -> Star

-- *** Debug helpers
data Verbosity = Low
               | Med
               | High
               | Unions
               deriving (Ord, Eq, Show)

debug :: Bool
debug = False

verbosity :: [Verbosity]
verbosity = []

traceDoc :: Verbosity -> Doc -> a -> a
traceDoc v d a | debug && v `elem` verbosity = trace (show d) a
               | otherwise = a

traceDocIf :: Bool -> Verbosity -> Doc -> a -> a
traceDocIf True = traceDoc
traceDocIf _    = \ _ _ a -> a

errorDoc :: HasCallStack => Doc -> a
errorDoc = error . ("\n"++) . show

deriving instance NFData Trm'
deriving instance NFData Bind'
deriving instance NFData (VarDecl TyName Name DefaultUni DefaultFun ())
deriving instance NFData (TyVarDecl TyName ())
deriving instance NFData Dat'
deriving instance NFData Strictness
deriving instance NFData Recursivity

deriving instance NFData (TyVarDecl NamedTyDeBruijn ())
deriving instance NFData Dat
deriving instance NFData (VarDecl NamedTyDeBruijn NamedDeBruijn DefaultUni DefaultFun ())
deriving instance NFData Bind
deriving instance NFData Trm

{-# COMPLETE Star, (:->) #-}
pattern Star :: Kin
pattern Star  = Type ()

pattern (:->) :: Kin -> Kin -> Kin
pattern (:->) a b = KindArrow () a b
infixr 3 :->

{-# LANGUAGE OverloadedStrings #-}
-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes where

import Data.Monoid
import qualified Data.Text as T

import Language.PureScript.Bridge.TypeInfo

-- | Uses  type parameters from existing TypeInfo:
psArray :: TypeInfo -> TypeInfo
psArray t = t {
    typePackage = "purescript-prim"
  , typeModule = "Prim"
  , typeName = "Array"
  }

psBool :: TypeInfo
psBool = TypeInfo {
    typePackage = "purescript-prim"
  , typeModule = "Prim"
  , typeName = "Boolean"
  , typeParameters = []
  }

-- | Uses  type parameters from existing TypeInfo:
psEither :: TypeInfo -> TypeInfo
psEither t = t {
    typePackage = "purescript-either"
  , typeModule = "Data.Either"
  , typeName = "Either"
  }

psInt :: TypeInfo
psInt = TypeInfo {
    typePackage = "purescript-prim"
  , typeModule = "Prim"
  , typeName = "Int"
  , typeParameters = []
  }

-- | Uses  type parameters from existing TypeInfo:
psMaybe :: TypeInfo -> TypeInfo
psMaybe t = t {
    typePackage = "purescript-maybe"
  , typeModule = "Data.Maybe"
  , typeName = "Maybe"
  }


psString :: TypeInfo
psString = TypeInfo {
    typePackage = "purescript-prim"
  , typeModule = "Prim"
  , typeName = "String"
  , typeParameters = []
  }

-- | Uses  type parameters from existing TypeInfo:
psTuple :: TypeInfo -> TypeInfo
psTuple t = t {
      typePackage = "purescript-tuples"
    , typeModule = if size == 2 then "Data.Tuple" else "Data.Tuple.Nested"
    , typeName = "Tuple" <> if size == 2 then "" else T.pack (show size)
    }
  where
    size = length (typeParameters t)

psUnit :: TypeInfo
psUnit = TypeInfo  {
    typePackage = "purescript-prelude"
  , typeModule = "Prelude"
  , typeName = "Unit"
  , typeParameters = []
  }

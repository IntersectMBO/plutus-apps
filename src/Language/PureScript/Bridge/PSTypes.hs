{-# LANGUAGE OverloadedStrings #-}
-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes where

import           Data.Monoid
import qualified Data.Text                           as T

import           Language.PureScript.Bridge.TypeInfo

-- | Uses  type parameters from existing TypeInfo:
psArray :: TypeInfo -> TypeInfo
psArray t = t {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Array"
  }

psBool :: TypeInfo
psBool = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Boolean"
  , _typeParameters = []
  }

-- | Uses  type parameters from existing TypeInfo:
psEither :: TypeInfo -> TypeInfo
psEither t = t {
    _typePackage = "purescript-either"
  , _typeModule = "Data.Either"
  , _typeName = "Either"
  }

psInt :: TypeInfo
psInt = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Int"
  , _typeParameters = []
  }

-- | Uses  type parameters from existing TypeInfo:
psMaybe :: TypeInfo -> TypeInfo
psMaybe t = t {
    _typePackage = "purescript-maybe"
  , _typeModule = "Data.Maybe"
  , _typeName = "Maybe"
  }


psString :: TypeInfo
psString = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "String"
  , _typeParameters = []
  }

-- | Uses  type parameters from existing TypeInfo:
psTuple :: TypeInfo -> TypeInfo
psTuple t = t {
      _typePackage = "purescript-tuples"
    , _typeModule = if size == 2 then "Data.Tuple" else "Data.Tuple.Nested"
    , _typeName = "Tuple" <> if size == 2 then "" else T.pack (show size)
    }
  where
    size = length (_typeParameters t)

psUnit :: TypeInfo
psUnit = TypeInfo  {
    _typePackage = "purescript-prelude"
  , _typeModule = "Prelude"
  , _typeName = "Unit"
  , _typeParameters = []
  }

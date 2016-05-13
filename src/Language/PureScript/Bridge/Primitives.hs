{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Bridge.Primitives where


import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeInfo


boolBridge :: TypeBridge
boolBridge = mkBridgeTo (eqTypeName "Bool") psBool

eitherBridge :: TypeBridge
eitherBridge = mkBridgeTo1 (eqTypeName "Either") psEither

intBridge :: TypeBridge
intBridge = mkBridgeTo (eqTypeName "Int") psInt

listBridge :: TypeBridge
listBridge = mkBridgeTo1 (eqTypeName "[]") psArray

maybeBridge :: TypeBridge
maybeBridge = mkBridgeTo1 (eqTypeName "Maybe") psMaybe

stringBridge :: TypeBridge
stringBridge = mkBridgeTo isStringLike psString
  where
    isStringLike t = isText t || isString t
    isText t = _typeName t == "Text"
    isString t = _typeName t == "[]" && all ((==) "Char" . _typeName) (_typeParameters t)

unitBridge :: TypeBridge
unitBridge = mkBridgeTo (eqTypeName "()") psUnit

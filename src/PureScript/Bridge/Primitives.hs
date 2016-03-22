{-# LANGUAGE OverloadedStrings #-}
module PureScript.Bridge.Primitives where


import PureScript.Bridge.TypeInfo


intBridge :: TypeBridge
intBridge t
  | eqTypeName "Int" t = Just $ t {
      typePackage = "purescript-prim"
    , typeModule = "Prim"
    }
  | otherwise = Nothing


stringBridge :: TypeBridge
stringBridge t
  | isStringLike = Just $ t {
      typePackage = "purescript-prim"
    , typeModule = "Prim"
    , typeName = "String"
    , typeParameters = []
    }
  | otherwise = Nothing
  where
    isStringLike = isText || isString
    isText = typeName t == "Text" && typePackage t == "text"
    isString = typeName t == "[]" && all ((==) "Char" . typeName) (typeParameters t)

listBridge :: TypeBridge
listBridge t
  | eqTypeName "[]" t = Just $ t {
      typePackage = "purescript-prim"
    , typeModule = "Prim"
    , typeName = "Array"
    }
  | otherwise = Nothing

maybeBridge :: TypeBridge
maybeBridge t
  | eqTypeName "Maybe" t = Just $ t {
      typePackage = "purescript-maybe"
    , typeModule = "Data.Maybe"
    }
  | otherwise = Nothing


eitherBridge :: TypeBridge
eitherBridge t
  | eqTypeName "Either" t = Just $ t {
      typePackage = "purescript-either"
    , typeModule = "Data.Either"
    }
  | otherwise = Nothing

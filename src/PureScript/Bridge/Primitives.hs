{-# LANGUAGE OverloadedStrings #-}
module PureScript.Bridge.Primitives where


import PureScript.Bridge.TypeInfo


intBridge :: TypeBridge
intBridge t | eqTypeName "Int" t = Just $ t {
    typePackage = "purescript-prim"
  , typeModule = "Prim"
  }
intBridge _ = Nothing

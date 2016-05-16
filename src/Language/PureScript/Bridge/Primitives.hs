{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Bridge.Primitives where


import           Data.Proxy
import           Language.PureScript.Bridge.Builder
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeInfo

boolBridge :: BridgePart
boolBridge = typeName ^== "Bool" >> return psBool

eitherBridge :: BridgePart
eitherBridge = typeName ^== "Either" >> psEither

intBridge :: BridgePart
intBridge = typeName ^== "Int" >> return psInt

listBridge :: BridgePart
listBridge = typeName ^== "[]" >> psArray

maybeBridge :: BridgePart
maybeBridge = typeName ^== "Maybe" >> psMaybe

stringBridge :: BridgePart
stringBridge = haskType ^== mkTypeInfo (Proxy :: Proxy String ) >> return psString

textBridge :: BridgePart
textBridge = do
    typeName   ^== "Text"
    typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
    return psString

unitBridge :: BridgePart
unitBridge = typeName ^== "()" >> return psUnit

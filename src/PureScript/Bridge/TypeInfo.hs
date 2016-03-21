{-# LANGUAGE OverloadedStrings #-}

module PureScript.Bridge.TypeInfo where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Proxy
import Data.Typeable


-- Basic info about a data type:
data TypeInfo = TypeInfo {
  -- | Hackage package
  typePackage :: Text
  -- | Full Module path
, typeModule :: Text
, typeName :: Text
, typeParameters :: [TypeInfo]
} deriving (Eq, Show)

mkTypeInfo :: Typeable t => Proxy t -> TypeInfo
mkTypeInfo = mkTypeInfo' . typeRep

mkTypeInfo' :: TypeRep -> TypeInfo
mkTypeInfo' rep = let
    con = typeRepTyCon rep
  in TypeInfo {
    typePackage = T.pack $ tyConPackage con
  , typeModule = T.pack $ tyConModule con
  , typeName = T.pack $ tyConName con
  , typeParameters = map mkTypeInfo' (typeRepArgs rep)
  }


-- Translates a Haskell type info to a PureScript type info:
type TypeBridge = TypeInfo -> Maybe TypeInfo

{--
 -- Optimistically and recursively translate types: If the passed TypeBridge returns Nothing,
 -- then the original TypeInfo is returned with the typePackage field cleared.
--}
bridge :: TypeBridge -> TypeInfo -> TypeInfo
bridge br info = let
    translated = info { typePackage = "" }
    res = fromMaybe translated (br info)
  in
    res {
      typeParameters = map (bridge br) $ typeParameters res
    }

eqTypeName :: Text -> TypeInfo -> Bool
eqTypeName name = (== name) . typeName 

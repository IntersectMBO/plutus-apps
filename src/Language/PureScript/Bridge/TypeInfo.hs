{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Language.PureScript.Bridge.TypeInfo where


import           Control.Lens
import           Data.Proxy
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Typeable


-- | Translates a Haskell type info to a PureScript type info:
type TypeBridge = TypeInfo -> Maybe TypeInfo


-- | Basic info about a data type:
data TypeInfo = TypeInfo {
  -- | Hackage package
  _typePackage    :: !Text
  -- | Full Module path
, _typeModule     :: !Text
, _typeName       :: !Text
, _typeParameters :: ![TypeInfo]
} deriving (Eq, Ord, Show)

mkTypeInfo :: Typeable t => Proxy t -> TypeInfo
mkTypeInfo = mkTypeInfo' . typeRep

mkTypeInfo' :: TypeRep -> TypeInfo
mkTypeInfo' rep = let
    con = typeRepTyCon rep
  in TypeInfo {
    _typePackage = T.pack $ tyConPackage con
  , _typeModule = T.pack $ tyConModule con
  , _typeName = T.pack $ tyConName con
  , _typeParameters = map mkTypeInfo' (typeRepArgs rep)
  }

-- | Put the TypeInfo in a list together with all its _typeParameters (recursively)
flattenTypeInfo :: TypeInfo -> [TypeInfo]
flattenTypeInfo t = t : concatMap flattenTypeInfo (_typeParameters t)

-- | Little helper for type bridge implementers
eqTypeName :: Text -> TypeInfo -> Bool
eqTypeName name = (== name) . _typeName

-- | Helper for simple bridge creation for basic types
mkBridgeTo :: (TypeInfo -> Bool) -> TypeInfo -> TypeBridge
mkBridgeTo match r t
  | match t = Just r
  | otherwise = Nothing

-- | Helper for simple bridge creation for type constructors
mkBridgeTo1 :: (TypeInfo -> Bool) -> (TypeInfo -> TypeInfo) -> TypeBridge
mkBridgeTo1 match r t
  | match t = Just $ r t
  | otherwise = Nothing

makeLenses ''TypeInfo

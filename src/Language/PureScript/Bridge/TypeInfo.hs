{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.PureScript.Bridge.TypeInfo (
 TypeInfo
 , mkTypeInfo
 , mkTypeInfo'
) where


import           Control.Lens
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Typeable
import           Unsafe.Coerce                       (unsafeCoerce)

import           Language.PureScript.Bridge.Internal

mkTypeInfo :: Typeable t => Proxy t -> TypeInfo 'Haskell
mkTypeInfo = mkTypeInfo' . typeRep

mkTypeInfo' :: TypeRep -> TypeInfo 'Haskell
mkTypeInfo' rep = let
    con = typeRepTyCon rep
  in TypeInfo {
    _typePackage = T.pack $ tyConPackage con
  , _typeModule = T.pack $ tyConModule con
  , _typeName = T.pack $ tyConName con
  , _typeParameters = map mkTypeInfo' (typeRepArgs rep)
  }



-- | Put the TypeInfo in a list together with all its _typeParameters (recursively)
flattenTypeInfo :: TypeInfo lang -> [TypeInfo lang]
flattenTypeInfo t = t : concatMap flattenTypeInfo (_typeParameters t)

-- | Little helper for type bridge implementers
eqTypeName :: Text -> TypeInfo lang -> Bool
eqTypeName name = (== name) . _typeName

-- | Helper for simple bridge creation for basic types
mkBridgeTo :: (TypeInfo 'Haskell -> Bool) -> TypeInfo 'PureScript -> TypeBridge
mkBridgeTo match r t
  | match t = Just r
  | otherwise = Nothing

-- | Helper for simple bridge creation for type constructors
mkBridgeTo1 :: (TypeInfo 'Haskell -> Bool) -> (TypeInfo 'Haskell -> TypeInfo 'PureScript) -> TypeBridge
mkBridgeTo1 match r tHask
  | match tHask = Just $ r tHask
  | otherwise = Nothing

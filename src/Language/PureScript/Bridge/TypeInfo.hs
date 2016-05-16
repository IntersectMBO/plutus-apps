{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Language.PureScript.Bridge.TypeInfo (
 TypeInfo (..)
 , mkTypeInfo
 , mkTypeInfo'
 , Language (..)
 , typePackage
 , typeModule
 , typeName
 , typeParameters
 , HasHaskType
 , haskType
 , flattenTypeInfo
) where


import           Control.Lens
import           Data.Proxy
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Typeable

data Language = Haskell | PureScript

-- | Basic info about a data type:
data TypeInfo (lang :: Language) = TypeInfo {
  -- | Hackage package
  _typePackage    :: !Text
  -- | Full Module path
, _typeModule     :: !Text
, _typeName       :: !Text
, _typeParameters :: ![TypeInfo lang]
} deriving (Eq, Ord, Show)

makeLenses ''TypeInfo

-- | Types that have a lens for accessing a 'TypeInfo Haskell'.
class HasHaskType t where
  haskType :: Lens' t (TypeInfo 'Haskell)

-- | Simple 'id' instance: Get the 'TypeInfo' itself.
instance HasHaskType (TypeInfo 'Haskell) where
  haskType inj = inj


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

-- | Put the TypeInfo in a list together with all its '_typeParameters' (recursively)
flattenTypeInfo :: TypeInfo lang -> [TypeInfo lang]
flattenTypeInfo t = t : concatMap flattenTypeInfo (_typeParameters t)

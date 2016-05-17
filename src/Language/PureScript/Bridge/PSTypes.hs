{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes where

import           Control.Lens                        (views)
import           Data.Monoid
import qualified Data.Text                           as T
import           Control.Monad.Reader.Class


import           Language.PureScript.Bridge.Builder
import           Language.PureScript.Bridge.TypeInfo

-- | Uses  type parameters from 'haskType' (bridged).
psArray :: MonadReader BridgeData m => m (TypeInfo 'PureScript)
psArray = TypeInfo "purescript-prim" "Prim" "Array" <$> psTypeParameters

psBool :: TypeInfo 'PureScript
psBool = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Boolean"
  , _typeParameters = []
  }

-- | Uses  type parameters from 'haskType' (bridged).
psEither :: MonadReader BridgeData m => m (TypeInfo 'PureScript)
psEither = TypeInfo "purescript-either" "Data.Either" "Either" <$> psTypeParameters

psInt :: TypeInfo 'PureScript
psInt = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Int"
  , _typeParameters = []
  }

-- | Uses  type parameters from 'haskType' (bridged).
psMaybe :: MonadReader BridgeData m => m (TypeInfo 'PureScript)
psMaybe = TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" <$> psTypeParameters

psString :: TypeInfo 'PureScript
psString = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "String"
  , _typeParameters = []
  }

-- | Uses  type parameters from 'haskType' (bridged).
psTuple :: MonadReader BridgeData m => m (TypeInfo 'PureScript)
psTuple = do
  size <- views (haskType . typeParameters) length
  let
    tupleModule = if size == 2 then "Data.Tuple" else "Data.Tuple.Nested"
    tupleName = "Tuple" <> if size == 2 then "" else T.pack (show size)
  TypeInfo "purescript-tuples" tupleModule tupleName <$> psTypeParameters

psUnit :: TypeInfo 'PureScript
psUnit = TypeInfo  {
    _typePackage = "purescript-prelude"
  , _typeModule = "Prelude"
  , _typeName = "Unit"
  , _typeParameters = []
  }

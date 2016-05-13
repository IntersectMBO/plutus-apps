{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}



module Language.PureScript.Bridge.SumType where

import           Control.Lens                        hiding (from, to)
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Typeable
import           Generics.Deriving

import           Language.PureScript.Bridge.TypeInfo

-- | Generic representation of your Haskell types, the contained (leaf) types can be modified to match
--   compatible PureScript types, by using 'TypeBridge' functions like 'defaultBridge' with 'writePSTypes'.
data SumType = SumType TypeInfo [DataConstructor] deriving Show

-- | TypInfo lens for SumType:
sumTypeInfo :: Functor f => (TypeInfo -> f TypeInfo ) -> SumType -> f SumType
sumTypeInfo inj (SumType info constrs) = flip SumType constrs <$> inj info

-- | DataConstructor lens for SumType:
sumTypeConstructors :: Functor f => ([DataConstructor] -> f [DataConstructor]) -> SumType -> f SumType
sumTypeConstructors inj (SumType info constrs) = SumType info <$> inj constrs

-- | Create a representation of your sum (and product) types,
--   for doing type translations and writing it out to your PureScript modules.
--   In order to get the type information we use a dummy variable of type Proxy (YourType).
mkSumType :: forall t. (Generic t, Typeable t, GDataConstructor (Rep t)) => Proxy t -> SumType
mkSumType p = SumType  (mkTypeInfo p) constructors
  where
    constructors = gToConstructors (from (undefined :: t))

data DataConstructor = DataConstructor {
  _sigConstructor :: !Text
, _sigValues      :: !(Either [TypeInfo] [RecordEntry])
} deriving Show


data RecordEntry = RecordEntry {
  _recLabel :: !Text
, _recValue :: !TypeInfo
} deriving Show

class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor]

class GRecordEntry f where
  gToRecordEntries :: f a -> [RecordEntry]

instance (Datatype a, GDataConstructor c) =>  GDataConstructor (D1 a c) where
  gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
  gToConstructors (_ :: (a :+: b) f) = gToConstructors (undefined :: a f) ++ gToConstructors (undefined :: b f)

instance (Constructor a, GRecordEntry b) => GDataConstructor (C1 a b) where
  gToConstructors c@(M1 r) = [
        DataConstructor {
          _sigConstructor = constructor
        , _sigValues = values
        }
      ]
    where
      constructor = T.pack $ conName c
      values = if conIsRecord c
        then Right $ gToRecordEntries r
        else Left $ map _recValue $ gToRecordEntries r

instance (GRecordEntry a, GRecordEntry b) => GRecordEntry (a :*: b) where
  gToRecordEntries (_ :: (a :*: b) f) = gToRecordEntries (undefined :: a f) ++ gToRecordEntries (undefined :: b f)


instance GRecordEntry U1 where
  gToRecordEntries _ = []

instance (Selector a, Typeable t) => GRecordEntry (S1 a (K1 R t)) where
  gToRecordEntries e = [
      RecordEntry { _recLabel = T.pack (selName e)
      , _recValue = mkTypeInfo (Proxy :: Proxy t)
      }
    ]

getUsedTypes :: SumType -> [TypeInfo]
getUsedTypes (SumType _ cs) = foldr constructorToType [] cs

constructorToType :: DataConstructor -> [TypeInfo] -> [TypeInfo]
constructorToType (DataConstructor _ (Left myTs)) ts = concatMap flattenTypeInfo myTs ++ ts
constructorToType (DataConstructor _ (Right rs))  ts = concatMap (flattenTypeInfo . _recValue) rs ++ ts

-- Lenses:
makeLenses ''DataConstructor
makeLenses ''RecordEntry

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Language.PureScript.Bridge.SumType where

import Generics.Deriving
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import Data.Typeable

import Language.PureScript.Bridge.TypeInfo

-- | Generic representation of your Haskell types, the contained (leaf) types can be modified to match
--   compatible PureScript types, by using 'TypeBridge' functions like 'defaultBridge' with 'writePSTypes'.
data SumType = SumType TypeInfo [DataConstructor] deriving Show

-- | Create a representation of your sum (and product) types,
--   for doing type translations and writing it out to your PureScript modules.
--   In order to get the type information we use a dummy variable of type Proxy (YourType).
toSumType :: forall t. (Generic t, Typeable t, GDataConstructor (Rep t)) => Proxy t -> SumType
toSumType p = SumType  (mkTypeInfo p) constructors
  where
    constructors = gToConstructors (from (undefined :: t))

data DataConstructor = DataConstructor {
  sigConstructor :: !Text
, sigValues :: !(Either [TypeInfo] [RecordEntry])
} deriving Show

data RecordEntry = RecordEntry {
  recLabel :: !Text
, recValue :: !TypeInfo
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
          sigConstructor = constructor
        , sigValues = values
        }
      ]
    where
      constructor = T.pack $ conName c
      values = if conIsRecord c
        then Right $ gToRecordEntries r
        else Left $ map recValue $ gToRecordEntries r

instance (GRecordEntry a, GRecordEntry b) => GRecordEntry (a :*: b) where
  gToRecordEntries (_ :: (a :*: b) f) = gToRecordEntries (undefined :: a f) ++ gToRecordEntries (undefined :: b f)


instance GRecordEntry U1 where
  gToRecordEntries _ = []

instance (Selector a, Typeable t) => GRecordEntry (S1 a (K1 R t)) where
  gToRecordEntries e = [
      RecordEntry { recLabel = T.pack (selName e)
      , recValue = mkTypeInfo (Proxy :: Proxy t)
      }
    ]

getUsedTypes :: SumType -> [TypeInfo]
getUsedTypes (SumType _ cs) = foldr constructorToType [] cs

constructorToType :: DataConstructor -> [TypeInfo] -> [TypeInfo]
constructorToType (DataConstructor _ (Left myTs)) ts = concatMap flattenTypeInfo myTs ++ ts
constructorToType (DataConstructor _ (Right rs))  ts = concatMap (flattenTypeInfo . recValue) rs ++ ts

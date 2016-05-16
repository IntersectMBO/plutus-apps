{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}



module Language.PureScript.Bridge.SumType (
  SumType (..)
, mkSumType
, DataConstructor (..)
, RecordEntry (..)
, getUsedTypes
, constructorToType
, sigConstructor
, sigValues
, sumTypeInfo
, sumTypeConstructors
, recLabel
, recValue
) where

import           Control.Lens                        hiding (from, to)
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Typeable
import           Generics.Deriving

import           Language.PureScript.Bridge.TypeInfo

-- | Generic representation of your Haskell types.
data SumType (lang :: Language) = SumType (TypeInfo lang) [DataConstructor lang] deriving Show

-- | TypInfo lens for 'SumType'.
sumTypeInfo :: Functor f => (TypeInfo lang -> f (TypeInfo lang) ) -> SumType lang -> f (SumType lang)
sumTypeInfo inj (SumType info constrs) = flip SumType constrs <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors :: Functor f => ([DataConstructor lang] -> f [DataConstructor lang]) -> SumType lang -> f (SumType lang)
sumTypeConstructors inj (SumType info constrs) = SumType info <$> inj constrs

-- | Create a representation of your sum (and product) types,
--   for doing type translations and writing it out to your PureScript modules.
--   In order to get the type information we use a dummy variable of type 'Proxy' (YourType).
mkSumType :: forall t. (Generic t, Typeable t, GDataConstructor (Rep t)) => Proxy t -> SumType 'Haskell
mkSumType p = SumType  (mkTypeInfo p) constructors
  where
    constructors = gToConstructors (from (undefined :: t))

data DataConstructor (lang :: Language) = DataConstructor {
  _sigConstructor :: !Text
, _sigValues      :: !(Either [TypeInfo lang] [RecordEntry lang])
} deriving Show


data RecordEntry (lang :: Language) = RecordEntry {
  _recLabel :: !Text
, _recValue :: !(TypeInfo lang)
} deriving Show

class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor 'Haskell]

class GRecordEntry f where
  gToRecordEntries :: f a -> [RecordEntry 'Haskell]

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

getUsedTypes :: SumType lang -> [TypeInfo lang]
getUsedTypes (SumType _ cs) = foldr constructorToType [] cs

constructorToType :: DataConstructor lang -> [TypeInfo lang] -> [TypeInfo lang]
constructorToType (DataConstructor _ (Left myTs)) ts = concatMap flattenTypeInfo myTs ++ ts
constructorToType (DataConstructor _ (Right rs))  ts = concatMap (flattenTypeInfo . _recValue) rs ++ ts

-- Lenses:
makeLenses ''DataConstructor
makeLenses ''RecordEntry

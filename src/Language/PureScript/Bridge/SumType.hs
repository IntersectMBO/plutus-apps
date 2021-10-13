{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Bridge.SumType
  ( SumType(..)
  , argonaut
  , mkSumType
  , genericShow
  , functor
  , equal
  , equal1
  , order
  , DataConstructor(..)
  , RecordEntry(..)
  , Instance(..)
  , nootype
  , getUsedTypes
  , constructorToTypes
  , sigConstructor
  , sigValues
  , sumTypeInfo
  , sumTypeConstructors
  , recLabel
  , recValue
  ) where

import           Control.Lens                        hiding (from, to)
import           Data.List                           (nub)
import           Data.Maybe                          (maybeToList)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Typeable
import           Generics.Deriving

import           Language.PureScript.Bridge.TypeInfo

-- | Generic representation of your Haskell types.
data SumType (lang :: Language) =
  SumType (TypeInfo lang) [DataConstructor lang] [Instance]
  deriving (Show, Eq)

-- | TypInfo lens for 'SumType'.
sumTypeInfo ::
     Functor f
  => (TypeInfo lang -> f (TypeInfo lang))
  -> SumType lang
  -> f (SumType lang)
sumTypeInfo inj (SumType info constrs is) =
  (\ti -> SumType ti constrs is) <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors ::
     Functor f
  => ([DataConstructor lang] -> f [DataConstructor lang])
  -> SumType lang
  -> f (SumType lang)
sumTypeConstructors inj (SumType info constrs is) =
  (\cs -> SumType info cs is) <$> inj constrs

-- | Create a representation of your sum (and product) types,
--   for doing type translations and writing it out to your PureScript modules.
mkSumType ::
     forall t. (Generic t, Typeable t, GDataConstructor (Rep t))
  => SumType 'Haskell
mkSumType =
  SumType
    (mkTypeInfo @t)
    constructors
    (Generic : maybeToList (nootype constructors))
  where
    constructors = gToConstructors (from (undefined :: t))

-- | Purescript typeclass instances that can be generated for your Haskell types.
data Instance
  = Generic
  | GenericShow
  | Json
  | Newtype
  | Functor
  | Eq
  | Eq1
  | Ord
  deriving (Eq, Show)

-- | The Purescript typeclass `Newtype` might be derivable if the original
-- Haskell type was a simple type wrapper.
nootype :: [DataConstructor lang] -> Maybe Instance
nootype cs =
  case cs of
    [constr]
      | either isSingletonList (const True) (_sigValues constr) -> Just Newtype
      | otherwise -> Nothing
    _ -> Nothing
  where
    isSingletonList [_] = True
    isSingletonList _   = False

-- | Ensure that aeson-compatible `EncodeJson` and `DecodeJson` instances are generated for your type.
argonaut :: SumType t -> SumType t
argonaut (SumType ti dc is) = SumType ti dc . nub $ Json : is

-- | Ensure that a generic `Show` instance is generated for your type.
genericShow :: SumType t -> SumType t
genericShow (SumType ti dc is) = SumType ti dc . nub $ GenericShow : is

-- | Ensure that a functor instance is generated for your type. It it
-- your responsibility to ensure your type is a functor.
functor :: SumType t -> SumType t
functor (SumType ti dc is) = SumType ti dc . nub $ Functor : is

-- | Ensure that an `Eq` instance is generated for your type.
equal :: SumType t -> SumType t
equal (SumType ti dc is) = SumType ti dc . nub $ Eq : is

-- | Ensure that an `Eq1` instance is generated for your type.
equal1 :: SumType t -> SumType t
equal1 (SumType ti dc is) = SumType ti dc . nub $ Eq1 : is

-- | Ensure that both `Eq` and `Ord` instances are generated for your type.
order :: SumType t -> SumType t
order (SumType ti dc is) = SumType ti dc . nub $ Eq : Ord : is

data DataConstructor (lang :: Language) =
  DataConstructor
    { _sigConstructor :: !Text -- ^ e.g. `Left`/`Right` for `Either`
    , _sigValues      :: !(Either [TypeInfo lang] [RecordEntry lang])
    }
  deriving (Show, Eq)

data RecordEntry (lang :: Language) =
  RecordEntry
    { _recLabel :: !Text -- ^ e.g. `runState` for `State`
    , _recValue :: !(TypeInfo lang)
    }
  deriving (Show, Eq)

class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor 'Haskell]

class GRecordEntry f where
  gToRecordEntries :: f a -> [RecordEntry 'Haskell]

instance (Datatype a, GDataConstructor c) => GDataConstructor (D1 a c) where
  gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) =>
         GDataConstructor (a :+: b) where
  gToConstructors (_ :: (a :+: b) f) =
    gToConstructors (undefined :: a f) ++ gToConstructors (undefined :: b f)

instance (Constructor a, GRecordEntry b) => GDataConstructor (C1 a b) where
  gToConstructors c@(M1 r) =
    [DataConstructor {_sigConstructor = constructor, _sigValues = values}]
    where
      constructor = T.pack $ conName c
      values =
        if conIsRecord c
          then Right $ gToRecordEntries r
          else Left $ map _recValue $ gToRecordEntries r

instance (GRecordEntry a, GRecordEntry b) => GRecordEntry (a :*: b) where
  gToRecordEntries (_ :: (a :*: b) f) =
    gToRecordEntries (undefined :: a f) ++ gToRecordEntries (undefined :: b f)

instance GRecordEntry U1 where
  gToRecordEntries _ = []

instance (Selector a, Typeable t) => GRecordEntry (S1 a (K1 R t)) where
  gToRecordEntries e =
    [ RecordEntry
        { _recLabel = T.pack (selName e)
        , _recValue = mkTypeInfo @t
        }
    ]

-- | Get all used types in a sum type.
--
--   This includes all types found at the right hand side of a sum type
--   definition, not the type parameters of the sum type itself
getUsedTypes :: SumType lang -> Set (TypeInfo lang)
getUsedTypes (SumType _ cs is) = foldMap constructorToTypes cs <> foldMap instanceToTypes is

constructorToTypes ::
     DataConstructor lang -> Set (TypeInfo lang)
constructorToTypes (DataConstructor _ (Left myTs)) =
  Set.fromList (concatMap flattenTypeInfo myTs)
constructorToTypes (DataConstructor _ (Right rs)) =
  Set.fromList (concatMap (flattenTypeInfo . _recValue) rs)

instanceToTypes :: Instance -> Set (TypeInfo lang)
instanceToTypes Generic =
  Set.singleton $ TypeInfo "purescript-prelude" "Data.Generic.Rep" "class Generic" []
instanceToTypes GenericShow =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Show" []
instanceToTypes Json =
  Set.fromList
    [ TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Decode" "class DecodeJson" []
    , TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Encode" "class EncodeJson" []
    ]
instanceToTypes Newtype =
  Set.singleton $ TypeInfo "purescript-newtype" "Data.Newtype" "class Newtype" []
instanceToTypes Functor =
  Set.singleton $ TypeInfo "purescript-functor" "Data.Functor" "class Functor" []
instanceToTypes Eq =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Eq" []
instanceToTypes Eq1 =
  Set.singleton $ TypeInfo "purescript-prelude" "Data.Eq" "class Eq1" []
instanceToTypes Ord =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Ord" []

-- Lenses:
makeLenses ''DataConstructor

makeLenses ''RecordEntry

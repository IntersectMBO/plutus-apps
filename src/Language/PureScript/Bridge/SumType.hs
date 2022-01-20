{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.PureScript.Bridge.SumType where

import Control.Lens hiding (from, to)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Generics.Deriving
import Language.PureScript.Bridge.TypeInfo

-- | Generic representation of your Haskell types.
data SumType (lang :: Language)
  = SumType (TypeInfo lang) [DataConstructor lang] [Instance lang]
  deriving (Show, Eq)

-- | TypeInfo lens for 'SumType'.
sumTypeInfo ::
  Functor f =>
  (TypeInfo lang -> f (TypeInfo lang)) ->
  SumType lang ->
  f (SumType lang)
sumTypeInfo inj (SumType info constrs is) =
  (\ti -> SumType ti constrs is) <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors ::
  Functor f =>
  ([DataConstructor lang] -> f [DataConstructor lang]) ->
  SumType lang ->
  f (SumType lang)
sumTypeConstructors inj (SumType info constrs is) =
  (\cs -> SumType info cs is) <$> inj constrs

-- | Create a representation of your sum (and product) types,
--   for doing type translations and writing it out to your PureScript modules.
mkSumType ::
  forall t.
  (Generic t, Typeable t, GDataConstructor (Rep t)) =>
  SumType 'Haskell
mkSumType =
  SumType
    (mkTypeInfo @t)
    constructors
    (Generic : maybeToList (nootype constructors))
  where
    constructors = gToConstructors (from (undefined :: t))

-- | Purescript typeclass instances that can be generated for your Haskell types.
data Instance (lang :: Language)
  = Generic
  | GenericShow
  | Json
  | Newtype
  | Functor
  | Eq
  | Eq1
  | Ord
  | Enum
  | Bounded
  | Custom (CustomInstance lang)
  deriving (Eq, Show)

type PSInstance = Instance 'PureScript

data InstanceMember (lang :: Language) = InstanceMember
  { _memberName :: Text,
    _memberBindings :: [Text],
    _memberBody :: Text,
    _memberDependencies :: [TypeInfo lang]
  }
  deriving (Eq, Ord, Show)

data InstanceImplementation (lang :: Language)
  = Derive
  | DeriveNewtype
  | Explicit [InstanceMember lang]
  deriving (Eq, Ord, Show)

data CustomInstance (lang :: Language) = CustomInstance
  { _customConstraints :: [TypeInfo lang],
    _customHead :: TypeInfo lang,
    _customImplementation :: InstanceImplementation lang
  }
  deriving (Eq, Ord, Show)

-- | The Purescript typeclass `Newtype` might be derivable if the original
-- Haskell type was a simple type wrapper.
nootype :: [DataConstructor lang] -> Maybe (Instance lang)
nootype [DataConstructor _ (Record _)] = Just Newtype
nootype [DataConstructor _ (Normal [_])] = Just Newtype
nootype _ = Nothing

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

data DataConstructor (lang :: Language) = DataConstructor
  { -- | e.g. `Left`/`Right` for `Either`
    _sigConstructor :: !Text,
    _sigValues :: !(DataConstructorArgs lang)
  }
  deriving (Show, Eq)

data DataConstructorArgs (lang :: Language)
  = Nullary
  | Normal (NonEmpty (TypeInfo lang))
  | Record (NonEmpty (RecordEntry lang))
  deriving (Show, Eq)

instance Semigroup (DataConstructorArgs lang) where
  Nullary <> b = b
  a <> Nullary = a
  Normal as <> Normal bs = Normal $ as <> bs
  Record as <> Record bs = Record $ as <> bs
  Normal as <> Record bs = Normal as <> Normal (_recValue <$> bs)
  Record as <> Normal bs = Normal (_recValue <$> as) <> Normal bs

instance Monoid (DataConstructorArgs lang) where
  mempty = Nullary

data RecordEntry (lang :: Language) = RecordEntry
  { -- | e.g. `runState` for `State`
    _recLabel :: !Text,
    _recValue :: !(TypeInfo lang)
  }
  deriving (Show, Eq)

class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor 'Haskell]

class GDataConstructorArgs f where
  gToDataConstructorArgs :: f a -> DataConstructorArgs 'Haskell

instance (Datatype a, GDataConstructor c) => GDataConstructor (D1 a c) where
  gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
  gToConstructors _ =
    gToConstructors (undefined :: a f) ++ gToConstructors (undefined :: b f)

instance (Constructor a, GDataConstructorArgs b) => GDataConstructor (C1 a b) where
  gToConstructors c@(M1 r) =
    [DataConstructor {_sigConstructor = constructor, _sigValues = values}]
    where
      constructor = T.pack $ conName c
      values = gToDataConstructorArgs r

instance (GDataConstructorArgs a, GDataConstructorArgs b) => GDataConstructorArgs (a :*: b) where
  gToDataConstructorArgs _ =
    gToDataConstructorArgs (undefined :: a f) <> gToDataConstructorArgs (undefined :: b f)

instance GDataConstructorArgs U1 where
  gToDataConstructorArgs _ = mempty

instance (Selector a, Typeable t) => GDataConstructorArgs (S1 a (K1 R t)) where
  gToDataConstructorArgs e = case selName e of
    "" -> Normal [mkTypeInfo @t]
    name -> Record [RecordEntry (T.pack name) (mkTypeInfo @t)]

-- | Get all used types in a sum type.
--
--   This includes all types found at the right hand side of a sum type
--   definition, not the type parameters of the sum type itself
getUsedTypes :: SumType lang -> Set (TypeInfo lang)
getUsedTypes (SumType _ cs is) = foldMap constructorToTypes cs <> foldMap instanceToTypes is

constructorToTypes ::
  DataConstructor lang -> Set (TypeInfo lang)
constructorToTypes (DataConstructor _ Nullary) = Set.empty
constructorToTypes (DataConstructor _ (Normal [ts])) =
  Set.fromList $ flattenTypeInfo ts
constructorToTypes (DataConstructor _ (Record [rs])) =
  Set.fromList . flattenTypeInfo $ _recValue rs
constructorToTypes (DataConstructor _ (Normal ts)) =
  Set.fromList . concatMap flattenTypeInfo $ NE.toList ts
constructorToTypes (DataConstructor _ (Record rs)) =
  Set.fromList . concatMap (flattenTypeInfo . _recValue) $ NE.toList rs

instanceToTypes :: Instance lang -> Set (TypeInfo lang)
instanceToTypes Generic =
  Set.singleton $ TypeInfo "purescript-prelude" "Data.Generic.Rep" "class Generic" []
instanceToTypes GenericShow =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Show" []
instanceToTypes Json =
  Set.fromList
    [ TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Decode" "class DecodeJson" [],
      TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Encode" "class EncodeJson" []
    ]
instanceToTypes Newtype =
  Set.singleton $ TypeInfo "purescript-newtype" "Data.Newtype" "class Newtype" []
instanceToTypes Functor =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Functor" []
instanceToTypes Eq =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Eq" []
instanceToTypes Eq1 =
  Set.singleton $ TypeInfo "purescript-prelude" "Data.Eq" "class Eq1" []
instanceToTypes Ord =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Ord" []
instanceToTypes Enum =
  Set.singleton $ TypeInfo "purescript-enums" "Data.Enum" "class Enum" []
instanceToTypes Bounded =
  Set.singleton $ TypeInfo "purescript-prelude" "Prelude" "class Bounded" []
instanceToTypes (Custom CustomInstance {..}) =
  Set.fromList $
    concatMap flattenTypeInfo $
      _customHead : (_customConstraints <> implementationToTypes _customImplementation)

implementationToTypes :: InstanceImplementation lang -> [TypeInfo lang]
implementationToTypes (Explicit members) = concatMap _memberDependencies members
implementationToTypes _ = []

-- Lenses:
makeLenses ''DataConstructor

makeLenses ''RecordEntry

makeLenses ''CustomInstance

makeLenses ''InstanceImplementation

makeLenses ''InstanceMember

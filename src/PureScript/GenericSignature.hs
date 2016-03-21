{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}


{-# LANGUAGE DeriveGeneric #-}


module PureScript.GenericSignature where




import Generics.Deriving


import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import Data.Typeable

data GenericType = GenericType {
  gTypeName :: Text
, gTypeParameters :: [Text]
} deriving Show

data DataConstructor = DataConstructor {
  sigConstructor :: Text
, sigValues :: Either [GenericType] [RecordEntry]
} deriving Show

data RecordEntry = RecordEntry {
  recLabel :: Text
, recValue :: GenericType
} deriving Show

-- | A GenericSignature is a universal representation of the structure of an arbitrary data structure (that does not contain function arrows).
data GenericSignature = SigSum GenericType [DataConstructor]
                      | SigNumber
                      | SigBoolean
                      | SigInt
                      | SigString
                      | SigChar
                      | SigArray GenericType
                      deriving Show


toSignature :: forall t. (Generic t, Typeable t, GSignature (Rep t)) => Proxy t -> GenericSignature
toSignature p =
  let
    baseSignature = gToSignature (from (undefined :: t))
    rep = typeRep p
    params = map (T.pack . tyConName . typeRepTyCon) (typeRepArgs rep)
  in
    case baseSignature of
      SigSum t cs -> SigSum (t { gTypeParameters = params}) cs
      a -> a

class GSignature f where
  gToSignature :: f a -> GenericSignature

class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor]

class GRecordEntry f where
  gToRecordEntries :: f a -> [RecordEntry]


instance (Datatype a, GDataConstructor c) =>  GSignature (D1 a c) where
  gToSignature t@(M1 c) = SigSum (GenericType (T.pack $ datatypeName t) []) (gToConstructors c)


instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
  gToConstructors (_ :: (a :+: b) f) = gToConstructors (undefined :: a f) ++ gToConstructors (undefined :: b f)

instance (Constructor a, GRecordEntry b) => GDataConstructor (C1 a b) where
  gToConstructors c@(M1 r) = [
      DataConstructor { sigConstructor = T.pack (conName c)
      , sigValues = values
      }
    ]
    where
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
      , recValue = GenericType {
          gTypeName = T.pack (tyConName (typeRepTyCon rep))
        , gTypeParameters = map (T.pack . tyConName . typeRepTyCon) (typeRepArgs rep)
        }
      }
    ]
    where
      rep = typeRep (Proxy :: Proxy t)



data Test = Test deriving Generic

data Test2 = Foo1 | Foo2 deriving Generic

data Test3 a = Bar a | FooBar (Maybe a) a deriving (Generic, Typeable)

data RecordTest = RecordTest {
    field1 :: Int
  , field2 :: String
  }
  | NoRecord deriving (Generic, Show)

data A
data B
data C
data D
data E

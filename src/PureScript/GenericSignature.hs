{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}


{-# LANGUAGE DeriveGeneric #-}


module PureScript.GenericSignature where




import Generics.Deriving


import Data.Text (Text)
import Data.Text as T
import Data.Proxy

-- Intermediate datastructures for constructing PureScript types, heavily inspired (copied) from PureScript generics:

data DataConstructor = DataConstructor {
  sigConstructor :: Text
, sigValues :: [GenericSignature]
} deriving Show

data RecordEntry = RecordEntry {
  recLabel :: Text
, recValue :: GenericSignature
} deriving Show

-- | A GenericSignature is a universal representation of the structure of an arbitrary data structure (that does not contain function arrows).
data GenericSignature = SigProd String [DataConstructor]
                      | SigRecord [RecordEntry]
                      | SigNumber
                      | SigBoolean
                      | SigInt
                      | SigString
                      | SigChar
                      | SigArray GenericSignature
                      deriving Show


class GSignature f where
  toSignature :: f a -> GenericSignature

class GDataConstructor f where
  toConstructors :: f a -> [DataConstructor]

instance Constructor a => GDataConstructor (C1 a b) where
  toConstructors c = [
      DataConstructor { sigConstructor = T.pack (conName c)
      , sigValues =[] -- TODO: Fix me!
      }
    ]

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
  toConstructors (_ :: (a :+: b) f) = toConstructors (undefined :: a f) ++ toConstructors (undefined :: b f)

instance (Datatype a, GDataConstructor c) =>  GSignature (D1 a c) where
  toSignature t@(M1 c) = SigProd (datatypeName t) (toConstructors c)


data Test = Test deriving Generic

data Test2 = Foo1 | Foo2 deriving Generic

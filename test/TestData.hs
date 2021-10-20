{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications #-}


module TestData where

import           Data.Functor.Classes               (Eq1(liftEq))
import           Data.Proxy
import           Data.Text                          (Text)
import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import Language.PureScript.Bridge.CodeGenSwitches (defaultSettings)



-- Check that examples compile:
textBridge :: BridgePart
textBridge = do
   typeName ^== "Text"
   typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
   return psString

stringBridge :: BridgePart
stringBridge = do
   haskType ^== mkTypeInfo @String
   return psString

data Foo = Foo
         | Bar Int
         | FooBar Int Text
         deriving (Eq, Ord, Generic, Typeable, Show)

data Func a = Func Int a
         deriving (Eq, Ord, Functor, Generic, Typeable, Show)

instance Eq1 Func where
  liftEq eq (Func n x) (Func m y) = n == m && x `eq` y

data Test = TestIntInt Int Int
          | TestBool {bool :: Bool}
          | TestVoid
          deriving (Generic, Typeable, Show)

data Bar a b m c = Bar1 (Maybe a) | Bar2 (Either a b) | Bar3 a
                 | Bar4 { myMonadicResult :: m b }
                 deriving (Generic, Typeable, Show)

data SingleRecord a b = SingleRecord {
    _a :: a
  , _b :: b
  , c  :: String
  } deriving(Generic, Eq, Ord, Typeable, Show)

data TwoRecords
  = FirstRecord {
    _fra :: String
  , _frb :: Int
  }
  | SecondRecord {
    _src :: Int
  , _srd :: [Int]
  } deriving(Generic, Typeable, Show)

newtype SomeNewtype = SomeNewtype Int
  deriving (Generic, Typeable, Show)

data SingleValueConstr = SingleValueConstr Int
  deriving (Generic, Typeable, Show)

data SingleProduct = SingleProduct Text Int
  deriving (Generic, Typeable, Show)

a :: HaskellType
a = mkTypeInfo @(Either String Int)

applyBridge :: FullBridge
applyBridge = buildBridge defaultBridge

psA :: PSType
psA = applyBridge a

b :: SumType 'Haskell
b = mkSumType @(Either String Int)

t :: TypeInfo 'PureScript
cs :: [DataConstructor 'PureScript]
psB :: SumType 'PureScript
psB@(SumType t cs _) = bridgeSumType (buildBridge defaultBridge) b

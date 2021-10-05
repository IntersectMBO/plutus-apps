{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import qualified Data.Map                                   as Map
import           Data.Monoid                                ((<>))
import           Data.Proxy
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.CodeGenSwitches
import           Language.PureScript.Bridge.TypeParameters
import           Test.Hspec                                 (Spec, describe,
                                                             hspec, it)
import           Test.Hspec.Expectations.Pretty
import           TestData
import           Text.PrettyPrint.Leijen.Text               (Doc, cat,
                                                             linebreak,
                                                             punctuate, vsep)

main :: IO ()
main = hspec allTests

allTests :: Spec
allTests = do
  describe "buildBridge for purescript 0.11" $ do
    let settings = purs_0_11_settings
    it "tests with Int" $
      let bst = buildBridge defaultBridge (mkTypeInfo (Proxy :: Proxy Int))
          ti =
            TypeInfo
              { _typePackage = ""
              , _typeModule = "Prim"
              , _typeName = "Int"
              , _typeParameters = []
              }
       in bst `shouldBe` ti
    it "tests with custom type Foo" $
      let prox = Proxy :: Proxy Foo
          bst =
            bridgeSumType
              (buildBridge defaultBridge)
              (order prox $ mkSumType prox)
          st =
            SumType
              TypeInfo
                { _typePackage = ""
                , _typeModule = "TestData"
                , _typeName = "Foo"
                , _typeParameters = []
                }
              [ DataConstructor {_sigConstructor = "Foo", _sigValues = Left []}
              , DataConstructor
                  { _sigConstructor = "Bar"
                  , _sigValues =
                      Left
                        [ TypeInfo
                            { _typePackage = ""
                            , _typeModule = "Prim"
                            , _typeName = "Int"
                            , _typeParameters = []
                            }
                        ]
                  }
              , DataConstructor
                  { _sigConstructor = "FooBar"
                  , _sigValues =
                      Left
                        [ TypeInfo
                            { _typePackage = ""
                            , _typeModule = "Prim"
                            , _typeName = "Int"
                            , _typeParameters = []
                            }
                        , TypeInfo
                            { _typePackage = ""
                            , _typeModule = "Prim"
                            , _typeName = "String"
                            , _typeParameters = []
                            }
                        ]
                  }
              ]
              [Eq, Ord, Encode, Decode, Generic]
       in bst `shouldBe` st
    it "tests generation of for custom type Foo" $
      let prox = Proxy :: Proxy Foo
          recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (order prox $ mkSumType prox)
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "data Foo"
              , "  = Foo"
              , "  | Bar Int"
              , "  | FooBar Int String"
              , ""
              , ""
              , "derive instance eqFoo :: Eq Foo"
              , "derive instance ordFoo :: Ord Foo"
              , "derive instance genericFoo :: Generic Foo"
              , "--------------------------------------------------------------------------------"
              , "_Foo :: Prism' Foo Unit"
              , "_Foo = prism' (\\_ -> Foo) f"
              , "  where"
              , "    f Foo = Just unit"
              , "    f _ = Nothing"
              , ""
              , "_Bar :: Prism' Foo Int"
              , "_Bar = prism' Bar f"
              , "  where"
              , "    f (Bar a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_FooBar :: Prism' Foo { a :: Int, b :: String }"
              , "_FooBar = prism' (\\{ a, b } -> FooBar a b) f"
              , "  where"
              , "    f (FooBar a b) = Just $ { a: a, b: b }"
              , "    f _ = Nothing"
              , "--------------------------------------------------------------------------------"
              ]
       in recTypeText `shouldRender` txt
    it "tests the generation of a whole (dummy) module" $
      let advanced =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
          modules = sumTypeToModule advanced Map.empty
          m = head . map (moduleToText settings) . Map.elems $ modules
          txt =
            T.unlines
              [ "-- File auto generated by purescript-bridge! --"
              , "module TestData where"
              , ""
              , "import Data.Either (Either)"
              , "import Data.Eq (class Eq1)"
              , "import Data.Generic (class Generic)"
              , "import Data.Lens (Iso', Lens', Prism', lens, prism')"
              , "import Data.Lens.Iso.Newtype (_Newtype)"
              , "import Data.Lens.Record (prop)"
              , "import Data.Maybe (Maybe, Maybe(..))"
              , "import Data.Newtype (class Newtype)"
              , "import Data.Symbol (SProxy(SProxy))"
              , ""
              , "import Prelude"
              , ""
              , "data Bar a b m c"
              , "  = Bar1 (Maybe a)"
              , "  | Bar2 (Either a b)"
              , "  | Bar3 a"
              , "  | Bar4"
              , "      { myMonadicResult :: m b"
              , "      }"
              , ""
              , ""
              , "derive instance genericBar :: Generic (Bar a b m c)"
              , "--------------------------------------------------------------------------------"
              , "_Bar1 :: forall a b m c. Prism' (Bar a b m c) (Maybe a)"
              , "_Bar1 = prism' Bar1 f"
              , "  where"
              , "    f (Bar1 a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_Bar2 :: forall a b m c. Prism' (Bar a b m c) (Either a b)"
              , "_Bar2 = prism' Bar2 f"
              , "  where"
              , "    f (Bar2 a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_Bar3 :: forall a b m c. Prism' (Bar a b m c) a"
              , "_Bar3 = prism' Bar3 f"
              , "  where"
              , "    f (Bar3 a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_Bar4 :: forall a b m c. Prism' (Bar a b m c) { myMonadicResult :: m b }"
              , "_Bar4 = prism' Bar4 f"
              , "  where"
              , "    f (Bar4 r) = Just r"
              , "    f _ = Nothing"
              , "--------------------------------------------------------------------------------"
              ]
       in m `shouldBe` txt
    it "test generation of constructor optics" $
      let bar =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
          foo =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy Foo))
          barOptics = constructorOptics bar
          fooOptics = constructorOptics foo
          txt =
            T.unlines
              [ "_Bar1 :: forall a b m c. Prism' (Bar a b m c) (Maybe a)"
              , "_Bar1 = prism' Bar1 f"
              , "  where"
              , "    f (Bar1 a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_Bar2 :: forall a b m c. Prism' (Bar a b m c) (Either a b)"
              , "_Bar2 = prism' Bar2 f"
              , "  where"
              , "    f (Bar2 a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_Bar3 :: forall a b m c. Prism' (Bar a b m c) a"
              , "_Bar3 = prism' Bar3 f"
              , "  where"
              , "    f (Bar3 a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_Bar4 :: forall a b m c. Prism' (Bar a b m c) { myMonadicResult :: m b }"
              , "_Bar4 = prism' Bar4 f"
              , "  where"
              , "    f (Bar4 r) = Just r"
              , "    f _ = Nothing"
              , ""
              , "_Foo :: Prism' Foo Unit"
              , "_Foo = prism' (\\_ -> Foo) f"
              , "  where"
              , "    f Foo = Just unit"
              , "    f _ = Nothing"
              , ""
              , "_Bar :: Prism' Foo Int"
              , "_Bar = prism' Bar f"
              , "  where"
              , "    f (Bar a) = Just $ a"
              , "    f _ = Nothing"
              , ""
              , "_FooBar :: Prism' Foo { a :: Int, b :: String }"
              , "_FooBar = prism' (\\{ a, b } -> FooBar a b) f"
              , "  where"
              , "    f (FooBar a b) = Just $ { a: a, b: b }"
              , "    f _ = Nothing"
              ]
       in vsep (punctuate linebreak $ barOptics <> fooOptics) `shouldRender`
          txt
    it "tests generation of record optics" $
      let recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (SingleRecord A B)))
          bar =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
          barOptics = recordOptics bar
          recTypeOptics = recordOptics recType
          txt =
            T.unlines
              [ "a :: forall a b. Lens' (SingleRecord a b) a"
              , "a = _Newtype <<< prop (SProxy :: SProxy \"_a\")"
              , ""
              , "b :: forall a b. Lens' (SingleRecord a b) b"
              , "b = _Newtype <<< prop (SProxy :: SProxy \"_b\")"
              ]
       in cat (punctuate linebreak $ barOptics <> recTypeOptics) `shouldRender`
          txt
    it "tests generation of newtypes for record data type" $
      let recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (SingleRecord A B)))
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "newtype SingleRecord a b"
              , "  = SingleRecord"
              , "      { _a :: a"
              , "      , _b :: b"
              , "      , c :: String"
              , "      }"
              , ""
              , ""
              , "derive instance genericSingleRecord :: Generic (SingleRecord a b)"
              , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
              , "--------------------------------------------------------------------------------"
              , "_SingleRecord :: forall a b. Iso' (SingleRecord a b) { _a :: a"
              , "                                                     , _b :: b"
              , "                                                     , c :: String }"
              , "_SingleRecord = _Newtype"
              , ""
              , "a :: forall a b. Lens' (SingleRecord a b) a"
              , "a = _Newtype <<< prop (SProxy :: SProxy \"_a\")"
              , ""
              , "b :: forall a b. Lens' (SingleRecord a b) b"
              , "b = _Newtype <<< prop (SProxy :: SProxy \"_b\")"
              , "--------------------------------------------------------------------------------"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of newtypes for haskell newtype" $
      let recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SomeNewtype))
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "newtype SomeNewtype"
              , "  = SomeNewtype Int"
              , ""
              , ""
              , "derive instance genericSomeNewtype :: Generic SomeNewtype"
              , "derive instance newtypeSomeNewtype :: Newtype SomeNewtype _"
              , "--------------------------------------------------------------------------------"
              , "_SomeNewtype :: Iso' SomeNewtype Int"
              , "_SomeNewtype = _Newtype"
              , "--------------------------------------------------------------------------------"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of newtypes for haskell data type with one argument" $
      let recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SingleValueConstr))
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "newtype SingleValueConstr"
              , "  = SingleValueConstr Int"
              , ""
              , ""
              , "derive instance genericSingleValueConstr :: Generic SingleValueConstr"
              , "derive instance newtypeSingleValueConstr :: Newtype SingleValueConstr _"
              , "--------------------------------------------------------------------------------"
              , "_SingleValueConstr :: Iso' SingleValueConstr Int"
              , "_SingleValueConstr = _Newtype"
              , "--------------------------------------------------------------------------------"
              ]
       in recTypeText `shouldRender` txt
    it
      "tests generation for haskell data type with one constructor, two arguments" $
      let recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SingleProduct))
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "data SingleProduct"
              , "  = SingleProduct String Int"
              , ""
              , ""
              , "derive instance genericSingleProduct :: Generic SingleProduct"
              , "--------------------------------------------------------------------------------"
              , "_SingleProduct :: Prism' SingleProduct { a :: String, b :: Int }"
              , "_SingleProduct = prism' (\\{ a, b } -> SingleProduct a b) f"
              , "  where"
              , "    f (SingleProduct a b) = Just $ { a: a, b: b }"
              , "--------------------------------------------------------------------------------"
              ]
       in recTypeText `shouldRender` txt
    it
      "tests that sum types with multiple constructors don't generate record optics" $
      let recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy TwoRecords))
          recTypeOptics = recordOptics recType
       in vsep recTypeOptics `shouldRender` "" -- No record optics for multi-constructors
  describe "buildBridge without lens-code-gen for purescript 0.11" $ do
    let settings = getSettings (noLenses <> useGen)
    it "tests generation of for custom type Foo" $
      let proxy = Proxy :: Proxy Foo
          recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (order proxy $ mkSumType proxy)
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "data Foo"
              , "  = Foo"
              , "  | Bar Int"
              , "  | FooBar Int String"
              , ""
              , ""
              , "derive instance eqFoo :: Eq Foo"
              , "derive instance ordFoo :: Ord Foo"
              , "derive instance genericFoo :: Generic Foo"
              ]
       in recTypeText `shouldRender` txt
    it "tests the generation of a whole (dummy) module" $
      let advanced' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
          modules = sumTypeToModule advanced' Map.empty
          m = head . map (moduleToText settings) . Map.elems $ modules
          txt =
            T.unlines
              [ "-- File auto generated by purescript-bridge! --"
              , "module TestData where"
              , ""
              , "import Data.Either (Either)"
              , "import Data.Eq (class Eq1)"
              , "import Data.Generic (class Generic)"
              , "import Data.Maybe (Maybe, Maybe(..))"
              , "import Data.Newtype (class Newtype)"
              , ""
              , "import Prelude"
              , ""
              , "data Bar a b m c"
              , "  = Bar1 (Maybe a)"
              , "  | Bar2 (Either a b)"
              , "  | Bar3 a"
              , "  | Bar4"
              , "      { myMonadicResult :: m b"
              , "      }"
              , ""
              , ""
              , "derive instance genericBar :: Generic (Bar a b m c)"
              ]
       in m `shouldBe` txt
    it "tests generation of newtypes for record data type" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (SingleRecord A B)))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b"
              , "  = SingleRecord"
              , "      { _a :: a"
              , "      , _b :: b"
              , "      , c :: String"
              , "      }"
              , ""
              , ""
              , "derive instance genericSingleRecord :: Generic (SingleRecord a b)"
              , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of newtypes for haskell newtype" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SomeNewtype))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SomeNewtype"
              , "  = SomeNewtype Int"
              , ""
              , ""
              , "derive instance genericSomeNewtype :: Generic SomeNewtype"
              , "derive instance newtypeSomeNewtype :: Newtype SomeNewtype _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of newtypes for haskell data type with one argument" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SingleValueConstr))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SingleValueConstr"
              , "  = SingleValueConstr Int"
              , ""
              , ""
              , "derive instance genericSingleValueConstr :: Generic SingleValueConstr"
              , "derive instance newtypeSingleValueConstr :: Newtype SingleValueConstr _"
              ]
       in recTypeText `shouldRender` txt
    it
      "tests generation for haskell data type with one constructor, two arguments" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SingleProduct))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "data SingleProduct"
              , "  = SingleProduct String Int"
              , ""
              , ""
              , "derive instance genericSingleProduct :: Generic SingleProduct"
              ]
       in recTypeText `shouldRender` txt
  describe "buildBridge without lens-code-gen and generics-rep" $ do
    let settings = getSettings (noLenses <> useGenRep)
    it "tests generation of typeclasses for custom type Foo" $
      let proxy = Proxy :: Proxy Foo
          recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (genericShow proxy $ order proxy $ mkSumType proxy)
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "data Foo"
              , "  = Foo"
              , "  | Bar Int"
              , "  | FooBar Int String"
              , ""
              , ""
              , "instance showFoo :: Show Foo where"
              , "  show x = genericShow x"
              , "derive instance eqFoo :: Eq Foo"
              , "derive instance ordFoo :: Ord Foo"
              , "derive instance genericFoo :: Generic Foo _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of typeclasses for custom type Func" $
      let proxy = Proxy :: Proxy (Func A)
          recType =
            bridgeSumType
              (buildBridge defaultBridge)
              (equal1 proxy $ functor proxy $ genericShow proxy $ mkSumType proxy)
          recTypeText = sumTypeToDoc settings recType
          txt =
            T.unlines
              [ "data Func a"
              , "  = Func Int a"
              , ""
              , ""
              , "derive instance eq1Func :: Eq1 Func"
              , "derive instance functorFunc :: Functor Func"
              , "instance showFunc :: (Show a) => Show (Func a) where"
              , "  show x = genericShow x"
              , "derive instance genericFunc :: Generic (Func a) _"
              ]
       in recTypeText `shouldRender` txt
    it "tests the generation of a whole (dummy) module" $
      let advanced' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
          modules = sumTypeToModule advanced' Map.empty
          m = head . map (moduleToText settings) . Map.elems $ modules
          txt =
            T.unlines
              [ "-- File auto generated by purescript-bridge! --"
              , "module TestData where"
              , ""
              , "import Data.Either (Either)"
              , "import Data.Eq (class Eq1)"
              , "import Data.Generic.Rep (class Generic)"
              , "import Data.Maybe (Maybe, Maybe(..))"
              , "import Data.Newtype (class Newtype)"
              , "import Data.Show.Generic (genericShow)"
              , ""
              , "import Prelude"
              , ""
              , "data Bar a b m c"
              , "  = Bar1 (Maybe a)"
              , "  | Bar2 (Either a b)"
              , "  | Bar3 a"
              , "  | Bar4"
              , "      { myMonadicResult :: m b"
              , "      }"
              , ""
              , ""
              , "derive instance genericBar :: Generic (Bar a b m c) _"
              ]
       in m `shouldBe` txt
    it "tests generation of newtypes for record data type" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy (SingleRecord A B)))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b"
              , "  = SingleRecord"
              , "      { _a :: a"
              , "      , _b :: b"
              , "      , c :: String"
              , "      }"
              , ""
              , ""
              , "derive instance genericSingleRecord :: Generic (SingleRecord a b) _"
              , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of newtypes for haskell newtype" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SomeNewtype))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SomeNewtype"
              , "  = SomeNewtype Int"
              , ""
              , ""
              , "derive instance genericSomeNewtype :: Generic SomeNewtype _"
              , "derive instance newtypeSomeNewtype :: Newtype SomeNewtype _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation of newtypes for haskell data type with one argument" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SingleValueConstr))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SingleValueConstr"
              , "  = SingleValueConstr Int"
              , ""
              , ""
              , "derive instance genericSingleValueConstr :: Generic SingleValueConstr _"
              , "derive instance newtypeSingleValueConstr :: Newtype SingleValueConstr _"
              ]
       in recTypeText `shouldRender` txt
    it
      "tests generation for haskell data type with one constructor, two arguments" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType (Proxy :: Proxy SingleProduct))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "data SingleProduct"
              , "  = SingleProduct String Int"
              , ""
              , ""
              , "derive instance genericSingleProduct :: Generic SingleProduct _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation Eq instances for polymorphic types" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              ((equal <*> mkSumType) (Proxy :: Proxy (SingleRecord A B)))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b"
              , "  = SingleRecord"
              , "      { _a :: a"
              , "      , _b :: b"
              , "      , c :: String"
              , "      }"
              , ""
              , ""
              , "derive instance eqSingleRecord :: (Eq a, Eq b) => Eq (SingleRecord a b)"
              , "derive instance genericSingleRecord :: Generic (SingleRecord a b) _"
              , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
              ]
       in recTypeText `shouldRender` txt
    it "tests generation Ord instances for polymorphic types" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              ((order <*> mkSumType) (Proxy :: Proxy (SingleRecord A B)))
          recTypeText = sumTypeToDoc settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b"
              , "  = SingleRecord"
              , "      { _a :: a"
              , "      , _b :: b"
              , "      , c :: String"
              , "      }"
              , ""
              , ""
              , "derive instance eqSingleRecord :: (Eq a, Eq b) => Eq (SingleRecord a b)"
              , "derive instance ordSingleRecord :: (Ord a, Ord b) => Ord (SingleRecord a b)"
              , "derive instance genericSingleRecord :: Generic (SingleRecord a b) _"
              , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
              ]
       in recTypeText `shouldRender` txt

shouldRender :: Doc -> Text -> Expectation
shouldRender actual expected = renderText actual `shouldBe` T.stripEnd expected

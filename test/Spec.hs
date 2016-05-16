{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Main where

import qualified Data.Map                                  as Map
import           Data.Proxy
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Typeable
import           GHC.Generics                              (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters



-- Check that examples compile:
textBridge :: BridgePart
textBridge = do
   typeName ^== "Text"
   typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
   return psString

stringBridge :: BridgePart
stringBridge = do
   haskType ^== mkTypeInfo (Proxy :: Proxy String)
   return psString

data Foo = Foo | Bar Int | FooBar Int Text deriving (Generic, Typeable, Show)

data Bar a b m = Bar1 (Maybe a) | Bar2 (Either a b) | Bar3 a
  | Bar4 {
      myMonadicResult :: m b
  } deriving (Generic, Typeable, Show)

main :: IO ()
main = do
  let r = buildBridge defaultBridge (mkTypeInfo (Proxy :: Proxy Int))
  let r2 = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy Foo))
  putStrLn "Bridge a single int: "
  print r
  putStrLn "\nBridge Foo:"
  print r2
  let advanced = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (Bar A B M1)))
  let modules = sumTypeToModule advanced Map.empty
  let m = head . map moduleToText . Map.elems $ modules
  putStrLn "\n Complete module for Bar type:\n"
  putStrLn $ T.unpack m

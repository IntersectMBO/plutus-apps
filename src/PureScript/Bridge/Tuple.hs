{-# LANGUAGE OverloadedStrings #-}
module PureScript.Bridge.Tuple where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T


import PureScript.Bridge.TypeInfo

data TupleParserState =
  Start | OpenFound | ColonFound | Tuple | NoTuple deriving (Eq, Show)


tupleBridge :: TypeBridge
tupleBridge t
  | isTuple (typeName t) = Just $ t {
      typePackage = "purescript-tuples"
    , typeModule = if size == 2 then "Data.Tuple" else "Data.Tuple.Nested"
    , typeName = "Tuple" <> if size == 2 then "" else T.pack (show size)
    }
  | otherwise = Nothing
  where
    size = length $ typeParameters t


step :: TupleParserState -> Char -> TupleParserState
step Start '(' = OpenFound
step Start _ = NoTuple
step OpenFound ',' = ColonFound
step OpenFound _ = NoTuple
step ColonFound ',' = ColonFound
step ColonFound ')' = Tuple
step ColonFound _ = NoTuple
step Tuple _ = NoTuple
step NoTuple _ = NoTuple

isTuple :: Text -> Bool
isTuple = (== Tuple) . T.foldl' step Start

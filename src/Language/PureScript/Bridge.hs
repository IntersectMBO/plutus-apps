{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Bridge (
    bridgeSumType
  , defaultBridge
 ) where


import qualified Data.Text as T

import Language.PureScript.Bridge.SumType
import Language.PureScript.Bridge.TypeInfo
import Language.PureScript.Bridge.Tuple
import Language.PureScript.Bridge.Primitives


import Control.Applicative
import Data.Maybe

bridgeSumType :: TypeBridge -> SumType -> SumType
bridgeSumType br (SumType t cs) = SumType t . map (bridgeConstructor br) $ cs

{--
 -- Optimistically and recursively translate types: If the passed TypeBridge returns Nothing,
 -- then the original TypeInfo is returned with the typePackage field cleared.
 -- You don't need to call this function directly, just use bridgeSumType with your TypeBridge
--}
doBridge :: TypeBridge -> TypeInfo -> TypeInfo
doBridge br info = let
    translated = info { typePackage = "" }
    res = fixTypeParameters $ fromMaybe translated (br info)
  in
    res {
      typeParameters = map (doBridge br) . typeParameters $ res
    }

-- | Default bridge for mapping primitive/common types:
-- | You can append your own bridges like this:
-- | defaultBridge <|> myBridge1 <|> myBridge2
defaultBridge :: TypeBridge
defaultBridge t = stringBridge t
  <|> listBridge t
  <|> maybeBridge t
  <|> eitherBridge t
  <|> intBridge t
  <|> tupleBridge t

bridgeConstructor :: TypeBridge -> DataConstructor -> DataConstructor
bridgeConstructor br (DataConstructor name (Left infos)) =
    DataConstructor name . Left $ map (doBridge br) infos
bridgeConstructor br (DataConstructor name (Right record)) =
    DataConstructor name . Right $ map (bridgeRecordEntry br) record

bridgeRecordEntry :: TypeBridge -> RecordEntry -> RecordEntry
bridgeRecordEntry br (RecordEntry label value) = RecordEntry label $ doBridge br value

-- | Translate types that come from any module named "Something.TypeParameters" to lower case:
fixTypeParameters :: TypeInfo -> TypeInfo
fixTypeParameters t
  | Just _ <- T.stripSuffix "TypeParameters" (typeModule t) = t {
      typePackage = "" -- Don't suggest any packages
    , typeModule = "" -- Don't import any modules
    , typeName = T.toLower (typeName t)
    }
  | otherwise = t

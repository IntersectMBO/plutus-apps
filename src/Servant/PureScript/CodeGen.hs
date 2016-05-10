{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PureScript.CodeGen where

import           Servant.PureScript.Internal
import           Text.PrettyPrint.Mainland

import           Control.Lens                        hiding (List)
import           Data.Aeson
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (mapMaybe, maybeToList)
import           Data.Monoid
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Data.Typeable
import           GHC.Generics                        hiding (to)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.Printer
import           Language.PureScript.Bridge.TypeInfo
import           Network.HTTP.Types.URI              (urlEncode)
import           Servant.API
import           Servant.Foreign


type FunctionName = Text

genFunction :: Settings -> Req TypeInfo -> Doc
genFunction opts req = let
    fnName = req ^. reqFuncName ^. camelCaseL
    params = filterParamsInReader opts . reqToParams $ req
    pTypes = map _pType params
    pNames = map _pName params
    signature = genSignature fnName pTypes (req ^. reqReturnType)
    body = genFnHead fnName pNames <+> genFnBody opts req
  in signature </> body


genGetReaderParams :: Settings -> Doc
genGetReaderParams = stack . map (genGetReaderParam . strictText) . Set.toList . putInReader
  where
    genGetReaderParam pName = "let" <+> pName <+> "= opts.params." <> pName


genSignature :: Text -> [TypeInfo] -> Maybe TypeInfo -> Doc
genSignature fnName params mRet = fName <+> align (constraint <+/> parameterString <+/> retString)
  where
    fName = strictText fnName
    constraint = ":: forall m. (MonadReader m, MonadAff m) =>"
    typeNames = map (strictText . typeInfoToText True) params
    parameterString = docIntercalate (softline <> "-> ") typeNames
    retName = maybe "Unit" (strictText . typeInfoToText True) mRet
    retString = "-> " <> retName

genFnHead :: Text -> [Text] -> Doc
genFnHead fnName params = fName <+> align (docIntercalate softline docParams <+> "=")
  where
    docParams = map strictText params
    fName = strictText fnName

genFnBody :: Settings -> Req TypeInfo -> Doc
genFnBody opts req = hang 2 $ "do"
    </> "opts <- ask"
    </> genGetReaderParams opts
    </> hang 2 ("let reqUrl =" <+> genBuildURL opts (req ^. reqUrl))

genBuildURL :: Settings -> Url TypeInfo -> Doc
genBuildURL opts url = "opts." <> strictText baseURLId <+> "<>"
    <+> genBuildPath (url ^. path ) <+> genBuildQuery (url ^. queryStr)

----------
genBuildPath :: Path TypeInfo -> Doc
genBuildPath = docIntercalate (softline <> "<> \"/\" <> ") . map (genBuildSegment . unSegment)

genBuildSegment :: SegmentType TypeInfo -> Doc
genBuildSegment (Static (PathSegment seg)) = dquotes $ strictText (textURLEncode False seg)
genBuildSegment (Cap arg) = parens $ "encodeURIComponent" <+> strictText (arg ^. argName ^. to unPathSegment)

----------
genBuildQuery :: [QueryArg TypeInfo] -> Doc
genBuildQuery [] = ""
genBuildQuery args = softline <> "<> \"?\" <> " <> (docIntercalate (softline <> "<> \"&\" <> ") . map genBuildQueryArg $ args)

genBuildQueryArg :: QueryArg TypeInfo -> Doc
genBuildQueryArg arg = case arg ^. queryArgType of
    Normal -> genQueryEncoding "encodeQuery"
    Flag   -> genQueryEncoding "encodeQuery"
    List   -> genQueryEncoding "encodeListQuery"
  where
    argText = arg ^. queryArgName ^. argName ^. to unPathSegment
    encodedArgName = strictText . textURLEncode True $ argText
    genQueryEncoding fn = fn <+> dquotes encodedArgName <+> strictText argText

filterParamsInReader :: Settings -> [Param TypeInfo] -> [Param TypeInfo]
filterParamsInReader opts = filter (not . flip Set.member (putInReader opts) . _pName)

reqsToImportLines :: [Req TypeInfo] -> ImportLines
reqsToImportLines = typesToImportLines Map.empty . concatMap reqToTypeInfos

reqToTypeInfos :: Req TypeInfo -> [TypeInfo]
reqToTypeInfos req = map _pType (reqToParams req) ++ maybeToList (req ^. reqReturnType)

-- | Extract all function parameters from a given Req.
reqToParams :: Req f -> [Param f]
reqToParams req = fmap headerArgToParam (req ^. reqHeaders)
               ++ maybeToList (reqBodyToParam (req ^. reqBody))
               ++ urlToParams (req ^. reqUrl)

urlToParams :: Url f -> [Param f]
urlToParams url = mapMaybe (segmentToParam . unSegment) (url ^. path) ++ map queryArgToParam (url ^. queryStr)

segmentToParam :: SegmentType f -> Maybe (Param f)
segmentToParam (Static _) = Nothing
segmentToParam (Cap arg) = Just Param {
    _pType = arg ^. argType
  , _pName = arg ^. argName ^. to unPathSegment
  }

queryArgToParam :: QueryArg f -> Param f
queryArgToParam arg = Param {
    _pType = arg ^. queryArgName ^. argType
  , _pName = arg ^. queryArgName ^. argName ^. to unPathSegment
  }

headerArgToParam :: HeaderArg f -> Param f
headerArgToParam (HeaderArg arg) = Param {
    _pName = arg ^. argName ^. to unPathSegment
  , _pType = arg ^. argType
  }
headerArgToParam _ = error "We do not support ReplaceHeaderArg - as I have no idea what this is all about."

reqBodyToParam :: Maybe f -> Maybe (Param f)
reqBodyToParam = fmap (Param "reqBody")

docIntercalate :: Doc -> [Doc] -> Doc
docIntercalate i = mconcat . punctuate i


textURLEncode :: Bool -> Text -> Text
textURLEncode spaceIsPlus = T.decodeUtf8 . urlEncode spaceIsPlus . T.encodeUtf8

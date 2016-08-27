{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- TODO: This module duplicates quite a lot of code from CodeGen.hs.
module Servant.Purescript.Subscriber where

import           Control.Lens                       hiding (List)
import qualified Data.Map                           as Map
import           Data.Maybe                         (mapMaybe, maybeToList)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes (psString)
import           Network.HTTP.Types.URI             (urlEncode)
import           Servant.Foreign
import           Servant.PureScript.Internal
import           Text.PrettyPrint.Mainland
import Servant.Purescript.CodeGen

genModule :: Settings -> [Req PSType] -> Maybe Doc
genModule opts allReqs = let
    isSubscribable :: Req PSType -> Bool
    isSubscribable req = T.empty `elem`  req ^.reqFuncName . to unFunctionName
    reqs       = filter isSubscribable allReqs
    allParams  = concatMap reqToParams allReqs
    rParams    = getReaderParams opts allParams
    apiImports = reqsToImportLines reqs
    imports    = mergeImportLines (_standardImports opts) apiImports
    moduleName = _apiModuleName opts <> ".Subscriber"
  in
    genModuleHeader moduleName imports rParams
    </> (docIntercalate line . map (genFunction rParams)) reqs

genFunction :: PSType -> [PSParam] -> Req PSType -> Doc
genFunction responseType allRParams req = let
    rParamsSet = Set.fromList allRParams
    fnName = req ^. reqFuncName ^. camelCaseL
    allParamsList = subscriberToUserTypeParam responseType : baseURLParam : reqToParams req
    allParams = Set.fromList allParamsList
    fnParams = filter (not . flip Set.member rParamsSet) allParamsList -- Use list not set, as we don't want to change order of parameters
    rParams = Set.toList $ rParamsSet `Set.intersection` allParams

    pTypes = map _pType fnParams
    pNames = map _pName fnParams
    signature = genSignature fnName pTypes (req ^. reqReturnType)
    body = genFnHead fnName pNames <+> genFnBody rParams req
  in signature </> body


genGetReaderParams :: [PSParam] -> Doc
genGetReaderParams = stack . map (genGetReaderParam . psVar . _pName)
  where
    genGetReaderParam pName' = "let" <+> pName' <+> "= spParams_." <> pName'


genSignature :: Text -> [PSType] -> Maybe PSType -> Doc
genSignature = genSignatureBuilder "forall m." <+/> "MonadReader (SPSettings_ SPParams_) m" <+/> "=>"

genFnHead :: Text -> [Text] -> Doc
genFnHead fnName params = fName <+> align (docIntercalate softline docParams <+> "=")
  where
    docParams = map psVar params
    fName = strictText fnName

genFnBody :: [PSParam] -> Req PSType -> Doc
genFnBody rParams req = "do"
    </> indent 2 (
          "spOpts_' <- ask"
      </> "let spOpts_ = case spOpts_' of SPSettings_ o -> o"
      </> "let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_"
      </> genGetReaderParams rParams
      </> hang 6 ("let httpMethod =" <+> dquotes (req ^. reqMethod ^. to T.decodeUtf8 ^. to strictText))
      </> hang 6 ("let reqPath ="     <+> genBuildPath (req ^. reqUrl . path))
      </> "let reqHeaders =" </> indent 6 (req ^. reqHeaders ^. to genBuildHeaders)
      </> "let reqQuery =" </> indent 6 (req ^. reqURL ^. queryStr . to genBuildQuery)
      </> "let spReq = " <> hang 2 ("HttpRequest" </>
                                   "{ httpMethod:" <+> "httpMethod"
                               </> ", httpPath:" <+> "reqPath"
                               </> ", httpHeaders:" <+> "reqHeaders"
                               </> ", httpQuery:" <+> "reqQuery"
                               </> ", httpBody:" <+> "printJson <<< encodeJson $ reqBody"
                               </> "}")
      </> "pure $ makeSubscriptions spReq " <> subscriberToUserId
    )

----------
genBuildPath :: Path PSType -> Doc
genBuildPath = "Path ["
  <> docIntercalate (softline <> ", ") . map (genBuildSegment . unSegment)
  <> "]"

genBuildSegment :: SegmentType PSType -> Doc
genBuildSegment (Static (PathSegment seg)) = dquotes $ strictText seg
genBuildSegment (Cap arg) = "gDefaultToURLPiece" <+> arg ^. argName ^. to unPathSegment ^. to psVar

----------
genBuildQuery :: [QueryArg PSType] -> Doc
genBuildQuery [] = ""
genBuildQuery args = softline <> "<> \"?\" <> " <> (docIntercalate (softline <> "<> \"&\" <> ") . map genBuildQueryArg $ args)

genBuildQueryArg :: QueryArg PSType -> Doc
genBuildQueryArg arg = case arg ^. queryArgType of
    Normal -> genQueryEncoding "encodeQuery"
    Flag   -> genQueryEncoding "encodeQuery"
    List   -> genQueryEncoding "encodeListQuery"
  where
    argText = arg ^. queryArgName ^. argName ^. to unPathSegment
    argName = strictText  argText
    genQueryEncoding fn = fn <+> dquotes encodedArgName <+> psVar argText

-----------

genBuildHeaders :: [HeaderArg PSType] -> Doc
genBuildHeaders = list . map genBuildHeader

genBuildHeader :: HeaderArg PSType -> Doc
genBuildHeader (HeaderArg arg) = let
    argText = arg ^. argName ^. to unPathSegment
    argName = strictText argText
  in
    align $ "Tuple" <+> argName <+> "(gDefaultToUrlPiece" <+> psVar argText <> ")"
genBuildHeader (ReplaceHeaderArg _ _) = error "ReplaceHeaderArg - not yet implemented!"

reqsToImportLines :: [Req PSType] -> ImportLines
reqsToImportLines = typesToImportLines Map.empty . concatMap reqToPSTypes

reqToPSTypes :: Req PSType -> [PSType]
reqToPSTypes req = map _pType (reqToParams req) ++ maybeToList (req ^. reqReturnType)

-- | Extract all function parameters from a given Req.
reqToParams :: Req PSType -> [Param PSType]
reqToParams req = Param baseURLId psString
               : fmap headerArgToParam (req ^. reqHeaders)
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

-- | Little helper for generating valid variable names
psVar :: Text -> Doc
psVar = strictText . toPSVarName


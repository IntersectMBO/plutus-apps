{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- TODO: This module duplicates quite a lot of code from CodeGen.hs.
module Servant.PureScript.MakeRequests where

import           Control.Lens                       hiding (List)
import           Data.Map                           (Map)
import           Data.Maybe                         (mapMaybe, maybeToList)
import           Data.Proxy                         (Proxy (Proxy))
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge         (buildBridge, defaultBridge)
import           Language.PureScript.Bridge.PSTypes (psString, psUnit)
import           Network.HTTP.Types.URI             (urlEncode)
import           Servant.Foreign
import           Servant.PureScript.CodeGen         hiding (genBuildHeader,
                                                     genBuildHeaders,
                                                     genBuildPath,
                                                     genBuildQuery,
                                                     genBuildQueryArg,
                                                     genBuildSegment, genFnBody,
                                                     genFunction, genModule,
                                                     genSignature)
import           Servant.PureScript.Internal
import           Servant.Subscriber.Request         (HttpRequest)
import           Text.PrettyPrint.Mainland

subscriberImportLines :: Map Text ImportLine
subscriberImportLines = importsFromList
  [
    ImportLine "Servant.Subscriber.Subscriptions" (Set.fromList [ "Subscriptions"
                                                                , "makeSubscriptions"
                                                                ])
  , ImportLine "Servant.Subscriber.Util" (Set.fromList [ "toUserType"
                                                       , "subGenNormalQuery"
                                                       , "subGenListQuery"
                                                       , "subGenFlagQuery"
                                                       , "TypedToUser"
                                                       ])
  , ImportLine "Servant.Subscriber" (Set.fromList ["ToUserType"])
  , ImportLine "Servant.Subscriber.Request" (Set.fromList ["HttpRequest(..)"])
  , ImportLine "Servant.Subscriber.Types" (Set.fromList ["Path(..)"])
  , ImportLine "Data.Tuple" (Set.fromList ["Tuple(..)"])
  ]

genModule :: Settings -> [Req PSType] -> Doc
genModule opts reqs = let
    allParams  = concatMap reqToParams reqs
    rParams    = getReaderParams opts allParams
    apiImports = reqsToImportLines reqs
    webAPIImports = importsFromList [
        ImportLine (opts ^. apiModuleName) (Set.fromList ["SPParams_(..)"])
      ]
    imports    = _standardImports opts
                  `mergeImportLines` apiImports
                  `mergeImportLines` subscriberImportLines
                  `mergeImportLines` webAPIImports
    moduleName = _apiModuleName opts <> ".MakeRequests"
  in
    genModuleHeader moduleName imports
    </> (docIntercalate line . map (genFunction rParams)) reqs

genFunction :: [PSParam] -> Req PSType -> Doc
genFunction allRParams req = let
    rParamsSet = Set.fromList allRParams
    fnName = req ^. reqFuncName ^. camelCaseL
    allParamsList = baseURLParam : reqToParams req
    allParams = Set.fromList allParamsList
    fnParams = filter (not . flip Set.member rParamsSet) allParamsList -- Use list not set, as we don't want to change order of parameters
    rParams = Set.toList $ rParamsSet `Set.intersection` allParams

    pTypes = map _pType fnParams
    pNames = map _pName fnParams
    signature = genSignature fnName pTypes (Just psHttpRequest)
    body = genFnHead fnName pNames <+> genFnBody rParams req
  in signature </> body


genSignature :: Text -> [PSType] -> Maybe PSType -> Doc
genSignature = genSignatureBuilder $ "forall m." <+/> "MonadReader (SPSettings_ SPParams_) m" <+/> "=>"

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
      </> "let reqQuery =" </> indent 6 (req ^. reqUrl ^. queryStr . to genBuildQuery)
      </> "let spReq = " <> hang 2 ("HttpRequest" </>
                                   "{ httpMethod:" <+> "httpMethod"
                               </> ", httpPath:" <+> "reqPath"
                               </> ", httpHeaders:" <+> "reqHeaders"
                               </> ", httpQuery:" <+> "reqQuery"
                               </> ", httpBody:" <+> case req ^. reqBody of
                                       Nothing -> "\"\""
                                       Just _ -> "printJson <<< encodeJson $ reqBody"
                               </> "}")
      </> "pure spReq"
    ) <> "\n"

----------
genBuildPath :: Path PSType -> Doc
genBuildPath p = "Path ["
  <> (docIntercalate (softline <> ", ") . map (genBuildSegment . unSegment)) p
  <> "]"

genBuildSegment :: SegmentType PSType -> Doc
genBuildSegment (Static (PathSegment seg)) = dquotes $ strictText seg
genBuildSegment (Cap arg) = "gDefaultToURLPiece" <+> arg ^. argName ^. to unPathSegment ^. to psVar

----------
genBuildQuery :: [QueryArg PSType] -> Doc
genBuildQuery []   = "[]"
genBuildQuery args = docIntercalate (softline <> "<> ") . map genBuildQueryArg $ args

genBuildQueryArg :: QueryArg PSType -> Doc
genBuildQueryArg arg = case arg ^. queryArgType of
    Normal -> genQueryEncoding "subGenNormalQuery"
    Flag   -> genQueryEncoding "subGenFlagQuery"
    List   -> genQueryEncoding "subGenListQuery"
  where
    argText = arg ^. queryArgName ^. argName ^. to unPathSegment
    argDoc = strictText argText
    genQueryEncoding fn = fn <+> dquotes argDoc <+> psVar argText

-----------

genBuildHeaders :: [HeaderArg PSType] -> Doc
genBuildHeaders = list . map genBuildHeader

genBuildHeader :: HeaderArg PSType -> Doc
genBuildHeader (HeaderArg arg) = let
    argText = arg ^. argName ^. to unPathSegment
    argDoc = strictText argText
  in
    align $ "Tuple" <+> dquotes argDoc <+> "(gDefaultToURLPiece" <+> psVar argText <> ")"
genBuildHeader (ReplaceHeaderArg _ _) = error "ReplaceHeaderArg - not yet implemented!"



psHttpRequest :: PSType
psHttpRequest = let
    haskType' = mkTypeInfo (Proxy :: Proxy HttpRequest)
    bridge = buildBridge defaultBridge
 in
    bridge haskType'

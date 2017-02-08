{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- TODO: This module duplicates quite a lot of code from MakeRequests.hs.
module Servant.PureScript.Subscriber where

import           Control.Lens                       hiding (List)
import           Data.Map                           (Map)
import           Data.Maybe                         (mapMaybe, maybeToList)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes (psString, psUnit)
import           Network.HTTP.Types.URI             (urlEncode)
import           Servant.Foreign
import           Servant.PureScript.CodeGen         (docIntercalate, genFnHead,
                                                     genModuleHeader,
                                                     genSignatureBuilder,
                                                     getReaderParams, psVar,
                                                     reqToParams,
                                                     reqsToImportLines)
import           Servant.PureScript.Internal
import           Servant.PureScript.MakeRequests    hiding (genFnBody,
                                                     genFunction, genModule,
                                                     genSignature)
import           Text.PrettyPrint.Mainland

genModule :: Settings -> [Req PSType] -> Doc
genModule opts allReqs = let
    isSubscribable :: Req PSType -> Bool
    isSubscribable req = T.empty `elem`  req ^.reqFuncName . to unFunctionName
    reqs       = filter isSubscribable allReqs
    allParams  = concatMap reqToParams allReqs
    rParams    = getReaderParams opts allParams
    apiImports = reqsToImportLines reqs
    webAPIImports = importsFromList [
        ImportLine (opts ^. apiModuleName) (Set.fromList ["SPParams_(..)"])
      ]
    imports    = _standardImports opts
                  `mergeImportLines` apiImports
                  `mergeImportLines` subscriberImportLines
                  `mergeImportLines` webAPIImports
    moduleName = _apiModuleName opts <> ".Subscriber"
  in
    genModuleHeader moduleName imports
    </> "import" <+> opts ^. apiModuleName . to strictText <> ".MakeRequests as MakeRequests"
    </> ""
    </> (docIntercalate line . map (genFunction rParams)) reqs

genFunction :: [PSParam] -> Req PSType -> Doc
genFunction allRParams req = let
    rParamsSet = Set.fromList allRParams
    fnName = req ^. reqFuncName ^. camelCaseL
    responseType = case req ^. reqReturnType of
                     Nothing -> psUnit
                     Just t  -> t
    allParamsList = makeTypedToUserParam responseType : baseURLParam : reqToParams req
    fnParams = filter (not . flip Set.member rParamsSet) allParamsList -- Use list not set, as we don't want to change order of parameters

    pTypes = map _pType fnParams
    pNames = map _pName fnParams
    signature = genSignature fnName pTypes (Just psSubscriptions)
    -- | Well - if you really want to put the ToUserType parameter into the Reader monad - this will crash:
    body = genFnHead fnName pNames <+> genFnBody fnName (tail pNames)
  in signature </> body


genSignature :: Text -> [PSType] -> Maybe PSType -> Doc
genSignature = genSignatureBuilder $ "forall m a." <+/> "MonadAsk (SPSettings_ SPParams_) m" <+/> "=>"

genFnBody :: Text -> [Text] -> Doc
genFnBody fName params = "do"
  </> indent 2 (
        "spReq <- MakeRequests." <> genFnCall fName params
    </> "pure $ makeSubscriptions spReq (toUserType " <> strictText subscriberToUserId <> ")"
    ) <> "\n"

genFnCall :: Text -> [Text] -> Doc
genFnCall fnName params = fName <+> align (docIntercalate softline docParams)
  where
    docParams = map psVar params
    fName = strictText fnName

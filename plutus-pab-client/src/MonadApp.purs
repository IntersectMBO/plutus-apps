module MonadApp where

import Prologue
import Affjax (defaultRequest, request)
import Affjax.RequestBody (string)
import Affjax.ResponseFormat as Response
import Animation (class MonadAnimate, animate)
import Clipboard (class MonadClipboard, copy)
import ContractExample (ExampleContracts)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Argonaut.Decode (decodeJson)
import Data.HTTP.Method (Method(..))
import Data.Lens (view)
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen (HalogenM, liftEffect, raise)
import Network.RemoteData as RemoteData
import Playground.Lenses (_getEndpointDescription)
import Plutus.PAB.Webserver (class HasSPSettings, SPSettings_, getApiFullreport, getApiContractInstances, getApiContractDefinitions, postApiContractActivate, getApiContractInstanceByContractinstanceidStatus)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState, ContractSignatureResponse, FullReport, CombinedWSStreamToServer, ContractActivationArgs)
import Servant.PureScript (AjaxError, ErrorDescription(..))
import Types (HAction, Output(..), State, WebData, _contractInstanceIdString)
import Wallet.Types (EndpointDescription, ContractInstanceId)

class
  Monad m <= MonadApp m where
  getFullReport :: m (WebData (FullReport ExampleContracts))
  getContractInstanceStatus :: ContractInstanceId -> m (WebData (ContractInstanceClientState ExampleContracts))
  getContractInstances :: Maybe String -> m (WebData (Array (ContractInstanceClientState ExampleContracts)))
  getContractDefinitions :: m (WebData (Array (ContractSignatureResponse ExampleContracts)))
  invokeEndpoint :: RawJson -> ContractInstanceId -> EndpointDescription -> m (WebData Unit)
  activateContract :: ContractActivationArgs ExampleContracts -> m (WebData ContractInstanceId)
  sendWebSocketMessage :: CombinedWSStreamToServer -> m Unit
  log :: String -> m Unit

newtype HalogenApp m a
  = HalogenApp (HalogenM (State ExampleContracts) (HAction ExampleContracts) () Output m a)

derive instance newtypeHalogenApp :: Newtype (HalogenApp m a) _

derive newtype instance functorHalogenApp :: Functor (HalogenApp m)

derive newtype instance applicativeHalogenApp :: Applicative (HalogenApp m)

derive newtype instance applyHalogenApp :: Apply (HalogenApp m)

derive newtype instance bindHalogenApp :: Bind (HalogenApp m)

derive newtype instance monadHalogenApp :: Monad (HalogenApp m)

derive newtype instance monadTransHalogenApp :: MonadTrans HalogenApp

derive newtype instance monadStateHalogenApp :: MonadState (State ExampleContracts) (HalogenApp m)

derive newtype instance monadAskHalogenApp :: MonadAsk env m => MonadAsk env (HalogenApp m)

derive newtype instance monadEffectHalogenApp :: MonadEffect m => MonadEffect (HalogenApp m)

derive newtype instance monadAffHalogenApp :: MonadAff m => MonadAff (HalogenApp m)

instance monadAnimateHalogenApp :: MonadAff m => MonadAnimate (HalogenApp m) (State ExampleContracts) where
  animate toggle action = HalogenApp $ animate toggle (unwrap action)

instance monadClipboardHalogenApp :: MonadEffect m => MonadClipboard (HalogenApp m) where
  copy = liftEffect <<< copy

------------------------------------------------------------
runHalogenApp :: forall m a. HalogenApp m a -> HalogenM (State ExampleContracts) (HAction ExampleContracts) () Output m a
runHalogenApp = unwrap

newtype Env
  = Env SPSettings_

derive instance newtypeEnv :: Newtype Env _

instance hasSpSettingsEnv :: HasSPSettings Env where
  spSettings = unwrap

instance monadAppHalogenApp :: (MonadAff m, MonadAsk Env m) => MonadApp (HalogenApp m) where
  getFullReport = runAjax getApiFullreport
  getContractInstanceStatus contractInstanceId =
    runAjax
      $ getApiContractInstanceByContractinstanceidStatus
          contractInstanceId
  getContractInstances = runAjax <<< getApiContractInstances
  getContractDefinitions = runAjax getApiContractDefinitions
  invokeEndpoint payload contractInstanceId endpointDescription =
    runAjax
      $ postApiContractInstanceByContractinstanceidEndpointByEndpointname
          payload
          (view _contractInstanceIdString contractInstanceId)
          (view _getEndpointDescription endpointDescription)
  activateContract contract = runAjax $ postApiContractActivate contract
  sendWebSocketMessage msg = HalogenApp $ raise $ SendWebSocketMessage msg
  log str = liftEffect $ Console.log str

runAjax :: forall m a. Functor m => ExceptT AjaxError m a -> m (WebData a)
runAjax action = RemoteData.fromEither <$> runExceptT action

-- Not using the generated purescript function to avoid double encoding of RawJson which results always as a JSON String
postApiContractInstanceByContractinstanceidEndpointByEndpointname :: forall m. MonadError AjaxError m => MonadAff m => RawJson -> String -> String -> m Unit
postApiContractInstanceByContractinstanceidEndpointByEndpointname (RawJson jsonString) contractInstanceId endpoint = do
  let
    req =
      { content: Just $ string jsonString
      , headers: defaultRequest.headers
      , method: Left POST
      , password: Nothing
      , responseFormat: Response.json
      , timeout: Nothing
      , url: "/api/contract/instance/" <> contractInstanceId <> "/endpoint/" <> endpoint
      , username: Nothing
      , withCredentials: false
      }
  result <- liftAff $ request req
  response <- case result of
    Left err -> throwError $ { request: req, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: req, description: UnexpectedHTTPStatus response }
  case decodeJson response.body of
    Left err -> throwError $ { request: req, description: DecodingError err }
    Right body -> pure body

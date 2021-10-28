module Types where

import Prologue
import Chain.Types as Chain
import Clipboard as Clipboard
import ContractExample (ExampleContracts)
import Control.Monad.Gen as Gen
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Lens (Getter', Iso', Traversal', Lens', to, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Data.UUID.Argonaut as UUID
import Ledger.Index (UtxoIndex)
import Network.RemoteData (RemoteData)
import Network.StreamData (StreamData)
import Network.StreamData as Stream
import Playground.Types (FunctionSchema)
import Plutus.Contract.Effects (ActiveEndpoint, PABReq)
import Plutus.Contract.Resumable (Request)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Webserver.Types (ChainReport, ContractReport, ContractInstanceClientState, ContractSignatureResponse, _ChainReport, _ContractReport, _ContractInstanceClientState, _ContractSignatureResponse, CombinedWSStreamToClient, CombinedWSStreamToServer)
import Plutus.V1.Ledger.Tx (Tx)
import Plutus.V1.Ledger.TxId (TxId)
import Schema (FormSchema)
import Schema.Types (FormArgument, FormEvent)
import Servant.PureScript (AjaxError)
import Test.QuickCheck (class Arbitrary)
import Wallet.Rollup.Types (AnnotatedTx)
import Wallet.Types (ContractInstanceId, EndpointDescription)
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS
import WebSocket.Support (FromSocket) as WS

data Query a
  = ReceiveWebSocketMessage (WS.FromSocket CombinedWSStreamToClient) a

data Output
  = SendWebSocketMessage CombinedWSStreamToServer

data StreamError
  = DecodingError JsonDecodeError
  | ServerError String
  | TransportError AjaxError

type WebStreamData
  = StreamData StreamError

type WebData
  = RemoteData AjaxError

fromWebData :: forall a. WebData a -> WebStreamData a
fromWebData = Stream.fromRemoteData <<< lmap TransportError

data HAction a
  = Init
  | ChangeView View
  | LoadFullReport
  | ActivateContract a
  | ChainAction Chain.Action
  | ClipboardAction Clipboard.Action
  | ChangeContractEndpointCall ContractInstanceId Int FormEvent
  | InvokeContractEndpoint ContractInstanceId EndpointForm

type ContractStates
  = Map ContractInstanceId (WebStreamData (ContractInstanceClientState ExampleContracts /\ Array EndpointForm))

newtype ContractSignatures a
  = ContractSignatures
  { unContractSignatures :: Array (ContractSignatureResponse a)
  }

derive instance genericContractSignatures :: Generic (ContractSignatures a) _

derive instance newtypeContractSignatures :: Newtype (ContractSignatures a) _

_ContractSignatures :: forall a. Iso' (ContractSignatures a) { unContractSignatures :: Array (ContractSignatureResponse a) }
_ContractSignatures = _Newtype

data WebSocketStatus
  = WebSocketOpen
  | WebSocketClosed (Maybe WS.CloseEvent)

derive instance genericWebSocketStatus :: Generic WebSocketStatus _

instance showWebSocketStatus :: Show WebSocketStatus where
  show WebSocketOpen = "WebSocketOpen"
  show (WebSocketClosed Nothing) = "WebSocketClosed"
  show (WebSocketClosed (Just closeEvent)) = "WebSocketClosed " <> WS.reason closeEvent

newtype State a
  = State
  { currentView :: View
  , contractSignatures :: WebStreamData (ContractSignatures a)
  , chainReport :: WebData ChainReport
  , chainState :: Chain.State
  , contractStates :: ContractStates
  , webSocketMessage :: WebStreamData CombinedWSStreamToClient
  , webSocketStatus :: WebSocketStatus
  }

type EndpointForm
  = { schema :: FunctionSchema FormSchema
    , argument :: FormArgument
    }

derive instance newtypeState :: Newtype (State a) _

derive instance genericState :: Generic (State a) _

_currentView :: forall a. Lens' (State a) View
_currentView = _Newtype <<< prop (Proxy :: _ "currentView")

_contractSignatures :: forall s r a. Newtype s { contractSignatures :: a | r } => Lens' s a
_contractSignatures = _Newtype <<< prop (Proxy :: _ "contractSignatures")

_chainReport :: forall s r a. Newtype s { chainReport :: a | r } => Lens' s a
_chainReport = _Newtype <<< prop (Proxy :: _ "chainReport")

_events :: forall s r a. Newtype s { events :: a | r } => Lens' s a
_events = _Newtype <<< prop (Proxy :: _ "events")

_chainState :: forall a. Lens' (State a) Chain.State
_chainState = _Newtype <<< prop (Proxy :: _ "chainState")

_contractStates :: forall a. Lens' (State a) ContractStates
_contractStates = _Newtype <<< prop (Proxy :: _ "contractStates")

_annotatedBlockchain :: Lens' ChainReport (Array (Array AnnotatedTx))
_annotatedBlockchain = _ChainReport <<< prop (Proxy :: _ "annotatedBlockchain")

_transactionMap :: Lens' ChainReport (Map TxId Tx)
_transactionMap = _ChainReport <<< prop (Proxy :: _ "transactionMap")

_webSocketMessage :: forall s a r. Newtype s { webSocketMessage :: a | r } => Lens' s a
_webSocketMessage = _Newtype <<< prop (Proxy :: _ "webSocketMessage")

_webSocketStatus :: forall s a r. Newtype s { webSocketStatus :: a | r } => Lens' s a
_webSocketStatus = _Newtype <<< prop (Proxy :: _ "webSocketStatus")

_contractReport :: forall s a r. Newtype s { contractReport :: a | r } => Lens' s a
_contractReport = _Newtype <<< prop (Proxy :: _ "contractReport")

_utxoIndex :: Lens' ChainReport UtxoIndex
_utxoIndex = _ChainReport <<< prop (Proxy :: _ "utxoIndex")

_crAvailableContracts :: forall t. Lens' (ContractReport t) (Array (ContractSignatureResponse t))
_crAvailableContracts = _ContractReport <<< prop (Proxy :: _ "crAvailableContracts")

_crActiveContractStates :: forall t. Lens' (ContractReport t) (Array (Tuple ContractInstanceId (PartiallyDecodedResponse PABReq)))
_crActiveContractStates = _ContractReport <<< prop (Proxy :: _ "crActiveContractStates")

_csrDefinition :: forall t. Lens' (ContractSignatureResponse t) t
_csrDefinition = _ContractSignatureResponse <<< prop (Proxy :: _ "csrDefinition")

_unContractSignatures :: forall t. Lens' (ContractSignatures t) (Array (ContractSignatureResponse t))
_unContractSignatures = _ContractSignatures <<< prop (Proxy :: _ "unContractSignatures")

_csContract :: forall t. Lens' (ContractInstanceClientState t) ContractInstanceId
_csContract = _Newtype <<< prop (Proxy :: _ "cicContract")

_csCurrentState :: forall t. Lens' (ContractInstanceClientState t) (PartiallyDecodedResponse ActiveEndpoint)
_csCurrentState = _Newtype <<< prop (Proxy :: _ "cicCurrentState")

_csContractDefinition :: forall t. Lens' (ContractInstanceClientState t) t
_csContractDefinition = _ContractInstanceClientState <<< prop (Proxy :: _ "cicDefinition")

_hooks :: forall t. Lens' (PartiallyDecodedResponse t) (Array (Request t))
_hooks = _Newtype <<< prop (Proxy :: _ "hooks")

_activeEndpoint :: Lens' ActiveEndpoint EndpointDescription
_activeEndpoint = _Newtype <<< prop (Proxy :: _ "aeDescription")

_contractActiveEndpoints :: Traversal' (PartiallyDecodedResponse ActiveEndpoint) EndpointDescription
_contractActiveEndpoints =
  _hooks
    <<< traversed
    <<< _rqRequest
    <<< _activeEndpoint

_rqRequest :: forall t. Lens' (Request t) t
_rqRequest = _Newtype <<< prop (Proxy :: _ "rqRequest")

_contractInstanceId :: Lens' ContractInstanceId UUID
_contractInstanceId = _Newtype <<< prop (Proxy :: _ "unContractInstanceId")

_contractInstanceIdString :: Getter' ContractInstanceId String
_contractInstanceIdString = _contractInstanceId <<< to UUID.toString

------------------------------------------------------------
data View
  = ActiveContracts
  | Blockchain
  | EventLog

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (ActiveContracts :| [ Blockchain, EventLog ])

instance showView :: Show View where
  show = genericShow

------------------------------------------------------------
_getPubKeyHash :: forall s r a. Newtype s { getPubKeyHash :: a | r } => Lens' s a
_getPubKeyHash = _Newtype <<< prop (Proxy :: _ "getPubKeyHash")

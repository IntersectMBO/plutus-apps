module Chain.Types where

import Prologue
import Clipboard (Action) as Clipboard
import Data.BigInt.Argonaut (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Fold', Iso', Lens', Prism', Traversal', filtered, prism', traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Ledger.Address (PaymentPubKeyHash(..))
import Plutus.V1.Ledger.Address (Address(..))
import Plutus.V1.Ledger.Credential (Credential(..))
import Ledger.Crypto (PubKey, Signature)
import Plutus.V1.Ledger.Interval (Interval)
import Ledger.Slot (Slot)
import Ledger.Tx.Internal (Tx, TxInput, TxIn, TxOut(..))
import Plutus.V1.Ledger.Tx (TxId, TxOutRef)
import Cardano.Api.TxBody as C
import Plutus.V1.Ledger.Value (Value)
import Wallet.Rollup.Types (AnnotatedTx(..), BeneficialOwner(..), DereferencedInput, SequenceId, TxKey, _TxKey)

data Action
  = FocusTx (Maybe TxId)
  | ClipboardAction Clipboard.Action

derive instance genericChainFocus :: Generic Action _

instance showChainFocus :: Show Action where
  show = genericShow

_FocusTx :: Prism' Action TxId
_FocusTx = prism' set get
  where
  get (FocusTx txId) = txId

  get _ = Nothing

  set = FocusTx <<< Just

newtype AnnotatedBlockchain = AnnotatedBlockchain (Array (Array AnnotatedTx))

derive instance newtypeAnnotatedBlockchain :: Newtype AnnotatedBlockchain _

_AnnotatedBlockchain :: Iso' AnnotatedBlockchain (Array (Array AnnotatedTx))
_AnnotatedBlockchain = _Newtype

_AnnotatedBlocks :: Traversal' AnnotatedBlockchain AnnotatedTx
_AnnotatedBlocks = _AnnotatedBlockchain <<< traversed <<< traversed

type State =
  { chainFocus :: Maybe TxId
  , chainFocusAppearing :: Boolean
  , chainFocusAge :: Ordering
  }

initialState :: State
initialState =
  { chainFocus: Nothing
  , chainFocusAppearing: false
  , chainFocusAge: EQ
  }

_chainFocus :: forall r a. Lens' { chainFocus :: a | r } a
_chainFocus = prop (Proxy :: _ "chainFocus")

_chainFocusAppearing :: forall r a. Lens' { chainFocusAppearing :: a | r } a
_chainFocusAppearing = prop (Proxy :: _ "chainFocusAppearing")

_chainFocusAge :: forall r a. Lens' { chainFocusAge :: a | r } a
_chainFocusAge = prop (Proxy :: _ "chainFocusAge")

_sequenceId :: Lens' AnnotatedTx SequenceId
_sequenceId = _Newtype <<< prop (Proxy :: _ "sequenceId")

_dereferencedInputs :: Lens' AnnotatedTx (Array DereferencedInput)
_dereferencedInputs = _Newtype <<< prop (Proxy :: _ "dereferencedInputs")

_value :: forall s a r. Newtype s { getValue :: a | r } => Lens' s a
_value = _Newtype <<< prop (Proxy :: _ "getValue")

_txIdOf :: Lens' AnnotatedTx TxId
_txIdOf = _Newtype <<< prop (Proxy :: _ "txId")

_balances :: Lens' AnnotatedTx (Map BeneficialOwner Value)
_balances = _Newtype <<< prop (Proxy :: _ "balances")

_txFee :: Lens' Tx Value
_txFee = _Newtype <<< prop (Proxy :: _ "txFee")

_txMint :: Lens' Tx Value
_txMint = _Newtype <<< prop (Proxy :: _ "txMint")

_txValidRange :: Lens' Tx (Interval Slot)
_txValidRange = _Newtype <<< prop (Proxy :: _ "txValidRange")

_txSignatures :: Lens' Tx (Map PubKey Signature)
_txSignatures = _Newtype <<< prop (Proxy :: _ "txSignatures")

_txInputs :: Lens' Tx (Array TxInput)
_txInputs = _Newtype <<< prop (Proxy :: _ "txInputs")

_txOutputs :: Lens' Tx (Array TxOut)
_txOutputs = _Newtype <<< prop (Proxy :: _ "txOutputs")

_txInRef :: Lens' TxIn TxOutRef
_txInRef = _Newtype <<< prop (Proxy :: _ "txInRef")

_txInputRef :: Lens' TxInput TxOutRef
_txInputRef = _Newtype <<< prop (Proxy :: _ "txInputRef")

_txOutRefId :: Lens' TxOutRef TxId
_txOutRefId = _Newtype <<< prop (Proxy :: _ "txOutRefId")

_txKeyTxId :: Lens' TxKey TxId
_txKeyTxId = _TxKey <<< prop (Proxy :: _ "_txKeyTxId")

_txKeyTxOutRefIdx :: Lens' TxKey BigInt
_txKeyTxOutRefIdx = _TxKey <<< prop (Proxy :: _ "_txKeyTxOutRefIdx")

toBeneficialOwner :: TxOut -> BeneficialOwner
toBeneficialOwner (TxOut { getTxOut: C.TxOut { txOutAddress } }) =
  let
    Address { addressCredential } = txOutAddress
  in
    case addressCredential of
      PubKeyCredential pkh -> OwnedByPaymentPubKey $ PaymentPubKeyHash { unPaymentPubKeyHash: pkh }
      ScriptCredential vh -> OwnedByScript vh

_findTx :: forall m. Monoid m => TxId -> Fold' m AnnotatedBlockchain AnnotatedTx
_findTx focussedTxId = (_AnnotatedBlocks <<< filtered isAnnotationOf)
  where
  isAnnotationOf :: AnnotatedTx -> Boolean
  isAnnotationOf (AnnotatedTx { txId }) = txId == focussedTxId

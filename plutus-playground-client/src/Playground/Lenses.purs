-- | Misc. useful lenses.
module Playground.Lenses where

import Data.Function ((<<<))
import Data.Map (Map)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Ledger.Index.Internal (UtxoIndex, _UtxoIndex)
import Plutus.V1.Ledger.Tx (TxOut, TxOutRef)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, _CurrencySymbol, _TokenName)

_currencySymbol :: Lens' CurrencySymbol String
_currencySymbol = _CurrencySymbol <<< prop (Proxy :: _ "unCurrencySymbol")

_tokenName :: Lens' TokenName String
_tokenName = _TokenName <<< prop (Proxy :: _ "unTokenName")

_amount :: forall r a. Lens' { amount :: a | r } a
_amount = prop (Proxy :: _ "amount")

_recipient :: forall r a. Lens' { recipient :: a | r } a
_recipient = prop (Proxy :: _ "recipient")

_endpointDescription :: forall r a. Lens' { endpointDescription :: a | r } a
_endpointDescription = prop (Proxy :: _ "endpointDescription")

_getEndpointDescription :: forall s r a. Newtype s { getEndpointDescription :: a | r } => Lens' s a
_getEndpointDescription = _Newtype <<< prop (Proxy :: _ "getEndpointDescription")

_endpointValue :: forall s r a. Newtype s { unEndpointValue :: a | r } => Lens' s a
_endpointValue = _Newtype <<< prop (Proxy :: _ "unEndpointValue")

_schema :: forall r a. Lens' { schema :: a | r } a
_schema = prop (Proxy :: _ "schema")

_txConfirmed :: forall s r a. Newtype s { unTxConfirmed :: a | r } => Lens' s a
_txConfirmed = _Newtype <<< prop (Proxy :: _ "unTxConfirmed")

_contractInstanceTag :: forall s r a. Newtype s { unContractInstanceTag :: a | r } => Lens' s a
_contractInstanceTag = _Newtype <<< prop (Proxy :: _ "unContractInstanceTag")

_txId :: Lens' TxId String
_txId = _Newtype <<< prop (Proxy :: _ "getTxId")

_utxoIndexEntries :: Lens' UtxoIndex (Map TxOutRef TxOut)
_utxoIndexEntries = _UtxoIndex <<< prop (Proxy :: _ "getIndex")

_aeDescription :: forall s r a. Newtype s { aeDescription :: a | r } => Lens' s a
_aeDescription = _Newtype <<< prop (Proxy :: _ "aeDescription")

_aeMetadata :: forall s r a. Newtype s { aeMetadata :: a | r } => Lens' s a
_aeMetadata = _Newtype <<< prop (Proxy :: _ "aeMetadata")

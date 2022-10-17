module Chain.View
  ( chainView
  , txOutOfView
  ) where

import Chain.Types
import Animation (animationClass)
import Bootstrap (active, card, cardBody_, cardFooter_, cardHeader, cardHeader_, col, col3_, col6_, colLg2, colMd3, colSm6, colXs12, col_, empty, nbsp, row, row_, tableBordered, tableSmall, textTruncate)
import Bootstrap as Bootstrap
import Bootstrap.Extra (clickable)
import Cardano.Api.TxBody as C
import Clipboard (showShortCopyLong)
import Data.Array ((:))
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Foldable (foldMap, foldr)
import Data.Foldable.Extra (interleave)
import Data.FoldableWithIndex (foldMapWithIndex, foldrWithIndex)
import Data.Lens (Traversal', _Just, filtered, has, preview, view)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Extra (abbreviate)
import Data.Tuple.Nested ((/\))
import Halogen.HTML (ClassName(..), HTML, IProp, br_, div, div_, h2_, hr_, p_, small_, span_, strong_, table, tbody_, td, text, th, th_, thead_, tr, tr_)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, colSpan, rowSpan)
import PlutusTx.AssocMap as AssocMap
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import Ledger.Address (PaymentPubKeyHash(..))
import Ledger.Tx.Internal (TxOut(..))
import Plutus.V1.Ledger.Tx (TxId(..))
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import Prologue (Ordering(..), Tuple, const, eq, show, zero, ($), (<$>), (<<<), (<>))
import Wallet.Rollup.Types (AnnotatedTx(..), BeneficialOwner(..), DereferencedInput(..), SequenceId(..))
import Web.UIEvent.MouseEvent (MouseEvent)

type NamingFn = PubKeyHash -> Maybe String

chainView :: forall p. NamingFn -> State -> AnnotatedBlockchain -> HTML p Action
chainView namingFn state annotatedBlockchain =
  div
    [ classes
        ( [ ClassName "chain" ]
            <> animationClass _chainFocusAppearing state
            <> case state.chainFocusAge of
              LT -> [ ClassName "chain-focus-newer" ]
              EQ -> []
              GT -> [ ClassName "chain-focus-older" ]
        )
    ]
    [ h2_
        [ text "Blockchain" ]
    , p_
        [ text "Click a transaction for details" ]
    , div
        [ classes [ row, ClassName "blocks" ] ]
        (chainSlotView state <$> Array.reverse (unwrap annotatedBlockchain))
    , detailView namingFn state annotatedBlockchain
    ]

slotClass :: ClassName
slotClass = ClassName "slot"

notFoundClass :: ClassName
notFoundClass = ClassName "not-found"

amountClass :: ClassName
amountClass = ClassName "amount"

chainSlotView :: forall p. State -> Array AnnotatedTx -> HTML p Action
chainSlotView _ [] = empty

chainSlotView state chainSlot =
  div [ classes [ colXs12, colSm6, colMd3, colLg2, slotClass ] ]
    (blockView state <$> chainSlot)

blockView :: forall p. State -> AnnotatedTx -> HTML p Action
blockView state (AnnotatedTx { txId, sequenceId }) =
  div
    [ classes ([ card, clickable, ClassName "transaction" ] <> if isActive then [ active ] else [])
    , onClickFocusTx txId
    ]
    [ entryCardHeader sequenceId false ]
  where
  isActive = has (_chainFocus <<< _Just <<< filtered (eq txId)) state

detailView :: forall p. NamingFn -> State -> AnnotatedBlockchain -> HTML p Action
detailView namingFn { chainFocus: Just focussedTxId } annotatedBlockchain = case preview (_findTx focussedTxId) annotatedBlockchain of
  Just annotatedTx -> transactionDetailView namingFn annotatedBlockchain annotatedTx
  Nothing -> empty

detailView _ { chainFocus: Nothing } _ = empty

transactionDetailView :: forall p. NamingFn -> AnnotatedBlockchain -> AnnotatedTx -> HTML p Action
transactionDetailView namingFn annotatedBlockchain annotatedTx =
  div
    [ class_ $ ClassName "detail" ]
    [ div
        [ classes [ row, ClassName "transaction" ] ]
        [ col3_
            [ h2_ [ text "Inputs" ]
            , div_ (dereferencedInputView namingFn annotatedBlockchain <$> (view _dereferencedInputs annotatedTx))
            ]
        , col6_
            [ h2_ [ text "Transaction" ]
            , div [ classes [ card, active ] ]
                [ entryCardHeader (view _sequenceId annotatedTx) true
                , cardBody_
                    [ div
                        [ class_ textTruncate ]
                        [ txIdView (view _txIdOf annotatedTx) ]
                    , div [ class_ textTruncate ] []
                    , div [ class_ textTruncate ] []
                    ]
                ]
            ]
        ]
    , balancesTable
        namingFn
        (view _sequenceId annotatedTx)
        (view _balances annotatedTx)
    ]

entryCardHeader :: forall i p. SequenceId -> Boolean -> HTML p i
entryCardHeader sequenceId withTriangle =
  cardHeader_
    if withTriangle then
      [ triangleRight, sequenceIdView sequenceId ]
    else
      [ sequenceIdView sequenceId ]

entryClass :: ClassName
entryClass = ClassName "entry"

triangleRight :: forall p i. HTML p i
triangleRight = div [ class_ $ ClassName "triangle-right" ] []

balancesTable :: forall p. NamingFn -> SequenceId -> Map BeneficialOwner Value -> HTML p Action
balancesTable namingFn sequenceId balances =
  div
    [ class_ $ ClassName "balances" ]
    [ h2_
        [ text "Balances Carried Forward"
        , nbsp
        , small_
            [ text "(as at "
            , sequenceIdView sequenceId
            , text ")"
            ]
        ]
    , table
        [ classes
            [ Bootstrap.table
            , tableBordered
            , tableSmall
            , ClassName "balances-table"
            ]
        ]
        [ thead_
            [ tr_
                ( th [ rowSpan 2 ] [ text "Beneficial Owner" ]
                    : foldMapWithIndex
                        ( \currency s ->
                            [ th
                                [ colSpan (Set.size s)
                                , class_ textTruncate
                                ]
                                [ let
                                    formatted = showCurrency currency
                                  in
                                    ClipboardAction
                                      <$> showShortCopyLong
                                        formatted
                                        (Just [ text $ abbreviate 15 formatted ])
                                ]
                            ]
                        )
                        headings
                )
            , tr_
                (foldMap (foldMap tokenHeadingView) headings)
            ]
        , tbody_
            ( foldMap
                ( \owner ->
                    [ tr [ class_ $ beneficialOwnerClass owner ]
                        ( th_
                            [ beneficialOwnerView namingFn owner ]
                            : foldMapWithIndex
                                ( \currency ->
                                    foldMap
                                      ( \token ->
                                          let
                                            _thisBalance :: Traversal' (Map BeneficialOwner Value) BigInt
                                            _thisBalance = ix owner <<< _value <<< ix currency <<< ix token

                                            amount :: Maybe BigInt
                                            amount = preview _thisBalance balances
                                          in
                                            [ td [ class_ amountClass ]
                                                [ text $ formatAmount $ fromMaybe zero amount ]
                                            ]
                                      )
                                )
                                headings
                        )
                    ]
                )
                (Map.keys balances)
            )
        ]
    ]
  where
  headings :: Map CurrencySymbol (Set TokenName)
  headings = collectBalanceTableHeadings balances

  tokenHeadingView :: forall i. TokenName -> Array (HTML p i)
  tokenHeadingView token = [ th_ [ showToken token ] ]

collectBalanceTableHeadings :: Map BeneficialOwner Value -> Map CurrencySymbol (Set TokenName)
collectBalanceTableHeadings balances = foldr collectCurrencies Map.empty $ Map.values balances
  where
  collectCurrencies :: Value -> Map CurrencySymbol (Set TokenName) -> Map CurrencySymbol (Set TokenName)
  collectCurrencies (Value { getValue: entries }) ownersBalance = foldrWithIndex collectTokenNames ownersBalance entries

  collectTokenNames :: CurrencySymbol -> AssocMap.Map TokenName BigInt -> Map CurrencySymbol (Set TokenName) -> Map CurrencySymbol (Set TokenName)
  collectTokenNames currency currencyBalances = Map.insertWith Set.union currency $ AssocMap.keys currencyBalances

sequenceIdView :: forall p i. SequenceId -> HTML p i
sequenceIdView (SequenceId { slotIndex, txIndex }) =
  span_
    [ span_ [ text "Slot" ]
    , nbsp
    , span_ [ text $ show slotIndex ]
    , span_ [ text ", " ]
    , span_ [ text "Tx" ]
    , nbsp
    , span_ [ text $ show txIndex ]
    ]

txIdView :: forall p. TxId -> HTML p Action
txIdView (TxId { getTxId: str }) =
  ClipboardAction
    <$> showShortCopyLong
      str
      ( Just
          [ strong_ [ text "Tx: " ]
          , nbsp
          , text str
          ]
      )

dereferencedInputView :: forall p. NamingFn -> AnnotatedBlockchain -> DereferencedInput -> HTML p Action
dereferencedInputView namingFn annotatedBlockchain (DereferencedInput { originalInput, refersTo }) =
  txOutOfView namingFn true refersTo
    $ case originatingTx of
        Just tx ->
          Just
            $ div
                [ class_ clickable
                , onClickFocusTx txId
                ]
                [ text "Created by:", nbsp, sequenceIdView (view _sequenceId tx) ]
        Nothing -> Nothing
  where
  txId :: TxId
  txId = view (_txInRef <<< _txOutRefId) originalInput

  originatingTx :: Maybe AnnotatedTx
  originatingTx = preview (_findTx txId) annotatedBlockchain

dereferencedInputView _ _ (InputNotFound txKey) =
  div
    [ classes [ card, entryClass, notFoundClass ] ]
    [ div [ classes [ cardHeader, textTruncate ] ]
        [ text "Input Not Found" ]
    , cardBody_
        [ txIdView (view _txKeyTxId txKey)
        , br_
        , text $ "Index: " <> BigInt.toString (view _txKeyTxOutRefIdx txKey)
        ]
    ]

txOutOfView :: forall p. NamingFn -> Boolean -> TxOut -> Maybe (HTML p Action) -> HTML p Action
txOutOfView namingFn showArrow txOut@(TxOut { getTxOut: C.TxOut { txOutValue } }) mFooter =
  div
    [ classes [ card, entryClass, beneficialOwnerClass beneficialOwner ] ]
    [ div [ classes [ cardHeader, textTruncate ] ]
        [ if showArrow then triangleRight else empty
        , beneficialOwnerView namingFn beneficialOwner
        ]
    , cardBody_
        [ valueView txOutValue ]
    , case mFooter of
        Nothing -> empty
        Just footer -> cardFooter_ [ footer ]
    ]
  where
  beneficialOwner = toBeneficialOwner txOut

beneficialOwnerClass :: BeneficialOwner -> ClassName
beneficialOwnerClass (OwnedByPaymentPubKey _) = ClassName "wallet"

beneficialOwnerClass (OwnedByScript _) = ClassName "script"

beneficialOwnerView :: forall p. NamingFn -> BeneficialOwner -> HTML p Action
beneficialOwnerView namingFn (OwnedByPaymentPubKey (PaymentPubKeyHash { unPaymentPubKeyHash: pubKeyHash })) = case namingFn pubKeyHash of
  Nothing -> showPubKeyHash pubKeyHash
  Just name ->
    span_
      [ span_ [ text name ]
      , br_
      , small_ [ showPubKeyHash pubKeyHash ]
      ]

beneficialOwnerView _ (OwnedByScript a) =
  ClipboardAction
    <$> showShortCopyLong a
      ( Just
          [ text "Script"
          , nbsp
          , text a
          ]
      )

showPubKeyHash :: forall p. PubKeyHash -> HTML p Action
showPubKeyHash (PubKeyHash { getPubKeyHash: p }) =
  ClipboardAction
    <$> showShortCopyLong p
      ( Just
          [ text "PubKeyHash"
          , nbsp
          , text p
          ]
      )

valueView :: forall p i. Value -> HTML p i
valueView (Value { getValue: (AssocMap.Map []) }) = empty

valueView (Value { getValue: (AssocMap.Map currencies) }) = div_ (interleave hr_ (currencyView <$> currencies))
  where
  currencyView :: Tuple CurrencySymbol (AssocMap.Map TokenName BigInt) -> HTML p i
  currencyView (currency /\ (AssocMap.Map tokens)) =
    div_
      [ div [ class_ Bootstrap.textTruncate ]
          [ strong_ [ text $ showCurrency currency ] ]
      , div_ (tokenView <$> tokens)
      ]

  tokenView :: Tuple TokenName BigInt -> HTML p i
  tokenView (token /\ amount) =
    row_
      [ col_ [ showToken token ]
      , div [ classes [ col, amountClass ] ]
          [ text $ formatAmount amount ]
      ]

formatAmount :: BigInt -> String
formatAmount = BigInt.toString

showCurrency :: CurrencySymbol -> String
showCurrency (CurrencySymbol { unCurrencySymbol: "" }) = "Ada"

showCurrency (CurrencySymbol { unCurrencySymbol: symbol }) = symbol

showToken :: forall p i. TokenName -> HTML p i
showToken (TokenName { unTokenName: "" }) = text "Lovelace"

showToken (TokenName { unTokenName: name }) = text name

onClickFocusTx :: forall p. TxId -> IProp (onClick :: MouseEvent | p) Action
onClickFocusTx txId =
  onClick
    $ const
    $ FocusTx
    $ Just txId

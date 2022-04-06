module Main where

import Prologue
import Affjax (get, post, printError) as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import AppM (AppM, Env, runAppM)
import Cardano.Wallet.Nami (WalletId)
import Cardano.Wallet.Nami as Nami
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Reader (class MonadAsk)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Array (head)
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano (CardanoWasm, loadCardanoWasm)
import Data.Cardano.Address (Address)
import Data.Cardano.BaseAddress as BaseAddress
import Data.Cardano.Ed25519KeyHash as Ed25519KeyHash
import Data.Cardano.StakeCredential as StakeCredential
import Data.Cardano.Transaction as Transaction
import Data.Cardano.TransactionWitnessSet as TransactionWitnessSet
import Data.Const (Const)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Milliseconds(Milliseconds), attempt, delay, error, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Foreign.Object as FO
import Foreign.Object as Object
import Formless as F
import Halogen (ClassName(ClassName), defaultEval)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import PaymentForm (Payment, PaymentForm, ChildSlots)
import PaymentForm as PaymentForm
import Text.Pretty (class Pretty, pretty, text)

type State =
  { cardanoWasm :: CardanoWasm
  , errors :: Set AppError
  , isLoadingWallet :: Boolean
  , isSubmittingPayment :: Boolean
  , lastSubmittedTxId :: Maybe String
  }

data Action
  = ConnectWallet
  | MakePayment Payment

data AppError
  = WalletConnectionError
  | MakePaymentAppError

derive instance eqAppError :: Eq AppError

derive instance ordAppError :: Ord AppError

instance prettyAppError :: Pretty AppError where
  pretty WalletConnectionError = text "There was an error while trying to connect to the browser wallet"
  pretty MakePaymentAppError = text "There was an error while submitting the payment."

main :: Effect Unit
main = do
  runHalogenAff do
    cardanoWasm <- loadCardanoWasm
    body <- awaitBody
    _ <- runUI (H.hoist (runAppM cardanoWasm) (mainComponent cardanoWasm)) unit body
    pure unit

mainComponent :: forall query input output. CardanoWasm -> H.Component query input output AppM
mainComponent cardanoWasm = do
  H.mkComponent
    { initialState: const $ initialState cardanoWasm
    , render
    , eval:
        H.mkEval
          defaultEval
            { handleAction = handleAction
            , initialize = Just ConnectWallet
            }
    }

-- TODO Put more sensible default values. These are used for testing purposes.
initialState :: CardanoWasm -> State
initialState cardanoWasm = do
  { cardanoWasm
  , errors: Set.empty
  , isLoadingWallet: Nami.isEnabled
  , isSubmittingPayment: false
  , lastSubmittedTxId: Nothing
  }

type ChildSlot = (formless :: F.Slot PaymentForm (Const Void) ChildSlots Payment Unit)

render :: State -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
render s =
  HH.div [ HP.class_ <<< ClassName $ "pa4 black-80" ]
    $
      [ HH.h1_ [ HH.text "PAB-Nami demo" ]
      , HH.p_ [ HH.text "This simple demo allows you to send a lovelace amount to another Cardano testnet address using the PAB (PayToWallet contract) and the Nami wallet." ]
      , HH.slot F._formless unit (PaymentForm.component s.cardanoWasm) unit MakePayment
      , modal s.isLoadingWallet (modalView s "Connecting to Nami...")
      , modal s.isSubmittingPayment (modalView s "Currently submitting the payment to the Cardano blockchain testnet...")
      ]
        <> submittedPaymentHtml
        <>
          [ HH.div_
              [ HH.ul_
                  $ map (\e -> HH.li [ HP.class_ <<< ClassName $ "light-red" ] [ HH.text $ show $ pretty e ])
                  $ Set.toUnfoldable s.errors
              ]
          ]
  where
  submittedPaymentHtml = case s.lastSubmittedTxId of
    Nothing -> []
    Just txId ->
      [ HH.div_
          [ HH.p_
              [ HH.text "Successfully submitted "
              , HH.a
                  [ HP.href $ "https://testnet.cardanoscan.io/transaction/" <> txId ]
                  [ HH.text "the transaction " ]
              , HH.text "to the Cardano testnet (need to wait a bit for the transaction to appear)."
              ]
          ]
      ]

modal
  :: Boolean
  -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
  -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
modal isVisible view =
  let
    displayClass = if isVisible then "flex" else "dn"

    containerClasses =
      "bottom-0 fixed items-center justify-center left-0 right-0 top-0"
        <> " "
        <> displayClass
  in
    HH.div [ HP.class_ <<< ClassName $ containerClasses ]
      [ HH.div
          [ HP.class_ <<< ClassName $ "bg-white flex items-center justify-center mw7 relative w-90 z-4" ]
          [ view ]
      , HH.div
          [ HP.class_ <<< ClassName $ "bg-black bottom-0 fixed left-0 o-80 right-0 top-0 z-1" ]
          []
      ]

modalView :: State -> String -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
modalView _ text =
  HH.div [ HP.class_ <<< ClassName $ "flex flex-column items-center justify-center pa6" ]
    [ HH.h1_ [ HH.text text ]
    , HH.div [ HP.class_ <<< ClassName $ "loader" ] []
    ]

handleAction
  :: forall msg m
   . MonadAsk Env m
  => MonadAff m
  => MonadThrow Error m
  => MonadError Error m
  => Action
  -> H.HalogenM State Action ChildSlots msg m Unit
-- | Connect to the Nami wallet
handleAction ConnectWallet = do
  enableRes <- H.lift $ try $ void $ Nami.enable
  H.modify_ \s -> s { isLoadingWallet = false }
  case enableRes of
    Left _ -> H.modify_ \s -> s { errors = Set.singleton WalletConnectionError }
    Right _ -> pure unit

-- | Makes a payment given the recipient's bech32 addresses and the amount in Lovelace
handleAction (MakePayment payment) = do
  H.modify_ \s -> s { isSubmittingPayment = true, lastSubmittedTxId = Nothing }
  walletIdE <- H.liftAff $ try Nami.getWalletId
  case walletIdE of
    Left err -> do
      log $ show err
      H.modify_ \s ->
        s
          { isSubmittingPayment = false
          , lastSubmittedTxId = Nothing
          , errors = Set.singleton WalletConnectionError
          }
    Right walletId -> do
      makePaymentRes <-
        H.lift
          $ try
          $ makePayment
              walletId
              payment.recipientBech32Addr
              payment.lovelaceAmount
      case makePaymentRes of
        Left err -> do
          log $ show err
          H.modify_ \s ->
            s
              { isSubmittingPayment = false
              , lastSubmittedTxId = Nothing
              , errors = Set.singleton MakePaymentAppError
              }
        Right txId -> do
          H.modify_ \s ->
            s
              { isSubmittingPayment = false
              , lastSubmittedTxId = Just txId
              , errors = Set.empty :: Set AppError
              }

-- | Given a 'WalletId', an 'Address' and an amount in lovelace, use the PAB and
-- Nami wallet to balance, sign and submit a transaction.
makePayment
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => MonadAsk CardanoWasm m
  => WalletId
  -> Address
  -> Int
  -> m String
makePayment walletId recipientAddr lovelaceAmount = do
  pubKeyHashesHex <- getPaymentStakeKeyHashesHex recipientAddr
  cid <-
    H.liftAff
      $ do
          cid <- activateContract walletId
          callPayToWalletEndpoint cid pubKeyHashesHex lovelaceAmount
          -- Need to wait a bit to ensure the endpoint correctly handled
          -- the request and modifies the constract instance's status
          delay $ Milliseconds 2000.0
          pure cid
  partialCborTx <- fetchContractPartialTx cid
  balanceSignAndSubmitTx partialCborTx

-- | Activate the 'PayToWallet' contract in the PAB.
activateContract
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => Nami.WalletId
  -> m String
activateContract walletId = do
  let
    body =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "caID" (A.fromArray [])
            , Tuple "caWallet"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "getWalletId" (A.fromString walletId.unWalletId) ]
                    )
                )
            ]
        )
  resE <-
    H.liftAff
      $ AX.post ResponseFormat.json
          "/api/contract/activate"
          (Just (RequestBody.json body))
  res <- either (throwError <<< error <<< AX.printError) pure resE
  either (throwError <<< error <<< show)
    (\c -> pure $ c.unContractInstanceId)
    $ (decodeJson (res.body) :: Either JsonDecodeError { unContractInstanceId :: String })

-- | From a Cardano address, extract the payment key and stake key encoded in
-- CBOR.
getPaymentStakeKeyHashesHex
  :: forall m
   . MonadEffect m
  => MonadAsk Env m
  => Address
  -> m (Tuple String String)
getPaymentStakeKeyHashesHex address = do
  baseAddress <- BaseAddress.fromAddress address
  let
    paymentKeyHash =
      Ed25519KeyHash.toBytes
        $ StakeCredential.toKeyHash
        $ BaseAddress.paymentCred baseAddress
  paymentKeyHashHex <-
    liftEffect $ identity
      $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer paymentKeyHash) :: Effect Buffer)
          >>= Buffer.toString Encoding.Hex
  let
    stakeKeyHash =
      Ed25519KeyHash.toBytes
        $ StakeCredential.toKeyHash
        $ BaseAddress.stakeCred baseAddress
  stakeKeyHashHex <-
    liftEffect $ identity
      $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer stakeKeyHash) :: Effect Buffer)
          >>= Buffer.toString Encoding.Hex
  pure $ Tuple paymentKeyHashHex stakeKeyHashHex

-- | Call the endpoint of our contract to generate a transaction for a payment.
callPayToWalletEndpoint
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => String
  -> Tuple String String
  -> Int
  -> m Unit
callPayToWalletEndpoint cid (Tuple paymentKeyHashHex stakeKeyHashHex) lovelaceAmount = do
  -- TODO: Use the generated PS types from plutus-ledger to construct the JSON instead
  let
    endpointCallBody =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "amount"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "getValue"
                            ( A.fromArray
                                [ A.fromArray
                                    [ A.fromObject $ Object.fromFoldable [ Tuple "unCurrencySymbol" (A.fromString "") ]
                                    , A.fromArray
                                        [ A.fromArray
                                            [ A.fromObject $ Object.fromFoldable [ Tuple "unTokenName" (A.fromString "") ]
                                            , A.fromNumber $ toNumber lovelaceAmount
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    )
                )
            , Tuple "pkh"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "unPaymentPubKeyHash"
                            ( A.fromObject
                                ( Object.fromFoldable
                                    [ Tuple "getPubKeyHash" (A.fromString paymentKeyHashHex) ]
                                )
                            )
                        ]
                    )
                )
            , Tuple "skh"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "unStakePubKeyHash"
                            ( A.fromObject
                                ( Object.fromFoldable
                                    [ Tuple "getPubKeyHash" (A.fromString stakeKeyHashHex) ]
                                )
                            )
                        ]
                    )
                )
            ]
        )
  resE <-
    H.liftAff do
      AX.post ResponseFormat.json
        ("/api/contract/instance/" <> cid <> "/endpoint/PayToWallet")
        (Just (RequestBody.json endpointCallBody))
  either (throwError <<< error <<< AX.printError) (const $ pure unit) resE

-- | Fetch the partial transaction from the PAB.
fetchContractPartialTx
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => String
  -> m String
fetchContractPartialTx cid = do
  resE <-
    H.liftAff
      $ AX.get ResponseFormat.json ("/api/contract/instance/" <> cid <> "/status")
  res <- either (throwError <<< error <<< AX.printError) pure resE
  let
    partialTxCborM =
      (\y -> A.toObject y >>= \x -> FO.lookup "transaction" x >>= A.toString)
        =<< head
        =<< A.toArray
        =<< FO.lookup "cicYieldedExportTxs"
        =<< A.toObject res.body
  maybe (throwError $ error "Could not parse fields cicYieldedExportTxs.[transaction]")
    pure
    partialTxCborM

-- | Given a partial transaction encoded in CBOR, balance, sign and submit it
-- using the Nami wallet api.
balanceSignAndSubmitTx
  :: forall m
   . MonadAsk Env m
  => MonadAff m
  => MonadThrow Error m
  => String
  -> m String
balanceSignAndSubmitTx partialTxCbor = do
  balancedTxCbor <- H.liftAff $ Nami.balanceTx partialTxCbor
  balancedTxE <- mkFromCbor balancedTxCbor Transaction.fromBytes
  balancedTx <- either throwError pure balancedTxE
  txWitnessSetCborE <- H.liftAff $ attempt $ Nami.signTx balancedTxCbor Nothing
  txWitnessSetCbor <- either throwError pure txWitnessSetCborE
  txWitnessSetE <- mkFromCbor txWitnessSetCbor TransactionWitnessSet.fromBytes
  txWitnessSet <- either throwError pure txWitnessSetE
  finalTxE <- Transaction.new (Transaction.body balancedTx) txWitnessSet
  finalTx <- either throwError pure finalTxE
  finalTxCbor <- bytesToCbor $ Transaction.toBytes finalTx
  Nami.submitTx finalTxCbor

-- | Decode a CBOR string to the given type 'a'.
mkFromCbor
  :: forall m a
   . MonadEffect m
  => String
  -> (Uint8Array -> m (Either Error a))
  -> m (Either Error a)
mkFromCbor cbor mk = do
  bytes <-
    liftEffect $ (Buffer.fromString cbor Encoding.Hex :: Effect Buffer)
      >>= Buffer.toArrayBuffer
      >>= (\x -> ArrayBuffer.whole x :: Effect Uint8Array)
  mk bytes

-- | Convert a byte array to a CBOR string.
bytesToCbor
  :: forall m
   . MonadEffect m
  => Uint8Array
  -> m String
bytesToCbor bytes =
  liftEffect $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer bytes) :: Effect Buffer)
    >>= Buffer.toString Encoding.Hex

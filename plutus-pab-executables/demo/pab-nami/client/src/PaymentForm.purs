module PaymentForm where

import Prologue
import AppM (AppM)
import Data.Cardano (CardanoWasm)
import Data.Cardano.Address (Address)
import Data.Cardano.Address as Address
import Data.Const (Const)
import Data.Either (hush)
import Data.Int (fromString)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen (ClassName(ClassName))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

data Action = HandleSubmit Event

type Payment =
  { recipientBech32Addr :: Address
  , lovelaceAmount :: Int
  }

data AddressError = RecipientAddrNotInBech32

data LovelaceAmountError
  = LovelaceAmountLowerThanMin
  | LovelaceAmountNotAnInt

newtype PaymentForm (r :: Row Type -> Type) f = PaymentForm
  ( r
      ( recipientBech32Addr :: f AddressError String Address
      , lovelaceAmount :: f LovelaceAmountError String Int
      )
  )

derive instance newtypePaymentForm :: Newtype (PaymentForm r f) _

type ChildSlots = (formless :: F.Slot' PaymentForm Payment Unit)

component :: CardanoWasm -> F.Component PaymentForm (Const Void) () Unit Payment AppM
component cardanoWasm = F.component (const $ input cardanoWasm) spec

input :: forall m. Monad m => CardanoWasm -> F.Input' PaymentForm m
input cardanoWasm =
  { initialInputs: Just (F.wrapInputFields { recipientBech32Addr: "addr_test1qpvagl6ns8gfe7f0w74nylkyht53hlfhsvhpgvsf45c0cr4j2ftykgcgwpq4hma4mtjupynzky5pg9p9qzalu8zu3d6qscr5vz", lovelaceAmount: "2000000" }) -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators:
      PaymentForm
        { recipientBech32Addr:
            F.hoistFnE_ \str -> do
              let
                addrE = (Address.fromBech32 str) cardanoWasm
              case hush addrE of
                Nothing -> Left RecipientAddrNotInBech32
                Just addr -> Right addr
        , lovelaceAmount:
            F.hoistFnE_ \str -> case fromString str of
              Nothing -> Left LovelaceAmountNotAnInt
              Just n
                | n < 2000000 -> Left LovelaceAmountLowerThanMin
                | otherwise -> Right n
        }
  }

spec :: forall input m. MonadEffect m => MonadAff m => F.Spec PaymentForm () (Const Void) Action () input Payment m
spec = F.defaultSpec { render = render, handleEvent = handleEvent, handleAction = handleAction }
  where
  handleEvent = F.raiseResult

  handleAction = case _ of
    HandleSubmit event -> do
      H.liftEffect $ Event.preventDefault event
      F.handleAction handleAction handleEvent F.submit

  render { form } =
    HH.form_ -- [ HE.onSubmit submitPreventDefault ]
      [ HH.fieldset [ HP.class_ <<< ClassName $ "ba b--transparent ph0 mh0" ]
          [ HH.legend [ HP.class_ <<< ClassName $ "ph0 mh0 fw6 clip" ] [ HH.text "Submit payment" ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Recipient's Bech32 address" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent w-100"
                  , HP.value $ F.getInput _bech32Addr form
                  , HE.onValueInput $ F.setValidate _bech32Addr
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _bech32Addr form of
                      Nothing -> ""
                      Just RecipientAddrNotInBech32 -> "The provided Cardano address is not in Bech32 format."
                  ]
              ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Lovelace" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent"
                  , HP.type_ HP.InputNumber
                  , HP.value $ F.getInput _lovelaceAmount form
                  , HE.onValueInput $ F.setValidate _lovelaceAmount
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _lovelaceAmount form of
                      Nothing -> ""
                      Just LovelaceAmountNotAnInt -> "The lovelace amount is not an number."
                      Just LovelaceAmountLowerThanMin -> "The lovelace amount must be a minimum of 2 000 000 to satisfy a constraint in the Cardano blockchain."
                  ]
              ]
          ]
      , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
          [ HH.button
              [ HP.class_ <<< ClassName $ "b ph3 pv2 input-reset ba b--black bg-transparent grow pointer f6"
              -- , HP.disabled $ isSubmittingPayment || isNothing recipientBech32Addr || isNothing lovelaceAmount
              , HE.onClick \e -> F.injAction $ HandleSubmit $ toEvent e
              ]
              [ HH.text "Make payment"
              ]
          ]
      ]

  _bech32Addr = Proxy :: Proxy "recipientBech32Addr"

  _lovelaceAmount = Proxy :: Proxy "lovelaceAmount"

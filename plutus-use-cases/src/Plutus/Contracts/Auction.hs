{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
module Plutus.Contracts.Auction(
    AuctionState(..),
    AuctionInput(..),
    BuyerSchema,
    SellerSchema,
    AuctionParams(..),
    HighestBid(..),
    auctionBuyer,
    auctionSeller,
    AuctionOutput(..),
    AuctionError(..),
    ThreadToken,
    SM.getThreadToken,
    covIdx
    ) where

import Control.Lens (makeClassyPrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import GHC.Generics (Generic)
import Ledger (Ada, Address, POSIXTime, Value, toPlutusAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Interval qualified as Interval
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), StateMachine (..), StateMachineClient, ThreadToken, Void,
                                     WaitingResult (..))
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Contract.Util (loopM)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Code
import PlutusTx.Coverage
import PlutusTx.Prelude
import Prelude qualified as Haskell

-- | Definition of an auction
data AuctionParams
    = AuctionParams
        { apOwner   :: Address -- ^ Current owner of the asset. This is where the proceeds of the auction will be sent.
        , apAsset   :: Value -- ^ The asset itself. This value is going to be locked by the auction script output.
        , apEndTime :: POSIXTime -- ^ When the time window for bidding ends.
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionParams


data HighestBid =
    HighestBid
        { highestBid    :: Ada
        , highestBidder :: Address
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''HighestBid

-- | The states of the auction
data AuctionState
    = Ongoing HighestBid -- Bids can be submitted.
    | Finished HighestBid -- The auction is finished
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the auction app
data AuctionOutput =
    AuctionOutput
        { auctionState       :: Last AuctionState
        , auctionThreadToken :: Last ThreadToken
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)
        deriving (Haskell.Semigroup, Haskell.Monoid) via (GenericSemigroupMonoid AuctionOutput)

auctionStateOut :: AuctionState -> AuctionOutput
auctionStateOut s = Haskell.mempty { auctionState = Last (Just s) }

threadTokenOut :: ThreadToken -> AuctionOutput
threadTokenOut t = Haskell.mempty { auctionThreadToken = Last (Just t) }

-- | Initial 'AuctionState'. In the beginning the highest bid is 0 and the
--   highest bidder is seller of the asset. So if nobody submits
--   any bids, the seller gets the asset back after the auction has ended.
initialState :: Address -> AuctionState
initialState self = Ongoing HighestBid{highestBid = 0, highestBidder = self}

PlutusTx.unstableMakeIsData ''AuctionState

-- | Transition between auction states
data AuctionInput
    = Bid { newBid :: Ada, newBidder :: Address } -- Increase the price
    | Payout
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionInput

type AuctionMachine = StateMachine AuctionState AuctionInput

{-# INLINABLE auctionTransition #-}
-- | The transitions of the auction state machine.
auctionTransition
    :: AuctionParams
    -> State AuctionState
    -> AuctionInput
    -> Maybe (TxConstraints Void Void, State AuctionState)
auctionTransition AuctionParams{apOwner, apAsset, apEndTime} State{stateData=oldStateData, stateValue=oldStateValue} input =
    case (oldStateData, input) of

        (Ongoing HighestBid{highestBid, highestBidder}, Bid{newBid, newBidder}) | newBid > highestBid -> -- if the new bid is higher,
            let constraints = if highestBid == 0 then mempty else
                    Constraints.mustPayToAddress highestBidder (Ada.toValue highestBid) -- we pay back the previous highest bid
                    <> Constraints.mustValidateIn (Interval.to apEndTime) -- but only if we haven't gone past 'apEndTime'
                newState =
                    State
                        { stateData = Ongoing HighestBid{highestBid = newBid, highestBidder = newBidder}
                        , stateValue = Value.noAdaValue oldStateValue
                                    <> Ada.toValue (Ada.fromValue oldStateValue - highestBid)
                                    <> Ada.toValue newBid -- and lock the new bid in the script output
                        }
            in Just (constraints, newState)

        (Ongoing h@HighestBid{highestBidder, highestBid}, Payout) ->
            let constraints =
                    Constraints.mustValidateIn (Interval.from apEndTime) -- When the auction has ended,
                    <> Constraints.mustPayToAddress apOwner (Ada.toValue highestBid) -- the owner receives the payment
                    <> Constraints.mustPayToAddress highestBidder apAsset -- and the highest bidder the asset
                newState = State { stateData = Finished h, stateValue = mempty }
            in Just (constraints, newState)

        -- Any other combination of 'AuctionState' and 'AuctionInput' is disallowed.
        -- This rules out new bids that don't go over the current highest bid.
        _ -> Nothing


{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: (ThreadToken, AuctionParams) -> AuctionMachine
auctionStateMachine (threadToken, auctionParams) =
    SM.mkStateMachine (Just threadToken) (auctionTransition auctionParams) isFinal
  where
    isFinal Finished{} = True
    isFinal _          = False

{-# INLINABLE mkValidator #-}
mkValidator :: (ThreadToken, AuctionParams) -> Scripts.ValidatorType AuctionMachine
mkValidator = SM.mkValidator . auctionStateMachine

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: (ThreadToken, AuctionParams) -> Scripts.TypedValidator AuctionMachine
typedValidator = Scripts.mkTypedValidatorParam @AuctionMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | The machine client of the auction state machine. It contains the script instance
--   with the on-chain code, and the Haskell definition of the state machine for
--   off-chain use.
machineClient
    :: Scripts.TypedValidator AuctionMachine
    -> ThreadToken -- ^ Thread token of the instance
    -> AuctionParams
    -> StateMachineClient AuctionState AuctionInput
machineClient inst threadToken auctionParams =
    let machine = auctionStateMachine (threadToken, auctionParams)
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

type BuyerSchema = Endpoint "bid" Ada
type SellerSchema = EmptySchema -- Don't need any endpoints: the contract runs automatically until the auction is finished.

data AuctionLog =
    AuctionStarted AuctionParams
    | AuctionFailed SM.SMContractError
    | BidSubmitted HighestBid
    | AuctionEnded HighestBid
    | CurrentStateNotFound
    | TransitionFailed (SM.InvalidTransition AuctionState AuctionInput)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AuctionError =
    StateMachineContractError SM.SMContractError -- ^ State machine operation failed
    | AuctionContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''AuctionError

instance AsContractError AuctionError where
    _ContractError = _AuctionContractError . _ContractError

instance SM.AsSMContractError AuctionError where
    _SMContractError = _StateMachineContractError . SM._SMContractError

-- | Client code for the seller
auctionSeller :: Value -> POSIXTime -> Contract AuctionOutput SellerSchema AuctionError ()
auctionSeller value time = do
    threadToken <- SM.getThreadToken
    tell $ threadTokenOut threadToken
    self <- toPlutusAddress <$> ownAddress
    let params       = AuctionParams{apOwner = self, apAsset = value, apEndTime = time }
        inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params

    _ <- handleError
            (\e -> do { logError (AuctionFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState self) value)

    logInfo $ AuctionStarted params
    _ <- awaitTime time

    r <- SM.runStep client Payout
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i) -- TODO: Add an endpoint "retry" to the seller?
        SM.TransitionSuccess (Finished h) -> logInfo $ AuctionEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)


-- | Get the current state of the contract and log it.
currentState
    :: StateMachineClient AuctionState AuctionInput
    -> Contract AuctionOutput BuyerSchema AuctionError (Maybe HighestBid)
currentState client = do
  mOcs <- mapError StateMachineContractError (SM.getOnChainState client)
  case mOcs of
    Just (SM.getStateData -> Ongoing s, _) -> do
        tell $ auctionStateOut $ Ongoing s
        pure (Just s)
    _ -> do
        logWarn CurrentStateNotFound
        pure Nothing

{- Note [Buyer client]

In the buyer client we want to keep track of the on-chain state of the auction
to give our user a chance to react if they are outbid by somebody else.

At the same time we want to have the "bid" endpoint active for any bids of our
own, and we want to stop the client when the auction is over.

To achieve this, we have a loop where we wait for one of several events to
happen and then deal with the event. The waiting is implemented in
@waitForChange@ and the event handling is in @handleEvent@.

Updates to the user are provided via 'tell'.

-}

data BuyerEvent =
        AuctionIsOver HighestBid -- ^ The auction has ended with the highest bid
        | SubmitOwnBid Ada -- ^ We want to submit a new bid
        | OtherBid HighestBid -- ^ Another buyer submitted a higher bid
        | NoChange HighestBid -- ^ Nothing has changed

waitForChange
    :: AuctionParams
    -> StateMachineClient AuctionState AuctionInput
    -> HighestBid
    -> Contract AuctionOutput BuyerSchema AuctionError BuyerEvent
waitForChange AuctionParams{apEndTime} client lastHighestBid = do
    -- Create a Promise that waits for either an update to the state machine's
    -- on chain state, or until the end of the auction
    smUpdatePromise <- SM.waitForUpdateTimeout client (isTime apEndTime)

    let
        auctionOver = AuctionIsOver lastHighestBid Haskell.<$ isTime apEndTime
        submitOwnBid = endpoint @"bid" $ pure . SubmitOwnBid
        otherBid = do
            promiseBind
                smUpdatePromise
                $ \case
                  -- If the state machine instance ended, then the auction is over.
                  -- In this case match, 'currentState client' should always be
                  -- 'Nothing'.
                  ContractEnded {} -> maybe (AuctionIsOver lastHighestBid) OtherBid <$> currentState client
                  -- The state machine transitionned to a new state
                  Transition {} -> do
                    highestBidMaybe <- currentState client
                    case highestBidMaybe of
                      -- If there is no current state, then the auction is over.
                      Nothing -> pure $ AuctionIsOver lastHighestBid
                      -- If the next state transition contains a new state,
                      -- there is either no change to the current bid, or there
                      -- is a new bid emitted by another wallet.
                      Just highestBid -> do
                        if highestBid Haskell.== lastHighestBid
                           then pure (NoChange highestBid)
                           else pure (OtherBid highestBid)
                  _ -> pure (NoChange lastHighestBid)

    -- see note [Buyer client]
    --
    -- Also note that the order of the promises in the list is important. When
    -- the auction is over, both 'auctionOver' and 'otherBid' contracts can be
    -- fully executed. However, if 'otherBid' is at the beginning of the list,
    -- it will return "NoChange" event before returning "AuctionIsOver". Thus
    -- the auction never ends and it results in an infinite loop.
    selectList [auctionOver, submitOwnBid, otherBid]

handleEvent
    :: StateMachineClient AuctionState AuctionInput
    -> HighestBid
    -> BuyerEvent
    -> Contract AuctionOutput BuyerSchema AuctionError (Either HighestBid ())
handleEvent client lastHighestBid change =
    let continue = pure . Left
        stop     = pure (Right ())
    -- see note [Buyer client]
    in case change of
        AuctionIsOver s -> tell (auctionStateOut $ Finished s) >> stop
        SubmitOwnBid ada -> do
            logInfo @Haskell.String "Submitting bid"
            self <- toPlutusAddress <$> ownAddress
            logInfo @Haskell.String "Received address"
            r <- SM.runStep client Bid{newBid = ada, newBidder = self}
            logInfo @Haskell.String "SM: runStep done"
            case r of
                SM.TransitionFailure i -> logError (TransitionFailed i) >> continue lastHighestBid
                SM.TransitionSuccess (Ongoing newHighestBid) -> logInfo (BidSubmitted newHighestBid) >> continue newHighestBid

                -- the last case shouldn't happen because the "Bid" transition always results in the "Ongoing"
                -- but you never know :-)
                SM.TransitionSuccess (Finished newHighestBid) -> logError (AuctionEnded newHighestBid) >> stop
        OtherBid s -> do
            tell (auctionStateOut $ Ongoing s)
            continue s
        NoChange s -> continue s

auctionBuyer :: ThreadToken -> AuctionParams -> Contract AuctionOutput BuyerSchema AuctionError ()
auctionBuyer currency params = do
    let inst   = typedValidator (currency, params)
        client = machineClient inst currency params

        -- the actual loop, see note [Buyer client]
        loop   = loopM (\h -> waitForChange params client h >>= handleEvent client h)

    tell $ threadTokenOut currency
    initial <- currentState client
    case initial of
        Just s -> loop s

        -- If the state can't be found we wait for it to appear.
        Nothing -> SM.waitForUpdateUntilTime client (apEndTime params) >>= \case
            Transition _ (Ongoing s) -> loop s
            InitialState (Ongoing s) -> loop s
            _                        -> logWarn CurrentStateNotFound

covIdx :: CoverageIndex
covIdx = getCovIdx $$(PlutusTx.compile [|| mkValidator ||])


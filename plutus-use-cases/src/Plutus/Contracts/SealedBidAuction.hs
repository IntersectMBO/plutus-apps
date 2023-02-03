{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
module Plutus.Contracts.SealedBidAuction(
  AuctionParams(..)
  , BidArgs(..)
  , RevealArgs(..)
  , AuctionError(..)
  , BidderSchema
  , SellerSchema
  , startAuction
  , bid
  , reveal
  , payout
  , packInteger
  , sellerContract
  , bidderContract
  ) where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (Address, POSIXTime, toPlutusAddress)
import Ledger.Tx.Constraints (TxConstraints)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.ValidityInterval qualified as Interval
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import Plutus.Contract.Secrets
import Plutus.Contract.StateMachine (State (..), StateMachine (..), StateMachineClient, Void)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell

{- Note [Sealed bid auction disclaimer]
   This file implements a sealed bid auction using `SecretArgument`s. In the bidding
   phase of the contract sealed bids appear hashed on the blockchain and hashed
   bids are "claimed" by the participants in the second phase of the auction.

   Because bids are integer lovelace values there is the faint possibility of a
   brute-force attack or lookup table attack on the bids in the bidding phase. An
   implementation intended to be deployed in the real world may consider adding a
   salt to the secret bids or using some other more sophisticated mechanism to
   avoid this attack. In other words, please don't blindly deploy this code
   without understanding the possible attack scenarios.
 -}

newtype BidArgs = BidArgs { secretBid :: SecretArgument Integer }
          deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

newtype RevealArgs = RevealArgs { publicBid :: Integer }
          deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

type BidderSchema = Endpoint "bid" BidArgs
                    .\/ Endpoint "reveal" RevealArgs
                    .\/ Endpoint "payout" ()
type SellerSchema = Endpoint "payout" ()

-- | Definition of an auction
data AuctionParams
    = AuctionParams
        { apOwner      :: Address -- ^ Current owner of the asset. This is where the proceeds of the auction will be sent.
        , apAsset      :: Value -- ^ The asset itself. This value is going to be locked by the auction script output.
        , apEndTime    :: POSIXTime -- ^ When the time window for bidding ends.
        , apPayoutTime :: POSIXTime -- ^ When the time window for revealing your bid ends.
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionParams


data SealedBid =
    SealedBid
        { sealedBid       :: BuiltinByteString
        , sealedBidBidder :: Address
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SealedBid

instance Eq SealedBid where
  (SealedBid bid bidder) == (SealedBid bid' bidder') = bid == bid' && bidder == bidder'

data RevealedBid =
    RevealedBid
        { revealedBid       :: Integer
        , revealedBidBidder :: Address
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''RevealedBid

-- | The states of the auction
data AuctionState
    = Ongoing [SealedBid] -- Bids can be submitted.
    | AwaitingPayout RevealedBid [SealedBid] -- The bidding is finished and we are awaiting payout
    | Finished
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionState

-- | Initial 'AuctionState'. In the beginning there are no bids.
initialState :: AuctionState
initialState = Ongoing []

-- | Transition between auction states
data AuctionInput
    = PlaceBid SealedBid -- Register a sealed bid
    | RevealBid RevealedBid -- Reveal a bid
    | Payout
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionInput

type AuctionMachine = StateMachine AuctionState AuctionInput

{-# INLINABLE packInteger #-}
-- | Pack an integer into a byte string with a leading
-- sign byte in little-endian order
packInteger :: Integer -> BuiltinByteString
packInteger k = if k < 0 then consByteString 1 (go (negate k) emptyByteString) else consByteString 0 (go k emptyByteString)
  where
    go n s
      | n == 0            = s
      | otherwise         = go (n `divide` 256) (consByteString (n `modulo` 256) s)

{-# INLINABLE hashInteger #-}
hashInteger :: Integer -> BuiltinByteString
hashInteger = sha2_256 . packInteger

{-# INLINABLE hashSecretInteger #-}
hashSecretInteger :: Secret Integer -> BuiltinByteString
hashSecretInteger = escape_sha2_256 . fmap packInteger

{-# INLINABLE sealBid #-}
sealBid :: RevealedBid -> SealedBid
sealBid RevealedBid{revealedBid, revealedBidBidder} = SealedBid (hashInteger revealedBid) revealedBidBidder

{-# INLINABLE valueOfBid #-}
valueOfBid :: RevealedBid -> Value
valueOfBid = Ada.lovelaceValueOf . revealedBid

{-# INLINABLE auctionTransition #-}
-- | The transitions of the auction state machine.
auctionTransition
  :: AuctionParams
  -> State AuctionState
  -> AuctionInput
  -> Maybe (TxConstraints Void Void, State AuctionState)
auctionTransition AuctionParams{apOwner, apAsset, apEndTime, apPayoutTime} State{stateData=oldStateData, stateValue=oldStateValue} input =
  case (oldStateData, input) of
    -- A new bid is placed, a bidder is only allowed to bid once
    (Ongoing bids, PlaceBid bid)
      | sealedBidBidder bid `notElem` map sealedBidBidder bids ->
        let validityTimeRange = Interval.lessThan $ apEndTime - 1
            constraints = Constraints.mustValidateInTimeRange validityTimeRange
            newState =
              State
                  { stateData  = Ongoing (bid:bids)
                  , stateValue = oldStateValue
                  }
        in Just (constraints, newState)

    -- The first bid is revealed
    (Ongoing bids, RevealBid bid)
      | sealBid bid `elem` bids ->
        let validityTimeRange = Interval.interval apEndTime apPayoutTime
            constraints = Constraints.mustValidateInTimeRange validityTimeRange
            newState =
              State
                  { stateData  = AwaitingPayout bid (filter (/= sealBid bid) bids)
                  , stateValue = oldStateValue <> valueOfBid bid
                  }
        in Just (constraints, newState)

    -- Nobody has revealed their bid and the deadline has arrived
    (Ongoing _, Payout) ->
      let constraints = Constraints.mustValidateInTimeRange (Interval.from apPayoutTime)
                      <> Constraints.mustPayToAddress apOwner apAsset
          newState =
            State
                { stateData  = Finished
                , stateValue = mempty
                }
      in Just (constraints, newState)

    -- We are waiting for the payout deadine and a bid is revealed that is higher
    -- than the current maximum bid
    (AwaitingPayout highestBid sealedBids, RevealBid bid)
      | revealedBid bid > revealedBid highestBid
        && sealBid bid `elem` sealedBids ->
        let validityTimeRange = Interval.lessThan $ 1 + apPayoutTime
            constraints = Constraints.mustValidateInTimeRange validityTimeRange
                        <> Constraints.mustPayToAddress (revealedBidBidder highestBid) (valueOfBid highestBid)
            newState =
              State
                  { stateData = AwaitingPayout bid (filter (/= sealBid bid) sealedBids)
                  , stateValue = Value.noAdaValue oldStateValue <> Ada.toValue (Ada.fromValue oldStateValue - Ada.lovelaceOf (revealedBid highestBid)) <> valueOfBid bid
                  }
        in Just (constraints, newState)

    -- At least one bid has been revealed and the payout is triggered
    (AwaitingPayout highestBid _, Payout) ->
      let constraints = Constraints.mustValidateInTimeRange (Interval.from apPayoutTime)
                      <> Constraints.mustPayToAddress apOwner (valueOfBid highestBid)
                      <> Constraints.mustPayToAddress (revealedBidBidder highestBid) apAsset
          newState =
            State
                { stateData  = Finished
                , stateValue = mempty
                }
      in Just (constraints, newState)

    -- No other combination of inputs makes sense
    _ -> Nothing

{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: AuctionParams -> AuctionMachine
auctionStateMachine auctionParams =
    SM.mkStateMachine Nothing (auctionTransition auctionParams) isFinal
  where
    isFinal Finished = True
    isFinal _        = False

{-# INLINABLE mkValidator #-}
mkValidator :: AuctionParams -> V2.ValidatorType AuctionMachine
mkValidator = SM.mkValidator . auctionStateMachine

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: AuctionParams -> V2.TypedValidator AuctionMachine
typedValidator = V2.mkTypedValidatorParam @AuctionMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

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

client :: AuctionParams -> StateMachineClient AuctionState AuctionInput
client auctionParams =
    let machine = auctionStateMachine auctionParams
        inst    = typedValidator auctionParams
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

startAuction :: Value -> POSIXTime -> POSIXTime -> Contract () SellerSchema AuctionError ()
startAuction asset endTime payoutTime = do
    self <- toPlutusAddress <$> ownAddress
    let params = AuctionParams self asset endTime payoutTime
    void $ SM.runInitialise (client params) (Ongoing []) (apAsset params)

bid :: AuctionParams -> Promise () BidderSchema AuctionError ()
bid params = endpoint @"bid" $ \ BidArgs{secretBid} -> do
    self <- toPlutusAddress <$> ownAddress
    let sBid = extractSecret secretBid
    void $ SM.runStep (client params) (PlaceBid $ SealedBid (hashSecretInteger sBid) self)

reveal :: AuctionParams -> Promise () BidderSchema AuctionError ()
reveal params = endpoint @"reveal" $ \ RevealArgs{publicBid} -> do
    self <- toPlutusAddress <$> ownAddress
    void $ SM.runStep (client params) (RevealBid $ RevealedBid publicBid self)

payout :: (HasEndpoint "payout" () s) => AuctionParams -> Promise () s AuctionError ()
payout params = endpoint @"payout" $ \() -> do
    void $ SM.runStep (client params) Payout

-- | Top-level contract for seller
sellerContract :: AuctionParams -> Contract () SellerSchema AuctionError ()
sellerContract params@AuctionParams{..} = startAuction apAsset apEndTime apPayoutTime >> awaitPromise (payout params)

-- | Top-level contract for buyer
bidderContract :: AuctionParams -> Contract () BidderSchema AuctionError ()
bidderContract params = selectList [bid params, reveal params, payout params] >> bidderContract params

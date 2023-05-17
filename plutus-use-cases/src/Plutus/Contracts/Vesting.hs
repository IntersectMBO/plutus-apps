{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
module Plutus.Contracts.Vesting (
    -- $vesting
    VestingParams(..),
    VestingSchema,
    VestingTranche(..),
    VestingError(..),
    AsVestingError(..),
    totalAmount,
    vestingContract,
    validate,
    vestingScript
    ) where

import Control.Lens
import Control.Monad (void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Prelude (Semigroup (..))

import Cardano.Node.Emulator.Internal.Node (pNetworkId, testnet)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, POSIXTime, POSIXTimeRange, PaymentPubKeyHash (unPaymentPubKeyHash),
               decoratedTxOutPlutusValue)
import Ledger.Interval qualified as Interval
import Ledger.Tx.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScriptWithDatumInTx, mustValidateInTimeRange)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.ValidityInterval qualified as ValidityInterval
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V2.Ledger.Api (ScriptContext (..), TxInfo (..), Validator)
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold)
import Prelude qualified as Haskell

{- |
    A simple vesting scheme. Money is locked by a contract and may only be
    retrieved after some time has passed.

    This is our first example of a contract that covers multiple transactions,
    with a contract state that changes over time.

    In our vesting scheme the money will be released in two _tranches_ (parts):
    A smaller part will be available after an initial number of time has
    passed, and the entire amount will be released at the end. The owner of the
    vesting scheme does not have to take out all the money at once: They can
    take out any amount up to the total that has been released so far. The
    remaining funds stay locked and can be retrieved later.

    Let's start with the data types.

-}

type VestingSchema =
        Endpoint "vest funds" ()
        .\/ Endpoint "retrieve funds" Value

data Vesting

instance ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = ()

-- | Tranche of a vesting scheme.
data VestingTranche = VestingTranche {
    vestingTrancheDate   :: POSIXTime,
    vestingTrancheAmount :: Value
    } deriving Generic

PlutusTx.makeLift ''VestingTranche

-- | A vesting scheme consisting of two tranches. Each tranche defines a date
--   (POSIX time) after which an additional amount can be spent.
data VestingParams = VestingParams {
    vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner    :: PaymentPubKeyHash
    } deriving Generic

PlutusTx.makeLift ''VestingParams

{-# INLINABLE totalAmount #-}
-- | The total amount vested
totalAmount :: VestingParams -> Value
totalAmount VestingParams{vestingTranche1,vestingTranche2} =
    vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2

{-# INLINABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given time range.
availableFrom :: VestingTranche -> POSIXTimeRange -> Value
availableFrom (VestingTranche d v) range =
    -- The valid range is an open-ended range starting from the tranche vesting date
    let validRange = Interval.from d
    -- If the valid range completely contains the argument range (meaning in particular
    -- that the start time of the argument range is after the tranche vesting date), then
    -- the money in the tranche is available, otherwise nothing is available.
    in if validRange `Interval.contains` range then v else zero

availableAt :: VestingParams -> POSIXTime -> Value
availableAt VestingParams{vestingTranche1, vestingTranche2} time =
    let f VestingTranche{vestingTrancheDate, vestingTrancheAmount} =
            if time >= vestingTrancheDate then vestingTrancheAmount else mempty
    in foldMap f [vestingTranche1, vestingTranche2]

{-# INLINABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: VestingTranche -> POSIXTimeRange -> Value
remainingFrom t@VestingTranche{vestingTrancheAmount} range =
    vestingTrancheAmount - availableFrom t range

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> () -> V2.ScriptContext -> Bool
validate VestingParams{vestingTranche1, vestingTranche2, vestingOwner} () () ctx@V2.ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    let
        remainingActual  = V2.valueLockedBy txInfo (V2.ownHash ctx)

        remainingExpected =
            remainingFrom vestingTranche1 txInfoValidRange
            + remainingFrom vestingTranche2 txInfoValidRange

    in remainingActual `Value.geq` remainingExpected
            -- The policy encoded in this contract
            -- is "vestingOwner can do with the funds what they want" (as opposed
            -- to "the funds must be paid to vestingOwner"). This is enforcey by
            -- the following condition:
            && V2.txSignedBy txInfo (unPaymentPubKeyHash vestingOwner)
            -- That way the recipient of the funds can pay them to whatever address they
            -- please, potentially saving one transaction.

vestingScript :: VestingParams -> Validator
vestingScript = Scripts.validatorScript . typedValidator

typedValidator :: VestingParams -> V2.TypedValidator Vesting
typedValidator = V2.mkTypedValidatorParam @Vesting
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

contractAddress :: VestingParams -> CardanoAddress
contractAddress = Scripts.validatorCardanoAddress testnet . typedValidator

data VestingError =
    VContractError ContractError
    | InsufficientFundsError Value Value Value
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''VestingError

instance AsContractError VestingError where
    _ContractError = _VContractError

vestingContract :: VestingParams -> Contract () VestingSchema VestingError ()
vestingContract vesting = selectList [vest, retrieve]
  where
    vest = endpoint @"vest funds" $ \() -> vestFundsC vesting
    retrieve = endpoint @"retrieve funds" $ \payment -> do
        liveness <- retrieveFundsC vesting payment
        case liveness of
            Alive -> awaitPromise retrieve
            Dead  -> pure ()

payIntoContract :: Value -> TxConstraints () ()
payIntoContract = mustPayToTheScriptWithDatumInTx ()

vestFundsC
    :: ( AsVestingError e
       )
    => VestingParams
    -> Contract w s e ()
vestFundsC vesting = mapError (review _VestingError) $ do
    let tx = payIntoContract (totalAmount vesting)
    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator vesting) tx
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx

data Liveness = Alive | Dead

retrieveFundsC
    :: ( AsVestingError e
       )
    => VestingParams
    -> Value
    -> Contract w s e Liveness
retrieveFundsC vesting payment = mapError (review _VestingError) $ do
    networkId <- pNetworkId <$> getParams
    let inst = typedValidator vesting
        addr = Scripts.validatorCardanoAddress networkId inst
    now <- fst <$> currentNodeClientTimeRange
    unspentOutputs <- utxosAt addr
    let
        currentlyLocked = foldMap decoratedTxOutPlutusValue (Map.elems unspentOutputs)
        remainingValue = currentlyLocked - payment
        mustRemainLocked = totalAmount vesting - availableAt vesting now
        maxPayment = currentlyLocked - mustRemainLocked

    when (remainingValue `Value.lt` mustRemainLocked)
        $ throwError
        $ InsufficientFundsError payment maxPayment mustRemainLocked

    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead
        remainingOutputs = case liveness of
                            Alive -> payIntoContract remainingValue
                            Dead  -> mempty
        tx = Constraints.spendUtxosFromTheScript unspentOutputs ()
                <> remainingOutputs
                <> mustValidateInTimeRange (ValidityInterval.from now)
                <> mustBeSignedBy (vestingOwner vesting)
                -- we don't need to add a pubkey output for 'vestingOwner' here
                -- because this will be done by the wallet when it balances the
                -- transaction.
    mkTxConstraints (Constraints.typedValidatorLookups inst
                  <> Constraints.unspentOutputs unspentOutputs) tx
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx
    return liveness

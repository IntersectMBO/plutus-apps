module Playground.Usecases where

vesting :: String
vesting =
  """-- Vesting scheme as a PLC contract
import Control.Lens (view)
import Control.Monad (void, when)
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Text qualified as T

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Interval qualified as Interval
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.Contract.Typed.Tx qualified as Typed
import Plutus.V1.Ledger.Api (Address, POSIXTime, POSIXTimeRange, Validator)
import Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInfo (..))
import Plutus.V1.Ledger.Contexts qualified as Validation
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold)
import Prelude as Haskell (Semigroup (..), show)

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
availableAt VestingParams{vestingTranche1, vestingTranche2} sl =
    let f VestingTranche{vestingTrancheDate, vestingTrancheAmount} =
            if sl >= vestingTrancheDate then vestingTrancheAmount else mempty
    in foldMap f [vestingTranche1, vestingTranche2]

{-# INLINABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: VestingTranche -> POSIXTimeRange -> Value
remainingFrom t@VestingTranche{vestingTrancheAmount} range =
    vestingTrancheAmount - availableFrom t range

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> () -> ScriptContext -> Bool
validate VestingParams{vestingTranche1, vestingTranche2, vestingOwner} () () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    let
        remainingActual  = Validation.valueLockedBy txInfo (Validation.ownHash ctx)

        remainingExpected =
            remainingFrom vestingTranche1 txInfoValidRange
            + remainingFrom vestingTranche2 txInfoValidRange

    in remainingActual `Value.geq` remainingExpected
            -- The policy encoded in this contract
            -- is "vestingOwner can do with the funds what they want" (as opposed
            -- to "the funds must be paid to vestingOwner"). This is enforcey by
            -- the following condition:
            && Validation.txSignedBy txInfo (unPaymentPubKeyHash vestingOwner)
            -- That way the recipient of the funds can pay them to whatever address they
            -- please, potentially saving one transaction.

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = ()

vestingScript :: VestingParams -> Validator
vestingScript = Scripts.validatorScript . typedValidator

typedValidator :: VestingParams -> Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidatorParam @Vesting
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

contractAddress :: VestingParams -> Address
contractAddress = Scripts.validatorAddress . typedValidator

vestingContract :: VestingParams -> Contract () VestingSchema T.Text ()
vestingContract vesting = selectList [vest, retrieve]
  where
    vest = endpoint @"vest funds" $ \_ -> vestFundsC vesting
    retrieve = endpoint @"retrieve funds" $ \payment -> do
        liveness <- retrieveFundsC vesting payment
        case liveness of
            Alive -> awaitPromise retrieve
            Dead  -> pure ()

payIntoContract :: Value -> TxConstraints () ()
payIntoContract = mustPayToTheScript ()

vestFundsC
    :: VestingParams
    -> Contract () s T.Text ()
vestFundsC vesting = do
    let txn = payIntoContract (totalAmount vesting)
    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator vesting) txn
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx

data Liveness = Alive | Dead

retrieveFundsC
    :: VestingParams
    -> Value
    -> Contract () s T.Text Liveness
retrieveFundsC vesting payment = do
    let inst = typedValidator vesting
        addr = Scripts.validatorAddress inst
    nextTime <- awaitTime 0
    unspentOutputs <- utxosAt addr
    let
        currentlyLocked = foldMap (view Tx.ciTxOutValue) (Map.elems unspentOutputs)
        remainingValue = currentlyLocked - payment
        mustRemainLocked = totalAmount vesting - availableAt vesting nextTime
        maxPayment = currentlyLocked - mustRemainLocked

    when (remainingValue `Value.lt` mustRemainLocked)
        $ throwError
        $ T.unwords
            [ "Cannot take out"
            , T.pack (show payment) `T.append` "."
            , "The maximum is"
            , T.pack (show maxPayment) `T.append` "."
            , "At least"
            , T.pack (show mustRemainLocked)
            , "must remain locked by the script."
            ]

    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead
        remainingOutputs = case liveness of
                            Alive -> payIntoContract remainingValue
                            Dead  -> mempty
        txn = Typed.collectFromScript unspentOutputs ()
                <> remainingOutputs
                <> mustValidateIn (Interval.from nextTime)
                <> mustBeSignedBy (vestingOwner vesting)
                -- we don't need to add a pubkey output for 'vestingOwner' here
                -- because this will be done by the wallet when it balances the
                -- transaction.
    mkTxConstraints (Constraints.typedValidatorLookups inst
                  <> Constraints.unspentOutputs unspentOutputs) txn
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx
    return liveness

endpoints :: Contract () VestingSchema T.Text ()
endpoints = vestingContract vestingParams
  where
    vestingOwner = mockWalletPaymentPubKeyHash w1
    vestingParams =
        VestingParams {vestingTranche1, vestingTranche2, vestingOwner}
    vestingTranche1 =
        VestingTranche
            {vestingTrancheDate = TimeSlot.scSlotZeroTime def + 20000, vestingTrancheAmount = Ada.lovelaceValueOf 50_000_000}
    vestingTranche2 =
        VestingTranche
            {vestingTrancheDate = TimeSlot.scSlotZeroTime def + 40000, vestingTrancheAmount = Ada.lovelaceValueOf 30_000_000}

mkSchemaDefinitions ''VestingSchema

$(mkKnownCurrencies [])
"""

game :: String
game =
  """-- A game with two players. Player 1 thinks of a secret word
-- and uses its hash, and the game validator script, to lock
-- some funds (the prize) in a pay-to-script transaction output.
-- Player 2 guesses the word by attempting to spend the transaction
-- output. If the guess is correct, the validator script releases the funds.
-- If it isn't, the funds stay locked.
import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import Plutus.V1.Ledger.Api (Address, Datum (Datum), ScriptContext, Validator, Value)
import PlutusTx qualified
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell

------------------------------------------------------------

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @HashedString @ClearString

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: Haskell.String
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams secret amt) -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show amt <> " to the script"
    let tx         = Constraints.mustPayToTheScript (hashString secret) amt
    void (submitTxConstraints gameInstance tx)

-- | The "guess" contract endpoint. See note [Contract endpoints]
guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" @GuessParams $ \(GuessParams theGuess) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)

    let redeemer = clearString theGuess
        tx       = collectFromScript utxos redeemer

    -- Log a message saying if the secret word was correctly guessed
    let hashedSecretWord = findSecretWordValue utxos
        isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True
    if isCorrectSecretWord
        then logWarn @Haskell.String "Correct secret word! Submitting the transaction"
        else logWarn @Haskell.String "Incorrect secret word, but still submiting the transaction"

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to guess the secret word"
    void (submitTxConstraintsSpending gameInstance utxos tx)

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: Map TxOutRef ChainIndexTxOut -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: ChainIndexTxOut -> Maybe HashedString
secretWordValue o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

game :: AsContractError e => Contract () GameSchema e ()
game = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [lock, guess]

{- Note [Contract endpoints]

A contract endpoint is a function that uses the wallet API to interact with the
blockchain. We can look at contract endpoints from two different points of view.

1. Contract users

Contract endpoints are the visible interface of the contract. They provide a
UI (HTML form) for entering the parameters of the actions we may take as part
of the contract.

2. Contract authors

As contract authors we define endpoints as functions that return a value of
type 'MockWallet ()'. This type indicates that the function uses the wallet API
to produce and spend transaction outputs on the blockchain.

Endpoints can have any number of parameters: 'lock' has two
parameters, 'guess' has one and 'startGame' has none. For each endpoint we
include a call to 'mkFunction' at the end of the contract definition. This
causes the Haskell compiler to generate a schema for the endpoint. The Plutus
Playground then uses this schema to present an HTML form to the user where the
parameters can be entered.

-}

endpoints :: AsContractError e => Contract () GameSchema e ()
endpoints = game

mkSchemaDefinitions ''GameSchema

$(mkKnownCurrencies [])
"""

crowdFunding :: String
crowdFunding =
  """-- Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction.
--
-- Note [Transactions in the crowdfunding campaign] explains the structure of
-- this contract on the blockchain.

import Control.Applicative (Applicative (pure))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Text (Text)
import Ledger (POSIXTime, POSIXTimeRange, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (..), TxInfo (..),
               getCardanoTxId)
import Ledger qualified
import Ledger.Interval qualified as Interval
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Ledger.Value (Value)
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Constraints qualified as Constraints
import Plutus.Contract.Typed.Tx qualified as Typed
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (Validator)
import Plutus.V1.Ledger.Contexts qualified as V
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..))
import Prelude (Semigroup (..))
import Prelude qualified as Haskell
import Wallet.Emulator qualified as Emulator

-- | A crowdfunding campaign.
data Campaign = Campaign
    { campaignDeadline           :: POSIXTime
    -- ^ The date by which the campaign funds can be contributed.
    , campaignCollectionDeadline :: POSIXTime
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignOwner              :: PaymentPubKeyHash
    -- ^ Public key of the campaign owner. This key is entitled to retrieve the
    --   funds if the campaign is successful.
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

-- | Action that can be taken by the participants in this contract. A value of
--   `CampaignAction` is provided as the redeemer. The validator script then
--   checks if the conditions for performing this action are met.
--
data CampaignAction = Collect | Refund

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
        Endpoint "schedule collection" ()
        .\/ Endpoint "contribute" Contribution

newtype Contribution = Contribution
        { contribValue :: Value
        -- ^ how much to contribute
        } deriving stock (Haskell.Eq, Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

-- | Construct a 'Campaign' value from the campaign parameters,
--   using the wallet's public key.
mkCampaign :: POSIXTime -> POSIXTime -> Wallet -> Campaign
mkCampaign ddl collectionDdl ownerWallet =
    Campaign
        { campaignDeadline = ddl
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = Emulator.mockWalletPaymentPubKeyHash ownerWallet
        }

-- | The 'POSIXTimeRange' during which the funds can be collected
collectionRange :: Campaign -> POSIXTimeRange
collectionRange cmp =
    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp - 1)

-- | The 'POSIXTimeRange' during which a refund may be claimed
refundRange :: Campaign -> POSIXTimeRange
refundRange cmp =
    Interval.from (campaignCollectionDeadline cmp)

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PaymentPubKeyHash

typedValidator :: Campaign -> Scripts.TypedValidator Crowdfunding
typedValidator = Scripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PaymentPubKeyHash -> TxInfo -> Bool
validRefund campaign contributor txinfo =
    -- Check that the transaction falls in the refund range of the campaign
    (refundRange campaign `Interval.contains` txInfoValidRange txinfo)
    -- Check that the transaction is signed by the contributor
    && (txinfo `V.txSignedBy` unPaymentPubKeyHash contributor)

validCollection :: Campaign -> TxInfo -> Bool
validCollection campaign txinfo =
    -- Check that the transaction falls in the collection range of the campaign
    (collectionRange campaign `Interval.contains` txInfoValidRange txinfo)
    -- Check that the transaction is signed by the campaign owner
    && (txinfo `V.txSignedBy` unPaymentPubKeyHash (campaignOwner campaign))

-- | The validator script is of type 'CrowdfundingValidator', and is
-- additionally parameterized by a 'Campaign' definition. This argument is
-- provided by the Plutus client, using 'PlutusTx.applyCode'.
-- As a result, the 'Campaign' definition is part of the script address,
-- and different campaigns have different addresses.
mkValidator :: Campaign -> PaymentPubKeyHash -> CampaignAction -> ScriptContext -> Bool
mkValidator c con act p = case act of
    -- the "refund" branch
    Refund  -> validRefund c con (scriptContextTxInfo p)
    -- the "collection" branch
    Collect -> validCollection c (scriptContextTxInfo p)

-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
--
contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . typedValidator

-- | The address of a [[Campaign]]
campaignAddress :: Campaign -> ValidatorHash
campaignAddress = Scripts.validatorHash . contributionScript

-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ()
crowdfunding c = selectList [contribute c, scheduleCollection c]

-- | A sample campaign
theCampaign :: POSIXTime -> Campaign
theCampaign startTime = Campaign
    { campaignDeadline = startTime + 40000
    , campaignCollectionDeadline = startTime + 60000
    , campaignOwner = Emulator.mockWalletPaymentPubKeyHash (Emulator.knownWallet 1)
    }

-- | The "contribute" branch of the contract for a specific 'Campaign'. Exposes
--   an endpoint that allows the user to enter their public key and the
--   contribution. Then waits until the campaign is over, and collects the
--   refund if the funding was not collected.
contribute :: AsContractError e => Campaign -> Promise () CrowdfundingSchema e ()
contribute cmp = endpoint @"contribute" $ \Contribution{contribValue} -> do
    contributor <- ownPaymentPubKeyHash
    let inst = typedValidator cmp
        tx = Constraints.mustPayToTheScript contributor contribValue
                <> Constraints.mustValidateIn (Interval.to (campaignDeadline cmp))
    txid <- fmap getCardanoTxId $ mkTxConstraints (Constraints.typedValidatorLookups inst) tx
        >>= adjustUnbalancedTx >>= submitUnbalancedTx

    utxo <- watchAddressUntilTime (Scripts.validatorAddress inst) (campaignCollectionDeadline cmp)

    -- 'utxo' is the set of unspent outputs at the campaign address at the
    -- collection deadline. If 'utxo' still contains our own contribution
    -- then we can claim a refund.

    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo Refund
                <> Constraints.mustValidateIn (refundRange cmp)
                <> Constraints.mustBeSignedBy contributor
    if Constraints.modifiesUtxoSet tx'
    then do
        logInfo @Text "Claiming refund"
        void $ mkTxConstraints (Constraints.typedValidatorLookups inst
                             <> Constraints.unspentOutputs utxo) tx'
            >>= adjustUnbalancedTx >>= submitUnbalancedTx
    else pure ()

-- | The campaign owner's branch of the contract for a given 'Campaign'. It
--   watches the campaign address for contributions and collects them if
--   the funding goal was reached in time.
scheduleCollection :: AsContractError e => Campaign -> Promise () CrowdfundingSchema e ()
scheduleCollection cmp =
    -- Expose an endpoint that lets the user fire the starting gun on the
    -- campaign. (This endpoint isn't technically necessary, we could just
    -- run the 'trg' action right away)
    endpoint @"schedule collection" $ \() -> do
        let inst = typedValidator cmp

        _ <- awaitTime $ campaignDeadline cmp
        unspentOutputs <- utxosAt (Scripts.validatorAddress inst)

        let tx = Typed.collectFromScript unspentOutputs Collect
                <> Constraints.mustValidateIn (collectionRange cmp)
        void $ submitTxConstraintsSpending inst unspentOutputs tx

{- note [Transactions in the crowdfunding campaign]

Assume there is a campaign `c :: Campaign` with two contributors
(identified by public key `pc_1` and `pc_2`) and one campaign owner (pco).
Each contributor creates a transaction, `t_1` and `t_2`, whose outputs are
locked by the scripts `contributionScript c pc_1` and `contributionScript
c pc_1` respectively.

There are two outcomes for the campaign.

1. Campaign owner collects the funds from both contributors. In this case
   the owner creates a single transaction with two inputs, referring to
   `t_1` and `t_2`. Each input contains the script `contributionScript c`
   specialised to a contributor. The redeemer script of this transaction
   contains the value `Collect`, prompting the validator script to check the
   branch for `Collect`.

2. Refund. In this case each contributor creates a transaction with a
   single input claiming back their part of the funds. This case is
   covered by the `Refund` branch, and its redeemer script is the
   `Refund` action.

In both cases, the validator script is run twice. In the first case
there is a single transaction consuming both inputs. In the second case there
are two different transactions that may happen at different times.

-}

{- note [PendingTx]

This part of the API (the PendingTx argument) is experimental and subject
to change.

-}

endpoints :: AsContractError e => Contract () CrowdfundingSchema e ()
endpoints = crowdfunding (theCampaign $ TimeSlot.scSlotZeroTime def)

mkSchemaDefinitions ''CrowdfundingSchema

$(mkKnownCurrencies [])
"""

errorHandling :: String
errorHandling =
  """import Control.Lens (makeClassyPrisms, prism', review)
import Control.Monad (void)
import Control.Monad.Error.Lens (catching, throwing, throwing_)
import Data.Text (Text)
import Data.Text qualified as T

import Data.Default (Default (def))
import Ledger.TimeSlot (SlotConfig)
import Ledger.TimeSlot qualified as TimeSlot
import Playground.Contract
import Plutus.Contract (AsContractError (_ContractError), ContractError, awaitTime, logInfo, mapError, selectList)
import Prelude (Maybe (..), const, show, ($), (+), (.), (<>))

-- Demonstrates how to deal with errors in Plutus contracts. We define a custom
-- error type 'MyError' with three constructors and use
-- 'Control.Lens.makeClassyPrisms' to generate the 'AsMyError' class. We can
-- then use 'MyError' in our contracts with the combinators from
-- 'Control.Monad.Error.Lens'. The unit tests in 'Spec.ErrorHandling' show how
-- to write tests for error conditions.

type Schema =
    Endpoint "throwError" Text
     .\/ Endpoint "catchError" Text
     .\/ Endpoint "catchContractError" ()

-- | 'MyError' has a constructor for each type of error that our contract
 --   can throw. The 'AContractError' constructor wraps a 'ContractError'.
data MyError =
    Error1 Text
    | Error2
    | AContractError ContractError
    deriving Show

makeClassyPrisms ''MyError

instance AsContractError MyError where
    -- 'ContractError' is another error type. It is defined in
    -- 'Plutus.Contract.Request'. By making 'MyError' an
    -- instance of 'AsContractError' we can handle 'ContractError's
    -- thrown by other contracts in our code (see 'catchContractError')
    _ContractError = _AContractError

instance AsMyError Text where
    _MyError = prism' (T.pack . show) (const Nothing)

-- | Throw an 'Error1', using 'Control.Monad.Error.Lens.throwing' and the
--   prism generated by 'makeClassyPrisms'
throw :: AsMyError e => Text -> Contract () s e ()
throw e = do
    logInfo @Text $  "throwError: " <> e
    throwing _Error1 e

-- | Handle the error from 'throw' using 'Control.Monad.Error.Lens.catching'
throwAndCatch :: AsMyError e => Text -> Contract () s e ()
throwAndCatch e =
    let handleError1 :: Text -> Contract () s e ()
        handleError1 t = logInfo $ "handleError: " <> t
     in catching _Error1 (throw e) handleError1

-- | Handle an error from 'awaitTime by wrapping it in the 'AContractError'
--   constructor
catchContractError :: (AsMyError e) => SlotConfig -> Contract () s e ()
catchContractError slotCfg =
    catching _AContractError
        (void $ mapError (review _AContractError) $
            awaitTime $ TimeSlot.scSlotZeroTime slotCfg + 10000)
        (\_ -> throwing_ _Error2)

contract
    :: ( AsMyError e
       , AsContractError e
       )
    => SlotConfig
    -> Contract () Schema e ()
contract slotCfg = selectList
    [ endpoint @"throwError" throw
    , endpoint @"catchError" throwAndCatch
    , endpoint @"catchContractError" $ const (catchContractError slotCfg)
    ]

endpoints :: (AsMyError e, AsContractError e) => Contract () Schema e ()
endpoints = contract def

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
"""

starter :: String
starter =
  """-- This is a starter contract, based on the Game contract,
-- containing the bare minimum required scaffolding.
--
-- What you should change to something more suitable for
-- your use case:
--   * The MyDatum type
--   * The MyRedeemer type
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * publish
--   * redeem

import Control.Monad (void)
import Ledger (Address)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Playground.Contract
import Plutus.Contract
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))

-- | These are the data script and redeemer types. We are using an integer
--   value for both, but you should define your own types.
newtype MyDatum = MyDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyDatum

newtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyRedeemer

-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
validateSpend _myDataValue _myRedeemerValue _ = error () -- Please provide an implementation.

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

data Starter
instance Scripts.ValidatorTypes Starter where
    type instance RedeemerType Starter = MyRedeemer
    type instance DatumType Starter = MyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: Scripts.TypedValidator Starter
starterInstance = Scripts.mkTypedValidator @Starter
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @MyDatum @MyRedeemer

-- | The schema of the contract, with two endpoints.
type Schema =
        Endpoint "publish" (Integer, Value)
        .\/ Endpoint "redeem" Integer

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [publish, redeem]

-- | The "publish" contract endpoint.
publish :: AsContractError e => Promise () Schema e ()
publish = endpoint @"publish" $ \(i, lockedFunds) -> do
    let tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds
    void $ submitTxConstraints starterInstance tx

-- | The "redeem" contract endpoint.
redeem :: AsContractError e => Promise () Schema e ()
redeem = endpoint @"redeem" $ \myRedeemerValue -> do
    unspentOutputs <- utxosAt contractAddress
    let redeemer = MyRedeemer myRedeemerValue
        tx       = collectFromScript unspentOutputs redeemer
    void $ submitTxConstraintsSpending starterInstance unspentOutputs tx

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
"""

contractDemos :: String
contractDemos =
  """[
    {
        "contractDemoName": "Hello, world",
        "contractDemoContext": {
            "warnings": [],
            "result": {
                "functionSchema": [
                    {
                        "argument": {
                            "tag": "FormSchemaUnit"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "dummy"
                        }
                    }
                ],
                "knownCurrencies": [
                    {
                        "hash": "",
                        "friendlyName": "Ada",
                        "knownTokens": [
                            {
                                "unTokenName": ""
                            }
                        ]
                    }
                ]
            }
        },
        "contractDemoEditorContents": "import Data.Text qualified as T\nimport Playground.Contract\nimport Plutus.Contract\nimport PlutusTx.Prelude\nimport Prelude qualified as Haskell\n\n-- | A 'Contract' that logs a message.\nhello :: Contract () EmptySchema T.Text ()\nhello = logInfo @Haskell.String \"Hello, world\"\n\nendpoints :: Contract () EmptySchema T.Text ()\nendpoints = hello\n\n-- 'mkSchemaDefinitions' doesn't work with 'EmptySchema'\n-- (that is, with 0 endpoints) so we define a\n-- dummy schema type with 1 endpoint to make it compile.\n-- TODO: Repair 'mkSchemaDefinitions'\ntype DummySchema = Endpoint \"dummy\" ()\n\nmkSchemaDefinitions ''DummySchema\n\n$(mkKnownCurrencies [])\n",
        "contractDemoSimulations": [
            {
                "simulationId": 1,
                "simulationActions": [
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    }
                ],
                "simulationName": "Hello, world",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    }
                ]
            }
        ]
    },
    {
        "contractDemoName": "Starter",
        "contractDemoContext": {
            "warnings": [],
            "result": {
                "functionSchema": [
                    {
                        "argument": {
                            "contents": [
                                {
                                    "tag": "FormSchemaInteger"
                                },
                                {
                                    "tag": "FormSchemaValue"
                                }
                            ],
                            "tag": "FormSchemaTuple"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "publish"
                        }
                    },
                    {
                        "argument": {
                            "tag": "FormSchemaInteger"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "redeem"
                        }
                    }
                ],
                "knownCurrencies": [
                    {
                        "hash": "",
                        "friendlyName": "Ada",
                        "knownTokens": [
                            {
                                "unTokenName": ""
                            }
                        ]
                    }
                ]
            }
        },
        "contractDemoEditorContents": "-- This is a starter contract, based on the Game contract,\n-- containing the bare minimum required scaffolding.\n--\n-- What you should change to something more suitable for\n-- your use case:\n--   * The MyDatum type\n--   * The MyRedeemer type\n--\n-- And add function implementations (and rename them to\n-- something suitable) for the endpoints:\n--   * publish\n--   * redeem\n\nimport Control.Monad (void)\nimport Ledger (Address)\nimport Ledger.Constraints qualified as Constraints\nimport Ledger.Typed.Scripts qualified as Scripts\nimport Ledger.Value (Value)\nimport Playground.Contract\nimport Plutus.Contract\nimport Plutus.V1.Ledger.Contexts (ScriptContext)\nimport PlutusTx qualified\nimport PlutusTx.Prelude hiding (Applicative (..))\n\n-- | These are the data script and redeemer types. We are using an integer\n--   value for both, but you should define your own types.\nnewtype MyDatum = MyDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)\nPlutusTx.makeLift ''MyDatum\n\nnewtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)\nPlutusTx.makeLift ''MyRedeemer\n\n-- | This method is the spending validator (which gets lifted to\n--   its on-chain representation).\nvalidateSpend :: MyDatum -> MyRedeemer -> ScriptContext -> Bool\nvalidateSpend _myDataValue _myRedeemerValue _ = error () -- Please provide an implementation.\n\n-- | The address of the contract (the hash of its validator script).\ncontractAddress :: Address\ncontractAddress = Scripts.validatorAddress starterInstance\n\ndata Starter\ninstance Scripts.ValidatorTypes Starter where\n    type instance RedeemerType Starter = MyRedeemer\n    type instance DatumType Starter = MyDatum\n\n-- | The script instance is the compiled validator (ready to go onto the chain)\nstarterInstance :: Scripts.TypedValidator Starter\nstarterInstance = Scripts.mkTypedValidator @Starter\n    $$(PlutusTx.compile [|| validateSpend ||])\n    $$(PlutusTx.compile [|| wrap ||]) where\n        wrap = Scripts.mkUntypedValidator @MyDatum @MyRedeemer\n\n-- | The schema of the contract, with two endpoints.\ntype Schema =\n        Endpoint \"publish\" (Integer, Value)\n        .\\/ Endpoint \"redeem\" Integer\n\ncontract :: AsContractError e => Contract () Schema e ()\ncontract = selectList [publish, redeem]\n\n-- | The \"publish\" contract endpoint.\npublish :: AsContractError e => Promise () Schema e ()\npublish = endpoint @\"publish\" $ \\(i, lockedFunds) -> do\n    let tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds\n    void $ submitTxConstraints starterInstance tx\n\n-- | The \"redeem\" contract endpoint.\nredeem :: AsContractError e => Promise () Schema e ()\nredeem = endpoint @\"redeem\" $ \\myRedeemerValue -> do\n    unspentOutputs <- utxosAt contractAddress\n    let redeemer = MyRedeemer myRedeemerValue\n        tx       = collectFromScript unspentOutputs redeemer\n    void $ submitTxConstraintsSpending starterInstance unspentOutputs tx\n\nendpoints :: AsContractError e => Contract () Schema e ()\nendpoints = contract\n\nmkSchemaDefinitions ''Schema\n\n$(mkKnownCurrencies [])\n",
        "contractDemoSimulations": [
            {
                "simulationId": 1,
                "simulationActions": [
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    {
                                        "contents": 12345,
                                        "tag": "FormIntegerF"
                                    },
                                    {
                                        "contents": {
                                            "getValue": [
                                                [
                                                    {
                                                        "unCurrencySymbol": ""
                                                    },
                                                    [
                                                        [
                                                            {
                                                                "unTokenName": ""
                                                            },
                                                            20000000
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        },
                                        "tag": "FormValueF"
                                    }
                                ],
                                "tag": "FormTupleF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "publish"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    },
                    {
                        "caller": {
                            "getWallet": 2
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": 12345,
                                "tag": "FormIntegerF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "redeem"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    }
                ],
                "simulationName": "Publish/Redeem",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    }
                ]
            },
            {
                "simulationId": 2,
                "simulationActions": [
                    {
                        "recipient": {
                            "getWallet": 2
                        },
                        "tag": "PayToWallet",
                        "sender": {
                            "getWallet": 1
                        },
                        "amount": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            20000000
                                        ]
                                    ]
                                ]
                            ]
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    }
                ],
                "simulationName": "Pay To Wallet",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    }
                ]
            }
        ]
    },
    {
        "contractDemoName": "Game",
        "contractDemoContext": {
            "warnings": [],
            "result": {
                "functionSchema": [
                    {
                        "argument": {
                            "contents": [
                                [
                                    "guessWord",
                                    {
                                        "tag": "FormSchemaString"
                                    }
                                ]
                            ],
                            "tag": "FormSchemaObject"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "guess"
                        }
                    },
                    {
                        "argument": {
                            "contents": [
                                [
                                    "secretWord",
                                    {
                                        "tag": "FormSchemaString"
                                    }
                                ],
                                [
                                    "amount",
                                    {
                                        "tag": "FormSchemaValue"
                                    }
                                ]
                            ],
                            "tag": "FormSchemaObject"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "lock"
                        }
                    }
                ],
                "knownCurrencies": [
                    {
                        "hash": "",
                        "friendlyName": "Ada",
                        "knownTokens": [
                            {
                                "unTokenName": ""
                            }
                        ]
                    }
                ]
            }
        },
        "contractDemoEditorContents": "-- A game with two players. Player 1 thinks of a secret word\n-- and uses its hash, and the game validator script, to lock\n-- some funds (the prize) in a pay-to-script transaction output.\n-- Player 2 guesses the word by attempting to spend the transaction\n-- output. If the guess is correct, the validator script releases the funds.\n-- If it isn't, the funds stay locked.\nimport Control.Monad (void)\nimport Data.ByteString.Char8 qualified as C\nimport Data.Map (Map)\nimport Data.Map qualified as Map\nimport Data.Maybe (catMaybes)\nimport Ledger qualified\nimport Ledger.Ada qualified as Ada\nimport Ledger.Constraints qualified as Constraints\nimport Ledger.Tx (ChainIndexTxOut (..))\nimport Ledger.Typed.Scripts qualified as Scripts\nimport Playground.Contract\nimport Plutus.Contract\nimport Plutus.V1.Ledger.Api (Address, Datum (Datum), ScriptContext, Validator, Value)\nimport PlutusTx qualified\nimport PlutusTx.Prelude hiding (pure, (<$>))\nimport Prelude qualified as Haskell\n\n------------------------------------------------------------\n\nnewtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)\n\nPlutusTx.makeLift ''HashedString\n\nnewtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)\n\nPlutusTx.makeLift ''ClearString\n\ntype GameSchema =\n        Endpoint \"lock\" LockParams\n        .\\/ Endpoint \"guess\" GuessParams\n\ndata Game\ninstance Scripts.ValidatorTypes Game where\n    type instance RedeemerType Game = ClearString\n    type instance DatumType Game = HashedString\n\ngameInstance :: Scripts.TypedValidator Game\ngameInstance = Scripts.mkTypedValidator @Game\n    $$(PlutusTx.compile [|| validateGuess ||])\n    $$(PlutusTx.compile [|| wrap ||]) where\n        wrap = Scripts.mkUntypedValidator @HashedString @ClearString\n\n-- create a data script for the guessing game by hashing the string\n-- and lifting the hash to its on-chain representation\nhashString :: Haskell.String -> HashedString\nhashString = HashedString . sha2_256 . toBuiltin . C.pack\n\n-- create a redeemer script for the guessing game by lifting the\n-- string to its on-chain representation\nclearString :: Haskell.String -> ClearString\nclearString = ClearString . toBuiltin . C.pack\n\n-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)\nvalidateGuess :: HashedString -> ClearString -> ScriptContext -> Bool\nvalidateGuess hs cs _ = isGoodGuess hs cs\n\nisGoodGuess :: HashedString -> ClearString -> Bool\nisGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'\n\n-- | The validator script of the game.\ngameValidator :: Validator\ngameValidator = Scripts.validatorScript gameInstance\n\n-- | The address of the game (the hash of its validator script)\ngameAddress :: Address\ngameAddress = Ledger.scriptAddress gameValidator\n\n-- | Parameters for the \"lock\" endpoint\ndata LockParams = LockParams\n    { secretWord :: Haskell.String\n    , amount     :: Value\n    }\n    deriving stock (Haskell.Eq, Haskell.Show, Generic)\n    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)\n\n--  | Parameters for the \"guess\" endpoint\nnewtype GuessParams = GuessParams\n    { guessWord :: Haskell.String\n    }\n    deriving stock (Haskell.Eq, Haskell.Show, Generic)\n    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)\n\n-- | The \"lock\" contract endpoint. See note [Contract endpoints]\nlock :: AsContractError e => Promise () GameSchema e ()\nlock = endpoint @\"lock\" @LockParams $ \\(LockParams secret amt) -> do\n    logInfo @Haskell.String $ \"Pay \" <> Haskell.show amt <> \" to the script\"\n    let tx         = Constraints.mustPayToTheScript (hashString secret) amt\n    void (submitTxConstraints gameInstance tx)\n\n-- | The \"guess\" contract endpoint. See note [Contract endpoints]\nguess :: AsContractError e => Promise () GameSchema e ()\nguess = endpoint @\"guess\" @GuessParams $ \\(GuessParams theGuess) -> do\n    -- Wait for script to have a UTxO of a least 1 lovelace\n    logInfo @Haskell.String \"Waiting for script to have a UTxO of at least 1 lovelace\"\n    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)\n\n    let redeemer = clearString theGuess\n        tx       = collectFromScript utxos redeemer\n\n    -- Log a message saying if the secret word was correctly guessed\n    let hashedSecretWord = findSecretWordValue utxos\n        isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True\n    if isCorrectSecretWord\n        then logWarn @Haskell.String \"Correct secret word! Submitting the transaction\"\n        else logWarn @Haskell.String \"Incorrect secret word, but still submiting the transaction\"\n\n    -- This is only for test purposes to have a possible failing transaction.\n    -- In a real use-case, we would not submit the transaction if the guess is\n    -- wrong.\n    logInfo @Haskell.String \"Submitting transaction to guess the secret word\"\n    void (submitTxConstraintsSpending gameInstance utxos tx)\n\n-- | Find the secret word in the Datum of the UTxOs\nfindSecretWordValue :: Map TxOutRef ChainIndexTxOut -> Maybe HashedString\nfindSecretWordValue =\n  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue\n\n-- | Extract the secret word in the Datum of a given transaction output is possible\nsecretWordValue :: ChainIndexTxOut -> Maybe HashedString\nsecretWordValue o = do\n  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)\n  PlutusTx.fromBuiltinData d\n\ngame :: AsContractError e => Contract () GameSchema e ()\ngame = do\n    logInfo @Haskell.String \"Waiting for guess or lock endpoint...\"\n    selectList [lock, guess]\n\n{- Note [Contract endpoints]\n\nA contract endpoint is a function that uses the wallet API to interact with the\nblockchain. We can look at contract endpoints from two different points of view.\n\n1. Contract users\n\nContract endpoints are the visible interface of the contract. They provide a\nUI (HTML form) for entering the parameters of the actions we may take as part\nof the contract.\n\n2. Contract authors\n\nAs contract authors we define endpoints as functions that return a value of\ntype 'MockWallet ()'. This type indicates that the function uses the wallet API\nto produce and spend transaction outputs on the blockchain.\n\nEndpoints can have any number of parameters: 'lock' has two\nparameters, 'guess' has one and 'startGame' has none. For each endpoint we\ninclude a call to 'mkFunction' at the end of the contract definition. This\ncauses the Haskell compiler to generate a schema for the endpoint. The Plutus\nPlayground then uses this schema to present an HTML form to the user where the\nparameters can be entered.\n\n-}\n\nendpoints :: AsContractError e => Contract () GameSchema e ()\nendpoints = game\n\nmkSchemaDefinitions ''GameSchema\n\n$(mkKnownCurrencies [])\n",
        "contractDemoSimulations": [
            {
                "simulationId": 1,
                "simulationActions": [
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "secretWord",
                                        {
                                            "contents": "Plutus",
                                            "tag": "FormStringF"
                                        }
                                    ],
                                    [
                                        "amount",
                                        {
                                            "contents": {
                                                "getValue": [
                                                    [
                                                        {
                                                            "unCurrencySymbol": ""
                                                        },
                                                        [
                                                            [
                                                                {
                                                                    "unTokenName": ""
                                                                },
                                                                50000000
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            },
                                            "tag": "FormValueF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "lock"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    },
                    {
                        "caller": {
                            "getWallet": 2
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "guessWord",
                                        {
                                            "contents": "Plutus",
                                            "tag": "FormStringF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "guess"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    }
                ],
                "simulationName": "Basic Game",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    }
                ]
            },
            {
                "simulationId": 2,
                "simulationActions": [
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "secretWord",
                                        {
                                            "contents": "Plutus",
                                            "tag": "FormStringF"
                                        }
                                    ],
                                    [
                                        "amount",
                                        {
                                            "contents": {
                                                "getValue": [
                                                    [
                                                        {
                                                            "unCurrencySymbol": ""
                                                        },
                                                        [
                                                            [
                                                                {
                                                                    "unTokenName": ""
                                                                },
                                                                50000000
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            },
                                            "tag": "FormValueF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "lock"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    },
                    {
                        "caller": {
                            "getWallet": 2
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "guessWord",
                                        {
                                            "contents": "Marlowe",
                                            "tag": "FormStringF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "guess"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    },
                    {
                        "caller": {
                            "getWallet": 3
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "guessWord",
                                        {
                                            "contents": "Plutus",
                                            "tag": "FormStringF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "guess"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    }
                ],
                "simulationName": "One Bad Guess",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 3
                        }
                    }
                ]
            }
        ]
    },
    {
        "contractDemoName": "Vesting",
        "contractDemoContext": {
            "warnings": [],
            "result": {
                "functionSchema": [
                    {
                        "argument": {
                            "tag": "FormSchemaValue"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "retrieve funds"
                        }
                    },
                    {
                        "argument": {
                            "tag": "FormSchemaUnit"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "vest funds"
                        }
                    }
                ],
                "knownCurrencies": [
                    {
                        "hash": "",
                        "friendlyName": "Ada",
                        "knownTokens": [
                            {
                                "unTokenName": ""
                            }
                        ]
                    }
                ]
            }
        },
        "contractDemoEditorContents": "-- Vesting scheme as a PLC contract\nimport Control.Lens (view)\nimport Control.Monad (void, when)\nimport Data.Default (Default (def))\nimport Data.Map qualified as Map\nimport Data.Text qualified as T\n\nimport Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))\nimport Ledger.Ada qualified as Ada\nimport Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)\nimport Ledger.Constraints qualified as Constraints\nimport Ledger.Interval qualified as Interval\nimport Ledger.TimeSlot qualified as TimeSlot\nimport Ledger.Tx qualified as Tx\nimport Ledger.Typed.Scripts qualified as Scripts\nimport Ledger.Value (Value)\nimport Ledger.Value qualified as Value\nimport Playground.Contract\nimport Plutus.Contract\nimport Plutus.Contract.Test\nimport Plutus.Contract.Typed.Tx qualified as Typed\nimport Plutus.V1.Ledger.Api (Address, POSIXTime, POSIXTimeRange, Validator)\nimport Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInfo (..))\nimport Plutus.V1.Ledger.Contexts qualified as Validation\nimport PlutusTx qualified\nimport PlutusTx.Prelude hiding (Semigroup (..), fold)\nimport Prelude as Haskell (Semigroup (..), show)\n\n{- |\n    A simple vesting scheme. Money is locked by a contract and may only be\n    retrieved after some time has passed.\n\n    This is our first example of a contract that covers multiple transactions,\n    with a contract state that changes over time.\n\n    In our vesting scheme the money will be released in two _tranches_ (parts):\n    A smaller part will be available after an initial number of time has\n    passed, and the entire amount will be released at the end. The owner of the\n    vesting scheme does not have to take out all the money at once: They can\n    take out any amount up to the total that has been released so far. The\n    remaining funds stay locked and can be retrieved later.\n\n    Let's start with the data types.\n\n-}\n\ntype VestingSchema =\n        Endpoint \"vest funds\" ()\n        .\\/ Endpoint \"retrieve funds\" Value\n\n-- | Tranche of a vesting scheme.\ndata VestingTranche = VestingTranche {\n    vestingTrancheDate   :: POSIXTime,\n    vestingTrancheAmount :: Value\n    } deriving Generic\n\nPlutusTx.makeLift ''VestingTranche\n\n-- | A vesting scheme consisting of two tranches. Each tranche defines a date\n--   (POSIX time) after which an additional amount can be spent.\ndata VestingParams = VestingParams {\n    vestingTranche1 :: VestingTranche,\n    vestingTranche2 :: VestingTranche,\n    vestingOwner    :: PaymentPubKeyHash\n    } deriving Generic\n\nPlutusTx.makeLift ''VestingParams\n\n{-# INLINABLE totalAmount #-}\n-- | The total amount vested\ntotalAmount :: VestingParams -> Value\ntotalAmount VestingParams{vestingTranche1,vestingTranche2} =\n    vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2\n\n{-# INLINABLE availableFrom #-}\n-- | The amount guaranteed to be available from a given tranche in a given time range.\navailableFrom :: VestingTranche -> POSIXTimeRange -> Value\navailableFrom (VestingTranche d v) range =\n    -- The valid range is an open-ended range starting from the tranche vesting date\n    let validRange = Interval.from d\n    -- If the valid range completely contains the argument range (meaning in particular\n    -- that the start time of the argument range is after the tranche vesting date), then\n    -- the money in the tranche is available, otherwise nothing is available.\n    in if validRange `Interval.contains` range then v else zero\n\navailableAt :: VestingParams -> POSIXTime -> Value\navailableAt VestingParams{vestingTranche1, vestingTranche2} sl =\n    let f VestingTranche{vestingTrancheDate, vestingTrancheAmount} =\n            if sl >= vestingTrancheDate then vestingTrancheAmount else mempty\n    in foldMap f [vestingTranche1, vestingTranche2]\n\n{-# INLINABLE remainingFrom #-}\n-- | The amount that has not been released from this tranche yet\nremainingFrom :: VestingTranche -> POSIXTimeRange -> Value\nremainingFrom t@VestingTranche{vestingTrancheAmount} range =\n    vestingTrancheAmount - availableFrom t range\n\n{-# INLINABLE validate #-}\nvalidate :: VestingParams -> () -> () -> ScriptContext -> Bool\nvalidate VestingParams{vestingTranche1, vestingTranche2, vestingOwner} () () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =\n    let\n        remainingActual  = Validation.valueLockedBy txInfo (Validation.ownHash ctx)\n\n        remainingExpected =\n            remainingFrom vestingTranche1 txInfoValidRange\n            + remainingFrom vestingTranche2 txInfoValidRange\n\n    in remainingActual `Value.geq` remainingExpected\n            -- The policy encoded in this contract\n            -- is \"vestingOwner can do with the funds what they want\" (as opposed\n            -- to \"the funds must be paid to vestingOwner\"). This is enforcey by\n            -- the following condition:\n            && Validation.txSignedBy txInfo (unPaymentPubKeyHash vestingOwner)\n            -- That way the recipient of the funds can pay them to whatever address they\n            -- please, potentially saving one transaction.\n\ndata Vesting\ninstance Scripts.ValidatorTypes Vesting where\n    type instance RedeemerType Vesting = ()\n    type instance DatumType Vesting = ()\n\nvestingScript :: VestingParams -> Validator\nvestingScript = Scripts.validatorScript . typedValidator\n\ntypedValidator :: VestingParams -> Scripts.TypedValidator Vesting\ntypedValidator = Scripts.mkTypedValidatorParam @Vesting\n    $$(PlutusTx.compile [|| validate ||])\n    $$(PlutusTx.compile [|| wrap ||])\n    where\n        wrap = Scripts.mkUntypedValidator\n\ncontractAddress :: VestingParams -> Address\ncontractAddress = Scripts.validatorAddress . typedValidator\n\nvestingContract :: VestingParams -> Contract () VestingSchema T.Text ()\nvestingContract vesting = selectList [vest, retrieve]\n  where\n    vest = endpoint @\"vest funds\" $ \\_ -> vestFundsC vesting\n    retrieve = endpoint @\"retrieve funds\" $ \\payment -> do\n        liveness <- retrieveFundsC vesting payment\n        case liveness of\n            Alive -> awaitPromise retrieve\n            Dead  -> pure ()\n\npayIntoContract :: Value -> TxConstraints () ()\npayIntoContract = mustPayToTheScript ()\n\nvestFundsC\n    :: VestingParams\n    -> Contract () s T.Text ()\nvestFundsC vesting = do\n    let txn = payIntoContract (totalAmount vesting)\n    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator vesting) txn\n      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx\n\ndata Liveness = Alive | Dead\n\nretrieveFundsC\n    :: VestingParams\n    -> Value\n    -> Contract () s T.Text Liveness\nretrieveFundsC vesting payment = do\n    let inst = typedValidator vesting\n        addr = Scripts.validatorAddress inst\n    nextTime <- awaitTime 0\n    unspentOutputs <- utxosAt addr\n    let\n        currentlyLocked = foldMap (view Tx.ciTxOutValue) (Map.elems unspentOutputs)\n        remainingValue = currentlyLocked - payment\n        mustRemainLocked = totalAmount vesting - availableAt vesting nextTime\n        maxPayment = currentlyLocked - mustRemainLocked\n\n    when (remainingValue `Value.lt` mustRemainLocked)\n        $ throwError\n        $ T.unwords\n            [ \"Cannot take out\"\n            , T.pack (show payment) `T.append` \".\"\n            , \"The maximum is\"\n            , T.pack (show maxPayment) `T.append` \".\"\n            , \"At least\"\n            , T.pack (show mustRemainLocked)\n            , \"must remain locked by the script.\"\n            ]\n\n    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead\n        remainingOutputs = case liveness of\n                            Alive -> payIntoContract remainingValue\n                            Dead  -> mempty\n        txn = Typed.collectFromScript unspentOutputs ()\n                <> remainingOutputs\n                <> mustValidateIn (Interval.from nextTime)\n                <> mustBeSignedBy (vestingOwner vesting)\n                -- we don't need to add a pubkey output for 'vestingOwner' here\n                -- because this will be done by the wallet when it balances the\n                -- transaction.\n    mkTxConstraints (Constraints.typedValidatorLookups inst\n                  <> Constraints.unspentOutputs unspentOutputs) txn\n      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx\n    return liveness\n\nendpoints :: Contract () VestingSchema T.Text ()\nendpoints = vestingContract vestingParams\n  where\n    vestingOwner = mockWalletPaymentPubKeyHash w1\n    vestingParams =\n        VestingParams {vestingTranche1, vestingTranche2, vestingOwner}\n    vestingTranche1 =\n        VestingTranche\n            {vestingTrancheDate = TimeSlot.scSlotZeroTime def + 20000, vestingTrancheAmount = Ada.lovelaceValueOf 50_000_000}\n    vestingTranche2 =\n        VestingTranche\n            {vestingTrancheDate = TimeSlot.scSlotZeroTime def + 40000, vestingTrancheAmount = Ada.lovelaceValueOf 30_000_000}\n\nmkSchemaDefinitions ''VestingSchema\n\n$(mkKnownCurrencies [])\n",
        "contractDemoSimulations": [
            {
                "simulationId": 1,
                "simulationActions": [
                    {
                        "caller": {
                            "getWallet": 2
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "tag": "FormUnitF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "vest funds"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 20
                    },
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": {
                                    "getValue": [
                                        [
                                            {
                                                "unCurrencySymbol": ""
                                            },
                                            [
                                                [
                                                    {
                                                        "unTokenName": ""
                                                    },
                                                    40000000
                                                ]
                                            ]
                                        ]
                                    ]
                                },
                                "tag": "FormValueF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "retrieve funds"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 40
                    },
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": {
                                    "getValue": [
                                        [
                                            {
                                                "unCurrencySymbol": ""
                                            },
                                            [
                                                [
                                                    {
                                                        "unTokenName": ""
                                                    },
                                                    40000000
                                                ]
                                            ]
                                        ]
                                    ]
                                },
                                "tag": "FormValueF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "retrieve funds"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocks",
                        "blocks": 1
                    }
                ],
                "simulationName": "Vest/Retrieve",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    }
                ]
            }
        ]
    },
    {
        "contractDemoName": "Crowd Funding",
        "contractDemoContext": {
            "warnings": [],
            "result": {
                "functionSchema": [
                    {
                        "argument": {
                            "contents": [
                                [
                                    "contribValue",
                                    {
                                        "tag": "FormSchemaValue"
                                    }
                                ]
                            ],
                            "tag": "FormSchemaObject"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "contribute"
                        }
                    },
                    {
                        "argument": {
                            "tag": "FormSchemaUnit"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "schedule collection"
                        }
                    }
                ],
                "knownCurrencies": [
                    {
                        "hash": "",
                        "friendlyName": "Ada",
                        "knownTokens": [
                            {
                                "unTokenName": ""
                            }
                        ]
                    }
                ]
            }
        },
        "contractDemoEditorContents": "-- Crowdfunding contract implemented using the [[Plutus]] interface.\n-- This is the fully parallel version that collects all contributions\n-- in a single transaction.\n--\n-- Note [Transactions in the crowdfunding campaign] explains the structure of\n-- this contract on the blockchain.\n\nimport Control.Applicative (Applicative (pure))\nimport Control.Monad (void)\nimport Data.Default (Default (def))\nimport Data.Text (Text)\nimport Ledger (POSIXTime, POSIXTimeRange, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (..), TxInfo (..),\n               getCardanoTxId)\nimport Ledger qualified\nimport Ledger.Interval qualified as Interval\nimport Ledger.TimeSlot qualified as TimeSlot\nimport Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)\nimport Ledger.Value (Value)\nimport Playground.Contract\nimport Plutus.Contract\nimport Plutus.Contract.Constraints qualified as Constraints\nimport Plutus.Contract.Typed.Tx qualified as Typed\nimport Plutus.Script.Utils.V1.Scripts qualified as Scripts\nimport Plutus.V1.Ledger.Api (Validator)\nimport Plutus.V1.Ledger.Contexts qualified as V\nimport PlutusTx qualified\nimport PlutusTx.Prelude hiding (Applicative (..), Semigroup (..))\nimport Prelude (Semigroup (..))\nimport Prelude qualified as Haskell\nimport Wallet.Emulator qualified as Emulator\n\n-- | A crowdfunding campaign.\ndata Campaign = Campaign\n    { campaignDeadline           :: POSIXTime\n    -- ^ The date by which the campaign funds can be contributed.\n    , campaignCollectionDeadline :: POSIXTime\n    -- ^ The date by which the campaign owner has to collect the funds\n    , campaignOwner              :: PaymentPubKeyHash\n    -- ^ Public key of the campaign owner. This key is entitled to retrieve the\n    --   funds if the campaign is successful.\n    } deriving (Generic, ToJSON, FromJSON, ToSchema)\n\nPlutusTx.makeLift ''Campaign\n\n-- | Action that can be taken by the participants in this contract. A value of\n--   `CampaignAction` is provided as the redeemer. The validator script then\n--   checks if the conditions for performing this action are met.\n--\ndata CampaignAction = Collect | Refund\n\nPlutusTx.unstableMakeIsData ''CampaignAction\nPlutusTx.makeLift ''CampaignAction\n\ntype CrowdfundingSchema =\n        Endpoint \"schedule collection\" ()\n        .\\/ Endpoint \"contribute\" Contribution\n\nnewtype Contribution = Contribution\n        { contribValue :: Value\n        -- ^ how much to contribute\n        } deriving stock (Haskell.Eq, Show, Generic)\n          deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)\n\n-- | Construct a 'Campaign' value from the campaign parameters,\n--   using the wallet's public key.\nmkCampaign :: POSIXTime -> POSIXTime -> Wallet -> Campaign\nmkCampaign ddl collectionDdl ownerWallet =\n    Campaign\n        { campaignDeadline = ddl\n        , campaignCollectionDeadline = collectionDdl\n        , campaignOwner = Emulator.mockWalletPaymentPubKeyHash ownerWallet\n        }\n\n-- | The 'POSIXTimeRange' during which the funds can be collected\ncollectionRange :: Campaign -> POSIXTimeRange\ncollectionRange cmp =\n    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp - 1)\n\n-- | The 'POSIXTimeRange' during which a refund may be claimed\nrefundRange :: Campaign -> POSIXTimeRange\nrefundRange cmp =\n    Interval.from (campaignCollectionDeadline cmp)\n\ndata Crowdfunding\ninstance Scripts.ValidatorTypes Crowdfunding where\n    type instance RedeemerType Crowdfunding = CampaignAction\n    type instance DatumType Crowdfunding = PaymentPubKeyHash\n\ntypedValidator :: Campaign -> Scripts.TypedValidator Crowdfunding\ntypedValidator = Scripts.mkTypedValidatorParam @Crowdfunding\n    $$(PlutusTx.compile [|| mkValidator ||])\n    $$(PlutusTx.compile [|| wrap ||])\n    where\n        wrap = Scripts.mkUntypedValidator\n\n{-# INLINABLE validRefund #-}\nvalidRefund :: Campaign -> PaymentPubKeyHash -> TxInfo -> Bool\nvalidRefund campaign contributor txinfo =\n    -- Check that the transaction falls in the refund range of the campaign\n    (refundRange campaign `Interval.contains` txInfoValidRange txinfo)\n    -- Check that the transaction is signed by the contributor\n    && (txinfo `V.txSignedBy` unPaymentPubKeyHash contributor)\n\nvalidCollection :: Campaign -> TxInfo -> Bool\nvalidCollection campaign txinfo =\n    -- Check that the transaction falls in the collection range of the campaign\n    (collectionRange campaign `Interval.contains` txInfoValidRange txinfo)\n    -- Check that the transaction is signed by the campaign owner\n    && (txinfo `V.txSignedBy` unPaymentPubKeyHash (campaignOwner campaign))\n\n-- | The validator script is of type 'CrowdfundingValidator', and is\n-- additionally parameterized by a 'Campaign' definition. This argument is\n-- provided by the Plutus client, using 'PlutusTx.applyCode'.\n-- As a result, the 'Campaign' definition is part of the script address,\n-- and different campaigns have different addresses.\nmkValidator :: Campaign -> PaymentPubKeyHash -> CampaignAction -> ScriptContext -> Bool\nmkValidator c con act p = case act of\n    -- the \"refund\" branch\n    Refund  -> validRefund c con (scriptContextTxInfo p)\n    -- the \"collection\" branch\n    Collect -> validCollection c (scriptContextTxInfo p)\n\n-- | The validator script that determines whether the campaign owner can\n--   retrieve the funds or the contributors can claim a refund.\n--\ncontributionScript :: Campaign -> Validator\ncontributionScript = Scripts.validatorScript . typedValidator\n\n-- | The address of a [[Campaign]]\ncampaignAddress :: Campaign -> ValidatorHash\ncampaignAddress = Scripts.validatorHash . contributionScript\n\n-- | The crowdfunding contract for the 'Campaign'.\ncrowdfunding :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ()\ncrowdfunding c = selectList [contribute c, scheduleCollection c]\n\n-- | A sample campaign\ntheCampaign :: POSIXTime -> Campaign\ntheCampaign startTime = Campaign\n    { campaignDeadline = startTime + 40000\n    , campaignCollectionDeadline = startTime + 60000\n    , campaignOwner = Emulator.mockWalletPaymentPubKeyHash (Emulator.knownWallet 1)\n    }\n\n-- | The \"contribute\" branch of the contract for a specific 'Campaign'. Exposes\n--   an endpoint that allows the user to enter their public key and the\n--   contribution. Then waits until the campaign is over, and collects the\n--   refund if the funding was not collected.\ncontribute :: AsContractError e => Campaign -> Promise () CrowdfundingSchema e ()\ncontribute cmp = endpoint @\"contribute\" $ \\Contribution{contribValue} -> do\n    contributor <- ownPaymentPubKeyHash\n    let inst = typedValidator cmp\n        tx = Constraints.mustPayToTheScript contributor contribValue\n                <> Constraints.mustValidateIn (Interval.to (campaignDeadline cmp))\n    txid <- fmap getCardanoTxId $ mkTxConstraints (Constraints.typedValidatorLookups inst) tx\n        >>= adjustUnbalancedTx >>= submitUnbalancedTx\n\n    utxo <- watchAddressUntilTime (Scripts.validatorAddress inst) (campaignCollectionDeadline cmp)\n\n    -- 'utxo' is the set of unspent outputs at the campaign address at the\n    -- collection deadline. If 'utxo' still contains our own contribution\n    -- then we can claim a refund.\n\n    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId\n        tx' = Typed.collectFromScriptFilter flt utxo Refund\n                <> Constraints.mustValidateIn (refundRange cmp)\n                <> Constraints.mustBeSignedBy contributor\n    if Constraints.modifiesUtxoSet tx'\n    then do\n        logInfo @Text \"Claiming refund\"\n        void $ mkTxConstraints (Constraints.typedValidatorLookups inst\n                             <> Constraints.unspentOutputs utxo) tx'\n            >>= adjustUnbalancedTx >>= submitUnbalancedTx\n    else pure ()\n\n-- | The campaign owner's branch of the contract for a given 'Campaign'. It\n--   watches the campaign address for contributions and collects them if\n--   the funding goal was reached in time.\nscheduleCollection :: AsContractError e => Campaign -> Promise () CrowdfundingSchema e ()\nscheduleCollection cmp =\n    -- Expose an endpoint that lets the user fire the starting gun on the\n    -- campaign. (This endpoint isn't technically necessary, we could just\n    -- run the 'trg' action right away)\n    endpoint @\"schedule collection\" $ \\() -> do\n        let inst = typedValidator cmp\n\n        _ <- awaitTime $ campaignDeadline cmp\n        unspentOutputs <- utxosAt (Scripts.validatorAddress inst)\n\n        let tx = Typed.collectFromScript unspentOutputs Collect\n                <> Constraints.mustValidateIn (collectionRange cmp)\n        void $ submitTxConstraintsSpending inst unspentOutputs tx\n\n{- note [Transactions in the crowdfunding campaign]\n\nAssume there is a campaign `c :: Campaign` with two contributors\n(identified by public key `pc_1` and `pc_2`) and one campaign owner (pco).\nEach contributor creates a transaction, `t_1` and `t_2`, whose outputs are\nlocked by the scripts `contributionScript c pc_1` and `contributionScript\nc pc_1` respectively.\n\nThere are two outcomes for the campaign.\n\n1. Campaign owner collects the funds from both contributors. In this case\n   the owner creates a single transaction with two inputs, referring to\n   `t_1` and `t_2`. Each input contains the script `contributionScript c`\n   specialised to a contributor. The redeemer script of this transaction\n   contains the value `Collect`, prompting the validator script to check the\n   branch for `Collect`.\n\n2. Refund. In this case each contributor creates a transaction with a\n   single input claiming back their part of the funds. This case is\n   covered by the `Refund` branch, and its redeemer script is the\n   `Refund` action.\n\nIn both cases, the validator script is run twice. In the first case\nthere is a single transaction consuming both inputs. In the second case there\nare two different transactions that may happen at different times.\n\n-}\n\n{- note [PendingTx]\n\nThis part of the API (the PendingTx argument) is experimental and subject\nto change.\n\n-}\n\nendpoints :: AsContractError e => Contract () CrowdfundingSchema e ()\nendpoints = crowdfunding (theCampaign $ TimeSlot.scSlotZeroTime def)\n\nmkSchemaDefinitions ''CrowdfundingSchema\n\n$(mkKnownCurrencies [])\n",
        "contractDemoSimulations": [
            {
                "simulationId": 1,
                "simulationActions": [
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "tag": "FormUnitF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "schedule collection"
                            }
                        }
                    },
                    {
                        "caller": {
                            "getWallet": 2
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "contribValue",
                                        {
                                            "contents": {
                                                "getValue": [
                                                    [
                                                        {
                                                            "unCurrencySymbol": ""
                                                        },
                                                        [
                                                            [
                                                                {
                                                                    "unTokenName": ""
                                                                },
                                                                11000000
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            },
                                            "tag": "FormValueF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "contribute"
                            }
                        }
                    },
                    {
                        "caller": {
                            "getWallet": 3
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "contribValue",
                                        {
                                            "contents": {
                                                "getValue": [
                                                    [
                                                        {
                                                            "unCurrencySymbol": ""
                                                        },
                                                        [
                                                            [
                                                                {
                                                                    "unTokenName": ""
                                                                },
                                                                10000000
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            },
                                            "tag": "FormValueF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "contribute"
                            }
                        }
                    },
                    {
                        "caller": {
                            "getWallet": 4
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": [
                                    [
                                        "contribValue",
                                        {
                                            "contents": {
                                                "getValue": [
                                                    [
                                                        {
                                                            "unCurrencySymbol": ""
                                                        },
                                                        [
                                                            [
                                                                {
                                                                    "unTokenName": ""
                                                                },
                                                                90000000
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            },
                                            "tag": "FormValueF"
                                        }
                                    ]
                                ],
                                "tag": "FormObjectF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "contribute"
                            }
                        }
                    },
                    {
                        "tag": "AddBlocksUntil",
                        "slot": {
                            "getSlot": 41
                        }
                    }
                ],
                "simulationName": "Basic Campaign",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 3
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 4
                        }
                    }
                ]
            }
        ]
    },
    {
        "contractDemoName": "Error Handling",
        "contractDemoContext": {
            "warnings": [],
            "result": {
                "functionSchema": [
                    {
                        "argument": {
                            "tag": "FormSchemaUnit"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "catchContractError"
                        }
                    },
                    {
                        "argument": {
                            "tag": "FormSchemaString"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "catchError"
                        }
                    },
                    {
                        "argument": {
                            "tag": "FormSchemaString"
                        },
                        "endpointDescription": {
                            "getEndpointDescription": "throwError"
                        }
                    }
                ],
                "knownCurrencies": [
                    {
                        "hash": "",
                        "friendlyName": "Ada",
                        "knownTokens": [
                            {
                                "unTokenName": ""
                            }
                        ]
                    }
                ]
            }
        },
        "contractDemoEditorContents": "import Control.Lens (makeClassyPrisms, prism', review)\nimport Control.Monad (void)\nimport Control.Monad.Error.Lens (catching, throwing, throwing_)\nimport Data.Text (Text)\nimport Data.Text qualified as T\n\nimport Data.Default (Default (def))\nimport Ledger.TimeSlot (SlotConfig)\nimport Ledger.TimeSlot qualified as TimeSlot\nimport Playground.Contract\nimport Plutus.Contract (AsContractError (_ContractError), ContractError, awaitTime, logInfo, mapError, selectList)\nimport Prelude (Maybe (..), const, show, ($), (+), (.), (<>))\n\n-- Demonstrates how to deal with errors in Plutus contracts. We define a custom\n-- error type 'MyError' with three constructors and use\n-- 'Control.Lens.makeClassyPrisms' to generate the 'AsMyError' class. We can\n-- then use 'MyError' in our contracts with the combinators from\n-- 'Control.Monad.Error.Lens'. The unit tests in 'Spec.ErrorHandling' show how\n-- to write tests for error conditions.\n\ntype Schema =\n    Endpoint \"throwError\" Text\n     .\\/ Endpoint \"catchError\" Text\n     .\\/ Endpoint \"catchContractError\" ()\n\n-- | 'MyError' has a constructor for each type of error that our contract\n --   can throw. The 'AContractError' constructor wraps a 'ContractError'.\ndata MyError =\n    Error1 Text\n    | Error2\n    | AContractError ContractError\n    deriving Show\n\nmakeClassyPrisms ''MyError\n\ninstance AsContractError MyError where\n    -- 'ContractError' is another error type. It is defined in\n    -- 'Plutus.Contract.Request'. By making 'MyError' an\n    -- instance of 'AsContractError' we can handle 'ContractError's\n    -- thrown by other contracts in our code (see 'catchContractError')\n    _ContractError = _AContractError\n\ninstance AsMyError Text where\n    _MyError = prism' (T.pack . show) (const Nothing)\n\n-- | Throw an 'Error1', using 'Control.Monad.Error.Lens.throwing' and the\n--   prism generated by 'makeClassyPrisms'\nthrow :: AsMyError e => Text -> Contract () s e ()\nthrow e = do\n    logInfo @Text $  \"throwError: \" <> e\n    throwing _Error1 e\n\n-- | Handle the error from 'throw' using 'Control.Monad.Error.Lens.catching'\nthrowAndCatch :: AsMyError e => Text -> Contract () s e ()\nthrowAndCatch e =\n    let handleError1 :: Text -> Contract () s e ()\n        handleError1 t = logInfo $ \"handleError: \" <> t\n     in catching _Error1 (throw e) handleError1\n\n-- | Handle an error from 'awaitTime by wrapping it in the 'AContractError'\n--   constructor\ncatchContractError :: (AsMyError e) => SlotConfig -> Contract () s e ()\ncatchContractError slotCfg =\n    catching _AContractError\n        (void $ mapError (review _AContractError) $\n            awaitTime $ TimeSlot.scSlotZeroTime slotCfg + 10000)\n        (\\_ -> throwing_ _Error2)\n\ncontract\n    :: ( AsMyError e\n       , AsContractError e\n       )\n    => SlotConfig\n    -> Contract () Schema e ()\ncontract slotCfg = selectList\n    [ endpoint @\"throwError\" throw\n    , endpoint @\"catchError\" throwAndCatch\n    , endpoint @\"catchContractError\" $ const (catchContractError slotCfg)\n    ]\n\nendpoints :: (AsMyError e, AsContractError e) => Contract () Schema e ()\nendpoints = contract def\n\nmkSchemaDefinitions ''Schema\n\n$(mkKnownCurrencies [])\n",
        "contractDemoSimulations": [
            {
                "simulationId": 1,
                "simulationActions": [
                    {
                        "caller": {
                            "getWallet": 1
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": "Hello",
                                "tag": "FormStringF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "throwError"
                            }
                        }
                    },
                    {
                        "caller": {
                            "getWallet": 2
                        },
                        "tag": "CallEndpoint",
                        "argumentValues": {
                            "argument": {
                                "contents": "World",
                                "tag": "FormStringF"
                            },
                            "endpointDescription": {
                                "getEndpointDescription": "catchError"
                            }
                        }
                    }
                ],
                "simulationName": "Throw/Catch",
                "simulationWallets": [
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 1
                        }
                    },
                    {
                        "simulatorWalletBalance": {
                            "getValue": [
                                [
                                    {
                                        "unCurrencySymbol": ""
                                    },
                                    [
                                        [
                                            {
                                                "unTokenName": ""
                                            },
                                            100000000
                                        ]
                                    ]
                                ]
                            ]
                        },
                        "simulatorWalletWallet": {
                            "getWallet": 2
                        }
                    }
                ]
            }
        ]
    }
]"""
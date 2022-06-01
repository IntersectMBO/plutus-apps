{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Generators for constructing blockchains and transactions for use in property-based testing.
module Ledger.Generators(
    -- * Mockchain
    Mockchain(..),
    genMockchain,
    genMockchain',
    emptyChain,
    GeneratorModel(..),
    generatorModel,
    -- * Transactions
    genValidTransaction,
    genValidTransaction',
    genValidTransactionSpending,
    genValidTransactionSpending',
    genInitialTransaction,
    genValidatorContext,
    genMintingPolicyContext,
    -- * Assertions
    assertValid,
    -- * Time
    genInterval,
    genSlotRange,
    genTimeRange,
    genSlot,
    genPOSIXTime,
    genSlotConfig,
    -- * Etc.
    genSomeCardanoApiTx,
    genAda,
    genValue,
    genValueNonNegative,
    genSizedByteString,
    genSizedByteStringExact,
    genTokenName,
    genSeed,
    genPassphrase,
    splitVal,
    validateMockchain,
    signAll,
    knownPaymentPublicKeys
    ) where

import Cardano.Api qualified as C
import Control.Monad (replicateM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Writer (runWriter)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Foldable (fold, foldl')
import Data.Functor.Identity (Identity)
import Data.List (sort)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (Ada, CurrencySymbol, Interval, OnChainTx (Valid), POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange,
               Passphrase (Passphrase), PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),
               RedeemerPtr (RedeemerPtr), ScriptContext (ScriptContext), ScriptTag (Mint), Slot (Slot), SlotRange,
               SomeCardanoApiTx (SomeTx), TokenName,
               Tx (txFee, txInputs, txMint, txMintScripts, txOutputs, txRedeemers, txValidRange), TxIn,
               TxInInfo (txInInfoOutRef), TxInfo (TxInfo), TxOut (txOutValue), TxOutRef (TxOutRef),
               UtxoIndex (UtxoIndex), ValidationCtx (ValidationCtx), Value, _runValidation, addSignature', pubKeyTxIn,
               pubKeyTxOut, toPublicKey, txId)
import Ledger qualified
import Ledger.CardanoWallet qualified as CW
import Ledger.Fee (FeeConfig (fcScriptsFeeFactor), calcFees)
import Ledger.Index qualified as Index
import Ledger.Params (Params (pSlotConfig))
import Ledger.TimeSlot (SlotConfig)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V1.Generators as ScriptGen
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts qualified as Script

-- | Attach signatures of all known private keys to a transaction.
signAll :: Tx -> Tx
signAll tx = foldl' (flip addSignature') tx
           $ fmap unPaymentPrivateKey knownPaymentPrivateKeys

-- | The parameters for the generators in this module.
data GeneratorModel = GeneratorModel {
    gmInitialBalance :: Map PaymentPubKey Value,
    -- ^ Value created at the beginning of the blockchain.
    gmPubKeys        :: Set PaymentPubKey
    -- ^ Public keys that are to be used for generating transactions.
    } deriving Show

-- | A generator model with some sensible defaults.
generatorModel :: GeneratorModel
generatorModel =
    let vl = Ada.lovelaceValueOf 100_000_000
        pubKeys = knownPaymentPublicKeys

    in
    GeneratorModel
    { gmInitialBalance = Map.fromList $ zip pubKeys (repeat vl)
    , gmPubKeys        = Set.fromList pubKeys
    }

-- | Estimate a constant fee for all transactions.
constantFee :: FeeConfig
constantFee = def { fcScriptsFeeFactor = 0 }

-- | Blockchain for testing the emulator implementation and traces.
--
--   To avoid having to rely on functions from the implementation of
--   plutus-ledger (in particular, 'Ledger.Tx.unspentOutputs') we note the
--   unspent outputs of the chain when it is first created.
data Mockchain = Mockchain {
    mockchainInitialTxPool :: [Tx],
    mockchainUtxo          :: Map TxOutRef TxOut,
    mockchainParams        :: Params
    } deriving Show

-- | The empty mockchain.
emptyChain :: Mockchain
emptyChain = Mockchain [] Map.empty def

-- | Generate a mockchain.
--
--   TODO: Generate more than 1 txn
genMockchain' :: MonadGen m
    => GeneratorModel
    -> m Mockchain
genMockchain' gm = do
    let (txn, ot) = genInitialTransaction gm
        tid = txId txn
    slotCfg <- genSlotConfig
    pure Mockchain {
        mockchainInitialTxPool = [txn],
        mockchainUtxo = Map.fromList $ first (TxOutRef tid) <$> zip [0..] ot,
        mockchainParams = def { pSlotConfig = slotCfg }
        }

-- | Generate a mockchain using the default 'GeneratorModel'.
--
genMockchain :: MonadGen m => m Mockchain
genMockchain = genMockchain' generatorModel

-- | A transaction with no inputs that mints some value (to be used at the
--   beginning of a blockchain).
genInitialTransaction ::
       GeneratorModel
    -> (Tx, [TxOut])
genInitialTransaction GeneratorModel{..} =
    let
        o = fmap (\f -> f Nothing) $ (uncurry $ flip pubKeyTxOut) <$> Map.toList gmInitialBalance
        t = fold gmInitialBalance
    in (mempty {
        txOutputs = o,
        txMint = t,
        txValidRange = Interval.from 0
        }, o)

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the minimum fee.
genValidTransaction :: MonadGen m
    => Mockchain
    -> m Tx
genValidTransaction = genValidTransaction' generatorModel constantFee

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the estimated fee.
genValidTransaction' :: MonadGen m
    => GeneratorModel
    -> FeeConfig
    -> Mockchain
    -> m Tx
genValidTransaction' g feeCfg (Mockchain _ ops _) = do
    -- Take a random number of UTXO from the input
    nUtxo <- if Map.null ops
                then Gen.discard
                else Gen.int (Range.linear 1 (Map.size ops))
    let ins = Set.fromList $ pubKeyTxIn . fst <$> inUTXO
        inUTXO = take nUtxo $ Map.toList ops
        totalVal = foldl' (<>) mempty $ map (txOutValue . snd) inUTXO
    genValidTransactionSpending' g feeCfg ins totalVal

genValidTransactionSpending :: MonadGen m
    => Set.Set TxIn
    -> Value
    -> m Tx
genValidTransactionSpending = genValidTransactionSpending' generatorModel constantFee

genValidTransactionSpending' :: MonadGen m
    => GeneratorModel
    -> FeeConfig
    -> Set.Set TxIn
    -> Value
    -> m Tx
genValidTransactionSpending' g feeCfg ins totalVal = do
    mintAmount <- toInteger <$> Gen.int (Range.linear 0 maxBound)
    mintTokenName <- genTokenName
    let mintValue = if mintAmount == 0
                       then Nothing
                       else Just $ ScriptGen.someTokenValue mintTokenName mintAmount
        fee' = calcFees feeCfg 0
        numOut = Set.size (gmPubKeys g) - 1
        totalValAda = Ada.fromValue totalVal
        totalValTokens = if Value.isZero (Value.noAdaValue totalVal) then Nothing else Just (Value.noAdaValue totalVal)
    if fee' < totalValAda
        then do
            -- We only split the Ada part of the input value
            splitOutVals <- splitVal numOut (totalValAda - fee')
            let outVals = case totalValTokens <> mintValue of
                  Nothing -> do
                    fmap Ada.toValue splitOutVals
                  Just mv -> do
                    -- If there is a minted value, we look for a value in the
                    -- splitted values which can be associated with it.
                    let outValForMint =
                          maybe mempty id $ List.find (\v -> v >= Ledger.minAdaTxOut)
                                          $ List.sort splitOutVals
                    Ada.toValue outValForMint <> mv : fmap Ada.toValue (List.delete outValForMint splitOutVals)
            let tx = mempty
                        { txInputs = ins
                        , txOutputs = fmap (\f -> f Nothing) $ uncurry pubKeyTxOut <$> zip outVals (Set.toList $ gmPubKeys g)
                        , txMint = maybe mempty id mintValue
                        , txMintScripts = Set.singleton ScriptGen.alwaysSucceedPolicy
                        , txRedeemers = Map.singleton (RedeemerPtr Mint 0) Script.unitRedeemer
                        , txFee = Ada.toValue fee'
                        }

                -- sign the transaction with all known wallets
                -- this is somewhat crude (but technically valid)
            pure (signAll tx)
        else Gen.discard

-- | Generate an 'Interval where the lower bound if less or equal than the
-- upper bound.
genInterval :: (MonadFail m, Ord a)
            => m a
            -> m (Interval a)
genInterval gen = do
    [b, e] <- sort <$> replicateM 2 gen
    return $ Interval.interval b e

-- | Generate a 'SlotRange' where the lower bound if less or equal than the
-- upper bound.
genSlotRange :: (MonadFail m, Hedgehog.MonadGen m) => m SlotRange
genSlotRange = genInterval genSlot

-- | Generate a 'POSIXTimeRange' where the lower bound if less or equal than the
-- upper bound.
genTimeRange :: (MonadFail m, Hedgehog.MonadGen m) => SlotConfig -> m POSIXTimeRange
genTimeRange sc = genInterval $ genPOSIXTime sc

-- | Generate a 'Slot' where the lowest slot number is 0.
genSlot :: (Hedgehog.MonadGen m) => m Slot
genSlot = Slot <$> Gen.integral (Range.linear 0 10000)

-- | Generate a 'POSIXTime' where the lowest value is 'scSlotZeroTime' given a
-- 'SlotConfig'.
genPOSIXTime :: (Hedgehog.MonadGen m) => SlotConfig -> m POSIXTime
genPOSIXTime sc = do
    let beginTime = getPOSIXTime $ TimeSlot.scSlotZeroTime sc
    POSIXTime <$> Gen.integral (Range.linear beginTime (beginTime + 10000000))

-- | Generate a 'SlotConfig' where the slot length goes from 1 to 100000
-- ms and the time of Slot 0 is the default 'scSlotZeroTime'.
genSlotConfig :: Hedgehog.MonadGen m => m SlotConfig
genSlotConfig = do
    sl <- Gen.integral (Range.linear 1 1000000)
    return $ def { TimeSlot.scSlotLength = sl }

-- TODO Unfortunately, there's no way to get a warning if another era has been
-- added to EraInMode. Alternative way?
genSomeCardanoApiTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genSomeCardanoApiTx = Gen.choice [ genByronEraInCardanoModeTx
                                 , genShelleyEraInCardanoModeTx
                                 , genAllegraEraInCardanoModeTx
                                 , genMaryEraInCardanoModeTx
                                 , genAlonzoEraInCardanoModeTx
                                 ]

genByronEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genByronEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ByronEra
  pure $ SomeTx tx C.ByronEraInCardanoMode

genShelleyEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genShelleyEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyEra
  pure $ SomeTx tx C.ShelleyEraInCardanoMode

genAllegraEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genAllegraEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.AllegraEra
  pure $ SomeTx tx C.AllegraEraInCardanoMode

genMaryEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genMaryEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.MaryEra
  pure $ SomeTx tx C.MaryEraInCardanoMode

genAlonzoEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genAlonzoEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.AlonzoEra
  pure $ SomeTx tx C.AlonzoEraInCardanoMode

genAda :: MonadGen m => m Ada
genAda = Ada.lovelaceOf <$> Gen.integral (Range.linear 0 (100000 :: Integer))

-- | Generate a 'ByteString s' of up to @s@ bytes.
genSizedByteString :: forall m. MonadGen m => Int -> m BS.ByteString
genSizedByteString s =
    let range = Range.linear 0 s
    in Gen.bytes range

-- | Generate a 'ByteString s' of exactly @s@ bytes.
genSizedByteStringExact :: forall m. MonadGen m => Int -> m BS.ByteString
genSizedByteStringExact s =
    let range = Range.singleton s
    in Gen.bytes range

-- | A TokenName is either an arbitrary bytestring or the ada token name
genTokenName :: MonadGen m => m TokenName
genTokenName = Gen.choice
    [ Value.tokenName <$> genSizedByteString 32
    , pure Ada.adaToken
    ]

-- | A currency symbol is either a validator hash (bytestring of length 32)
-- or the ada symbol (empty bytestring).
genCurrencySymbol :: MonadGen m => m CurrencySymbol
genCurrencySymbol = Gen.choice
    [ Value.currencySymbol <$> genSizedByteStringExact 32
    , pure Ada.adaSymbol
    ]

genValue' :: MonadGen m => Range Integer -> m Value
genValue' valueRange = do
    let
        sngl = Value.singleton <$> genCurrencySymbol <*> genTokenName <*> Gen.integral valueRange

        -- generate values with no more than 5 elements to avoid the tests
        -- taking too long (due to the map-as-list-of-kv-pairs implementation)
        maxCurrencies = 5

    numValues <- Gen.int (Range.linear 0 maxCurrencies)
    fold <$> traverse (const sngl) [0 .. numValues]

-- | Generate a 'Value' with a value range of @minBound .. maxBound@.
genValue :: MonadGen m => m Value
genValue = genValue' $ fromIntegral <$> Range.linearBounded @Int

-- | Generate a 'Value' with a value range of @0 .. maxBound@.
genValueNonNegative :: MonadGen m => m Value
genValueNonNegative = genValue' $ fromIntegral <$> Range.linear @Int 0 maxBound

-- | Assert that a transaction is valid in a chain.
assertValid :: (MonadTest m, HasCallStack)
    => Tx
    -> Mockchain
    -> m ()
assertValid tx mc = Hedgehog.assert $ isNothing $ validateMockchain mc tx

-- | Validate a transaction in a mockchain.
validateMockchain :: Mockchain -> Tx -> Maybe Index.ValidationError
validateMockchain (Mockchain txPool _ params) tx = result where
    h      = 1
    idx    = Index.initialise [map Valid txPool]
    result = fmap snd $ fst $ fst $ Index.runValidation (Index.validateTransaction h tx) (ValidationCtx idx params)

{- | Split a value into max. n positive-valued parts such that the sum of the
     parts equals the original value. Each part should contain the required
     minimum amount of Ada.

     I noticed how for values of `mx` > 1000 the resulting lists are much smaller than
     one would expect. I think this may be caused by the way we select the next value
     for the split. It looks like the available funds get exhausted quite fast, which
     makes the function return before generating anything close to `mx` values.
-}
splitVal :: (MonadGen m, Integral n) => Int -> n -> m [n]
splitVal _  0     = pure []
splitVal mx init' = go 0 0 [] where
    go i c l =
        if i >= pred mx || init' - c < 2 * minAda
        then pure $ (init' - c) : l
        else do
            v <- Gen.integral (Range.linear minAda $ init' - c - minAda)
            if v + c == init'
            then pure $ v : l
            else go (succ i) (v + c) (v : l)
    minAda = fromIntegral $ Ada.getLovelace $ Ledger.minAdaTxOut + Ledger.maxFee

genTxInfo :: MonadGen m => Mockchain -> m TxInfo
genTxInfo chain = do
    tx <- genValidTransaction chain
    let idx = UtxoIndex $ mockchainUtxo chain
    let params = mockchainParams chain
    let (res, _) = runWriter $ runExceptT $ runReaderT (_runValidation (Index.mkTxInfo tx)) (ValidationCtx idx params)
    either (const Gen.discard) pure res

genScriptPurposeSpending :: MonadGen m => TxInfo -> m Contexts.ScriptPurpose
genScriptPurposeSpending TxInfo{txInfoInputs} = Gen.element $ Contexts.Spending . txInInfoOutRef <$> txInfoInputs

genScriptPurposeMinting :: MonadGen m => TxInfo -> m Contexts.ScriptPurpose
genScriptPurposeMinting TxInfo{txInfoMint} = Gen.element $ Contexts.Minting <$> Value.symbols txInfoMint

-- TODO: add Rewarding and Certifying purposes

genValidatorContext :: MonadGen m => Mockchain -> m ScriptContext
genValidatorContext chain = do
    txInfo <- genTxInfo chain
    purpose <- genScriptPurposeSpending txInfo
    pure $ ScriptContext txInfo purpose

genMintingPolicyContext :: MonadGen m => Mockchain -> m ScriptContext
genMintingPolicyContext chain = do
    txInfo <- genTxInfo chain
    purpose <- genScriptPurposeMinting txInfo
    pure $ ScriptContext txInfo purpose

knownPaymentPublicKeys :: [PaymentPubKey]
knownPaymentPublicKeys =
    PaymentPubKey . toPublicKey . unPaymentPrivateKey <$> knownPaymentPrivateKeys

knownPaymentPrivateKeys :: [PaymentPrivateKey]
knownPaymentPrivateKeys = CW.paymentPrivateKey <$> CW.knownMockWallets

-- | Seed suitable for testing a seed but not for actual wallets as ScrubbedBytes isn't used to ensure
--  memory isn't inspectable
genSeed :: MonadGen m => m BS.ByteString
genSeed =  Gen.bytes $ Range.singleton 32

genPassphrase :: MonadGen m => m Passphrase
genPassphrase =
  Passphrase <$> Gen.utf8 (Range.singleton 16) Gen.unicode

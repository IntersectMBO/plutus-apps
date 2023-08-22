{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Generators for constructing blockchains and transactions for use in property-based testing.
module Cardano.Node.Emulator.Generators(
    -- * Mockchain
    Mockchain(..),
    genMockchain,
    genMockchain',
    emptyChain,
    GeneratorModel(..),
    generatorModel,
    -- * Transactions
    genValidTransaction,
    genValidTransactionBody,
    genValidTransaction',
    genValidTransactionSpending,
    genValidTransactionSpending',
    genInitialTransaction,
    makeTx,
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
    failOnCardanoError,
    genPolicyId,
    genAssetId,
    Gen.genAssetName,
    genSingleton,
    genValue,
    genValueNonNegative,
    genSizedByteString,
    genSeed,
    genPassphrase,
    splitVal,
    validateMockchain,
    signAll,
    CW.knownAddresses,
    CW.knownPaymentPublicKeys,
    CW.knownPaymentPrivateKeys,
    CW.knownPaymentKeys,
    knownXPrvs,
    alwaysSucceedPolicy,
    alwaysSucceedPolicyId,
    someTokenValue,
    Tx.emptyTxBodyContent
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Node.Emulator.Internal.Node.Params (Params (pSlotConfig), testnet)
import Cardano.Node.Emulator.Internal.Node.TimeSlot (SlotConfig)
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Cardano.Node.Emulator.Internal.Node.Validation (validateCardanoTx)
import Control.Monad (guard, replicateM)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString qualified as BS
import Data.Default (Default (def), def)
import Data.Foldable (fold, foldl')
import Data.Functor (($>))
import Data.List (sort)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Gen.Cardano.Api.Typed qualified as Gen
import GHC.Stack (HasCallStack)
import Hedgehog (Gen, MonadGen, MonadTest, Range)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (CardanoTx (CardanoEmulatorEraTx), Interval, MintingPolicy (getMintingPolicy),
               POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange, Passphrase (Passphrase),
               PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey, Slot (Slot), SlotRange, TxOut,
               ValidationErrorInPhase, ValidationPhase (Phase1, Phase2), ValidationResult (FailPhase1, FailPhase2),
               addCardanoTxSignature, createGenesisTransaction, minLovelaceTxOutEstimated, pubKeyAddress, txOutValue)
import Ledger.CardanoWallet qualified as CW
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (ToCardanoError, fromCardanoPlutusScript)
import Ledger.Tx.CardanoAPI qualified as C hiding (makeTransactionBody)
import Ledger.Value.CardanoAPI qualified as Value
import Numeric.Natural (Natural)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Scripts qualified as Script
import PlutusTx (toData)

-- | Attach signatures of all known private keys to a transaction.
signAll :: CardanoTx -> CardanoTx
signAll tx = foldl' (flip addCardanoTxSignature) tx
           $ fmap unPaymentPrivateKey CW.knownPaymentPrivateKeys

-- | The parameters for the generators in this module.
data GeneratorModel = GeneratorModel {
    gmInitialBalance      :: !(Map PaymentPubKey C.Lovelace),
    -- ^ Value created at the beginning of the blockchain.
    gmPubKeys             :: !(Set PaymentPubKey),
    -- ^ Public keys that are to be used for generating transactions.
    gmMaxCollateralInputs :: !(Maybe Natural)
    } deriving Show

-- | A generator model with some sensible defaults.
generatorModel :: GeneratorModel
generatorModel =
    let vl = C.Lovelace $ 1_000_000 * 100
        pubKeys = CW.knownPaymentPublicKeys

    in
    GeneratorModel
    { gmInitialBalance = Map.fromList $ zip pubKeys (repeat vl)
    , gmPubKeys        = Set.fromList pubKeys
    , gmMaxCollateralInputs = C.protocolParamMaxCollateralInputs def
    }

-- | Blockchain for testing the emulator implementation and traces.
--
--   To avoid having to rely on functions from the implementation of
--   plutus-ledger (in particular, 'Ledger.Tx.unspentOutputs') we note the
--   unspent outputs of the chain when it is first created.
data Mockchain = Mockchain {
    mockchainInitialTxPool :: [CardanoTx],
    mockchainUtxo          :: Map C.TxIn TxOut,
    mockchainParams        :: Params
    } deriving Show

-- | The empty mockchain.
emptyChain :: Mockchain
emptyChain = Mockchain [] Map.empty def

-- | Generate a mockchain.
--
--   TODO: Generate more than 1 txn
genMockchain' ::
       GeneratorModel
    -> Gen Mockchain
genMockchain' gm = do
    slotCfg <- genSlotConfig
    (txn, ot) <- genInitialTransaction gm
    let params = def { pSlotConfig = slotCfg }
        -- There is a problem that txId of emulator tx and tx of cardano tx are different.
        -- We convert the emulator tx to cardano tx here to get the correct transaction id
        -- because later we anyway will use the converted cardano tx so the utxo should match it.
        tid = Tx.getCardanoTxId txn
    pure Mockchain {
        mockchainInitialTxPool = [txn],
        mockchainUtxo = Map.fromList $ first (C.TxIn tid . C.TxIx) <$> zip [0..] ot,
        mockchainParams = params
        }

-- | Generate a mockchain using the default 'GeneratorModel'.
--
genMockchain :: Gen Mockchain
genMockchain = genMockchain' generatorModel

-- | A transaction with no inputs that mints some value (to be used at the
--   beginning of a blockchain).
genInitialTransaction ::
       GeneratorModel
    -> Gen (CardanoTx, [TxOut])
genInitialTransaction GeneratorModel{..} = do
    let pkAddr pk = either (error . show) id $ C.toCardanoAddressInEra testnet $ pubKeyAddress pk Nothing
        initialDist = Map.mapKeys pkAddr $ fmap Value.lovelaceToValue gmInitialBalance
    let tx@(CardanoEmulatorEraTx (C.Tx (C.TxBody txBodyContent) _)) = createGenesisTransaction initialDist
        txOuts = Tx.TxOut <$> C.txOuts txBodyContent
    pure (tx, txOuts)

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the minimum fee.
genValidTransaction
    :: Mockchain
    -> Gen CardanoTx
genValidTransaction = genValidTransaction' generatorModel

genValidTransactionBody
    :: Mockchain
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genValidTransactionBody = genValidTransactionBody' generatorModel

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the estimated fee.
genValidTransaction'
    :: GeneratorModel
    -> Mockchain
    -> Gen CardanoTx
genValidTransaction' g chain = genValidTransactionBody' g chain >>= makeTx

genValidTransactionSpending
    :: [C.TxIn]
    -> C.Value
    -> Gen CardanoTx
genValidTransactionSpending = genValidTransactionSpending' generatorModel

genValidTransactionSpending'
    :: GeneratorModel
    -> [C.TxIn]
    -> C.Value
    -> Gen CardanoTx
genValidTransactionSpending' g ins totalVal =
    genValidTransactionBodySpending' g ins totalVal >>= makeTx


makeTx
    :: MonadFail m
    => C.TxBodyContent C.BuildTx C.BabbageEra
    -> m CardanoTx
makeTx bodyContent = do
    txBody <- either (fail . ("makeTx: Can't create TxBody: " <>) . show) pure $ C.makeTransactionBody bodyContent
    pure $ signAll $ CardanoEmulatorEraTx $ C.Tx txBody []

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the estimated fee.
genValidTransactionBody'
    :: GeneratorModel
    -> Mockchain
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genValidTransactionBody' g (Mockchain _ ops _) = do
    -- Take a random number of UTXO from the input
    nUtxo <- if Map.null ops
                then Gen.discard
                else Gen.int (Range.linear 1 (Map.size ops))
    let ins = fst <$> inUTXO
        inUTXO = take nUtxo $ Map.toList ops
        totalVal = foldMap (txOutValue . snd) inUTXO
    genValidTransactionBodySpending' g ins totalVal

genValidTransactionBodySpending'
    :: GeneratorModel
    -> [C.TxIn]
    -> C.Value
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genValidTransactionBodySpending' g ins totalVal = do
    mintAmount <- toInteger <$> Gen.int (Range.linear 0 maxBound)
    mintTokenName <- Gen.genAssetName
    let mintValue = guard (mintAmount == 0) $> someTokenValue mintTokenName mintAmount
        fee' = C.Lovelace 300000
        numOut = Set.size (gmPubKeys g) - 1
        totalValAda = C.selectLovelace totalVal
        totalValTokens = guard (Value.isZero (Value.noAdaValue totalVal)) $> Value.noAdaValue totalVal
        canPayTheFees = fee' < totalValAda
    guard canPayTheFees
    -- We only split the Ada part of the input value
    splitOutVals <- splitVal numOut (totalValAda - fee')
    let outVals = case totalValTokens <> mintValue of
            Nothing -> Value.lovelaceToValue <$> splitOutVals
            Just mv -> do
                -- If there is a minted value, we look for a value in the
                -- splitted values which can be associated with it.
                let outValForMint =
                        fromMaybe mempty $ List.find (>= Ledger.minLovelaceTxOutEstimated)
                                         $ List.sort splitOutVals
                Value.lovelaceToValue outValForMint
                    <> mv : fmap Value.lovelaceToValue (List.delete outValForMint splitOutVals)
    pubKeys <- Gen.shuffle $ Set.toList $ gmPubKeys g
    let txOutputs = either (fail . ("Cannot create outputs: " <>) . show) id
                    $ traverse (\(v, ppk) -> pubKeyTxOut v ppk Nothing)
                    $ zip outVals pubKeys
    mintWitness <- failOnCardanoError $ C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
                           <$> (C.PScript <$> C.toCardanoPlutusScript
                                                  (C.AsPlutusScript C.AsPlutusScriptV2)
                                                  (getMintingPolicy alwaysSucceedPolicy))
                           <*> pure C.NoScriptDatumForMint
                           <*> pure (C.fromPlutusData $ toData Script.unitRedeemer)
                           <*> pure C.zeroExecutionUnits
    let txMintValue = C.TxMintValue C.MultiAssetInBabbageEra (fromMaybe mempty mintValue)
                          (C.BuildTxWith (Map.singleton alwaysSucceedPolicyId mintWitness))
        txIns = map (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) ins
    txInsCollateral <- maybe
        (fail "Cannot gen collateral")
        (pure . C.TxInsCollateral C.CollateralInBabbageEra . flip take ins . fromIntegral)
        (gmMaxCollateralInputs g)
    pure $ Tx.emptyTxBodyContent
           { C.txIns
           , C.txInsCollateral
           , C.txMintValue
           , C.txFee = C.toCardanoFee fee'
           , C.txOuts = Tx.getTxOut <$> txOutputs
           }

-- | Create a transaction output locked by a public payment key and optionnaly a public stake key.
pubKeyTxOut :: C.Value -> PaymentPubKey -> Maybe V1.StakingCredential -> Either ToCardanoError TxOut
pubKeyTxOut v pk sk = do
  aie <- C.toCardanoAddressInEra testnet $ pubKeyAddress pk sk
  pure $ Tx.TxOut $ C.TxOut aie (C.toCardanoTxOutValue v) C.TxOutDatumNone C.ReferenceScriptNone

-- | Validate a transaction in a mockchain.
validateMockchain :: Mockchain -> CardanoTx -> Maybe Ledger.ValidationErrorInPhase
validateMockchain (Mockchain _ utxo params) tx = result where
    cUtxoIndex = C.UTxO $ Tx.toCtxUTxOTxOut <$> utxo
    result = case validateCardanoTx params 1 cUtxoIndex tx of
        FailPhase1 _ err   -> Just (Phase1, err)
        FailPhase2 _ err _ -> Just (Phase2, err)
        _                  -> Nothing

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

-- | Generate a 'ByteString s' of up to @s@ bytes.
genSizedByteString :: forall m. MonadGen m => Int -> m BS.ByteString
genSizedByteString s =
    let range = Range.linear 0 s
    in Gen.bytes range

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genPolicyId :: Gen C.PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, C.PolicyId <$> Gen.genScriptHash)
    ]

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genAssetId :: Gen C.AssetId
genAssetId = Gen.choice
    [ C.AssetId <$> genPolicyId <*> Gen.genAssetName
    , return C.AdaAssetId
    ]

genSingleton :: Range Integer -> Gen C.Value
genSingleton range = Value.assetIdValue <$> genAssetId <*> Gen.integral range

genValue' :: Range Integer -> Gen C.Value
genValue' valueRange = do
    let
        -- generate values with no more than 5 elements to avoid the tests
        -- taking too long (due to the map-as-list-of-kv-pairs implementation)
        maxCurrencies = 5

    numValues <- Gen.int (Range.linear 0 maxCurrencies)
    fold <$> traverse (const $ genSingleton valueRange) [0 .. numValues]

-- | Generate a 'Value' with a value range of @minBound .. maxBound@.
genValue :: Gen C.Value
genValue = genValue' $ fromIntegral <$> Range.linearBounded @Int

-- | Generate a 'Value' with a value range of @0 .. maxBound@.
genValueNonNegative :: Gen C.Value
genValueNonNegative = genValue' $ fromIntegral <$> Range.linear @Int 0 maxBound

-- | Assert that a transaction is valid in a chain.
assertValid :: (MonadTest m, HasCallStack)
    => CardanoTx
    -> Mockchain
    -> m ()
assertValid tx mc = let res = validateMockchain mc tx in do
    H.annotateShow res
    H.assert $ isNothing res

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
    minAda = 3_000_000 -- For fee and min Ada for tx outs

knownXPrvs :: [Crypto.XPrv]
knownXPrvs = unPaymentPrivateKey <$> CW.knownPaymentPrivateKeys

-- | Seed suitable for testing a seed but not for actual wallets as ScrubbedBytes isn't used to ensure
--  memory isn't inspectable
genSeed :: MonadGen m => m BS.ByteString
genSeed =  Gen.bytes $ Range.singleton 32

genPassphrase :: MonadGen m => m Passphrase
genPassphrase =
  Passphrase <$> Gen.utf8 (Range.singleton 16) Gen.unicode

alwaysSucceedPolicy :: Script.MintingPolicy
alwaysSucceedPolicy = Script.MintingPolicy (fromCardanoPlutusScript $ C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint)

alwaysSucceedPolicyId :: C.PolicyId
alwaysSucceedPolicyId = C.scriptPolicyId (C.PlutusScript C.PlutusScriptV1 $ C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint)

someTokenValue :: C.AssetName -> Integer -> C.Value
someTokenValue an i = C.valueFromList [(C.AssetId alwaysSucceedPolicyId an, C.Quantity i)]

-- | Catch cardano error and fail wi it
failOnCardanoError :: MonadFail m => Either C.ToCardanoError a -> m a
failOnCardanoError = either (fail . show) pure

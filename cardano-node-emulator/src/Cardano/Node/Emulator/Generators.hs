{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    TxInputWitnessed(..),
    generatorModel,
    -- * Transactions
    genValidTransaction,
    genValidTransaction',
    genValidTransactionSpending,
    genValidTransactionSpending',
    genInitialTransaction,
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
    genPolicyId,
    genAssetId,
    Gen.genAssetName,
    genSingleton,
    genValue,
    genValueNonNegative,
    genSizedByteString,
    genSizedByteStringExact,
    genSeed,
    genPassphrase,
    splitVal,
    validateMockchain,
    signAll,
    signTx,
    CW.knownAddresses,
    CW.knownPaymentPublicKeys,
    CW.knownPaymentPrivateKeys,
    CW.knownPaymentKeys,
    knownXPrvs,
    alwaysSucceedPolicyId,
    someTokenValue
    ) where

import Control.Lens ((&))
import Control.Monad (replicateM)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.ByteString qualified as BS
import Data.Default (Default (def), def)
import Data.Either.Combinators (leftToMaybe)
import Data.Foldable (fold, foldl')
import Data.Functor.Identity (Identity)
import Data.List (sort)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters (..))
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Node.Emulator.Params (Params (pSlotConfig))
import Cardano.Node.Emulator.TimeSlot (SlotConfig)
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Cardano.Node.Emulator.Validation (fromPlutusTxSigned, validateCardanoTx)
import Data.String (fromString)
import Ledger (CardanoTx (EmulatorTx), Datum, Interval, Language (PlutusV1), POSIXTime (POSIXTime, getPOSIXTime),
               POSIXTimeRange, Passphrase (Passphrase), PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey,
               Slot (Slot), SlotRange, SomeCardanoApiTx (SomeTx),
               Tx (txCollateralInputs, txFee, txInputs, txMint, txOutputs, txValidRange),
               TxInType (ConsumePublicKeyAddress, ConsumeSimpleScriptAddress, ScriptAddress), TxInput (TxInput),
               TxInputType (TxConsumePublicKeyAddress, TxConsumeSimpleScriptAddress, TxScriptAddress), TxOut,
               TxOutRef (TxOutRef), ValidationErrorInPhase, Validator, Versioned, addCardanoTxSignature,
               addMintingPolicy, getValidator, maxFee, minAdaTxOutEstimated, minLovelaceTxOutEstimated, pubKeyTxOut,
               scriptHash, txData, txOutValue, txScripts, validatorHash)
import Ledger.CardanoWallet qualified as CW
import Ledger.Index.Internal qualified as Index (UtxoIndex (UtxoIndex))
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (fromCardanoPlutusScript, fromPlutusIndex)
import Ledger.Value.CardanoAPI qualified as Value
import Numeric.Natural (Natural)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Scripts (Versioned (Versioned), datumHash)
import Plutus.Script.Utils.V1.Generators qualified as ScriptGen
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts qualified as Script

-- | Attach signatures of all known private keys to a transaction.
signAll :: CardanoTx -> CardanoTx
signAll tx = foldl' (flip addCardanoTxSignature) tx
           $ fmap unPaymentPrivateKey CW.knownPaymentPrivateKeys

-- | The parameters for the generators in this module.
data GeneratorModel = GeneratorModel {
    gmInitialBalance      :: Map PaymentPubKey C.Value,
    -- ^ Value created at the beginning of the blockchain.
    gmPubKeys             :: Set PaymentPubKey,
    -- ^ Public keys that are to be used for generating transactions.
    gmMaxCollateralInputs :: Maybe Natural
    } deriving Show

-- | A generator model with some sensible defaults.
generatorModel :: GeneratorModel
generatorModel =
    let vl = Value.adaValueOf 100
        pubKeys = CW.knownPaymentPublicKeys

    in
    GeneratorModel
    { gmInitialBalance = Map.fromList $ zip pubKeys (repeat vl)
    , gmPubKeys        = Set.fromList pubKeys
    , gmMaxCollateralInputs = protocolParamMaxCollateralInputs def
    }

-- | Blockchain for testing the emulator implementation and traces.
--
--   To avoid having to rely on functions from the implementation of
--   plutus-ledger (in particular, 'Ledger.Tx.unspentOutputs') we note the
--   unspent outputs of the chain when it is first created.
data Mockchain = Mockchain {
    mockchainInitialTxPool :: [CardanoTx],
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
    slotCfg <- genSlotConfig
    let (txn, ot) = genInitialTransaction gm
        params = def { pSlotConfig = slotCfg }
        signedTx = signTx params mempty txn
        -- There is a problem that txId of emulator tx and tx of cardano tx are different.
        -- We convert the emulator tx to cardano tx here to get the correct transaction id
        -- because later we anyway will use the converted cardano tx so the utxo should match it.
        tid = Tx.getCardanoTxId signedTx
    pure Mockchain {
        mockchainInitialTxPool = [txn],
        mockchainUtxo = Map.fromList $ first (TxOutRef tid) <$> zip [0..] ot,
        mockchainParams = params
        }

-- | Generate a mockchain using the default 'GeneratorModel'.
--
genMockchain :: MonadGen m => m Mockchain
genMockchain = genMockchain' generatorModel

-- | A transaction with no inputs that mints some value (to be used at the
--   beginning of a blockchain).
genInitialTransaction ::
       GeneratorModel
    -> (CardanoTx, [TxOut])
genInitialTransaction GeneratorModel{..} =
    let o = either (error . ("Cannot create outputs: " <>) . show) id
          $ traverse (\(ppk, v) -> pubKeyTxOut v ppk Nothing) $ Map.toList gmInitialBalance
        t = fold gmInitialBalance
    in (EmulatorTx $ mempty {
        txOutputs = o,
        txMint = t,
        txValidRange = Interval.from 0
        }, o)

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the minimum fee.
genValidTransaction
    :: Mockchain
    -> Gen CardanoTx
genValidTransaction = genValidTransaction' generatorModel

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the estimated fee.
genValidTransaction'
    :: GeneratorModel
    -> Mockchain
    -> Gen CardanoTx
genValidTransaction' g (Mockchain _ ops _) = do
    -- Take a random number of UTXO from the input
    nUtxo <- if Map.null ops
                then Gen.discard
                else Gen.int (Range.linear 1 (Map.size ops))
    let ins = (`TxInputWitnessed` ConsumePublicKeyAddress) . fst <$> inUTXO
        inUTXO = take nUtxo $ Map.toList ops
        totalVal = foldl' (<>) mempty $ map (txOutValue . snd) inUTXO
    genValidTransactionSpending' g ins totalVal

genValidTransactionSpending
    :: [TxInputWitnessed]
    -> C.Value
    -> Gen CardanoTx
genValidTransactionSpending = genValidTransactionSpending' generatorModel

-- | A transaction input, consisting of a transaction output reference and an input type with data witnesses.
data TxInputWitnessed = TxInputWitnessed !TxOutRef !Ledger.TxInType

genValidTransactionSpending'
    :: GeneratorModel
    -> [TxInputWitnessed]
    -> C.Value
    -> Gen CardanoTx
genValidTransactionSpending' g ins totalVal = do
    mintAmount <- toInteger <$> Gen.int (Range.linear 0 maxBound)
    mintTokenName <- Gen.genAssetName
    let mintValue = if mintAmount == 0
                       then Nothing
                       else Just $ someTokenValue mintTokenName mintAmount
        fee' = C.Lovelace 300000
        numOut = Set.size (gmPubKeys g) - 1
        totalValAda = C.selectLovelace totalVal
        totalValTokens = if Value.isZero (Value.noAdaValue totalVal) then Nothing else Just $ Value.noAdaValue totalVal
    if fee' < totalValAda
        then do
            -- We only split the Ada part of the input value
            splitOutVals <- splitVal numOut (totalValAda - fee')
            let outVals = case totalValTokens <> mintValue of
                  Nothing -> do
                    fmap Value.lovelaceToValue splitOutVals
                  Just mv -> do
                    -- If there is a minted value, we look for a value in the
                    -- splitted values which can be associated with it.
                    let outValForMint =
                          maybe mempty id $ List.find (\v -> v >= Ledger.minLovelaceTxOutEstimated)
                                          $ List.sort splitOutVals
                    Value.lovelaceToValue outValForMint <> mv : fmap Value.lovelaceToValue (List.delete outValForMint splitOutVals)
                txOutputs = either (error . ("Cannot create outputs: " <>) . show) id
                          $ traverse (\(v, ppk) -> pubKeyTxOut v ppk Nothing) $ zip outVals (Set.toList $ gmPubKeys g)
                (ins', witnesses) = unzip $ map txInToTxInput ins
                (scripts, datums) = bimap catMaybes catMaybes $ unzip witnesses
                tx = mempty
                        { txInputs = ins'
                        , txCollateralInputs = maybe [] (flip take ins' . fromIntegral) (gmMaxCollateralInputs g)
                        , txOutputs = txOutputs
                        , txMint = maybe mempty id mintValue
                        , txFee = fee'
                        , txData = Map.fromList (map (\d -> (datumHash d, d)) datums)
                        , txScripts = Map.fromList (map ((\s -> (scriptHash s, s)) . fmap getValidator) scripts)
                        }
                    & addMintingPolicy (Versioned alwaysSucceedPolicy PlutusV1) (Script.unitRedeemer, Nothing)
                    & EmulatorTx

                -- sign the transaction with all known wallets
                -- this is somewhat crude (but technically valid)
            pure (signAll tx)
        else Gen.discard

    where
        -- | Translate TxIn to TxInput taking out data witnesses if present.
        txInToTxInput :: TxInputWitnessed -> (TxInput, (Maybe (Versioned Validator), Maybe Datum))
        txInToTxInput (TxInputWitnessed outref txInType) = case txInType of
            Ledger.ConsumePublicKeyAddress -> (TxInput outref TxConsumePublicKeyAddress, (Nothing, Nothing))
            Ledger.ConsumeSimpleScriptAddress -> (TxInput outref Ledger.TxConsumeSimpleScriptAddress, (Nothing, Nothing))
            Ledger.ScriptAddress (Left vl) rd dt ->
                (TxInput outref (Ledger.TxScriptAddress rd (Left $ validatorHash vl) (fmap datumHash dt)), (Just vl, dt))
            Ledger.ScriptAddress (Right ref) rd dt ->
                (TxInput outref (Ledger.TxScriptAddress rd (Right ref) (fmap datumHash dt)), (Nothing, dt))

signTx :: Params -> Map TxOutRef TxOut -> CardanoTx -> CardanoTx
signTx params utxo = let
  cUtxoIndex = either (error . show) id $ fromPlutusIndex (Index.UtxoIndex utxo)
  in Tx.onCardanoTx
      (\t -> fromPlutusTxSigned params cUtxoIndex t CW.knownPaymentKeys)
      Tx.CardanoApiTx

-- | Validate a transaction in a mockchain.
validateMockchain :: Mockchain -> CardanoTx -> Maybe Ledger.ValidationErrorInPhase
validateMockchain (Mockchain _ utxo params) tx = result where
    cUtxoIndex = either (error . show) id $ fromPlutusIndex (Index.UtxoIndex utxo)
    result = leftToMaybe $ validateCardanoTx params 1 cUtxoIndex (signTx params utxo tx)

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
                                 , genBabbageEraInCardanoModeTx
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

genBabbageEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genBabbageEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.BabbageEra
  pure $ SomeTx tx C.BabbageEraInCardanoMode

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
    Hedgehog.annotateShow res
    Hedgehog.assert $ isNothing res

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
    minAda = fromIntegral $ Ada.getLovelace $ Ledger.minAdaTxOutEstimated + Ledger.maxFee

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

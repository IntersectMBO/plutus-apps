{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An index of unspent transaction outputs, and some functions for validating
--   transactions using the index.
module Ledger.Index(
    -- * Types for transaction validation based on UTXO index
    ValidationMonad,
    ValidationCtx(..),
    UtxoIndex(..),
    insert,
    insertCollateral,
    insertBlock,
    initialise,
    Validation(..),
    runValidation,
    lookup,
    lkpValue,
    lkpTxOut,
    lkpOutputs,
    ValidationError(..),
    ValidationErrorInPhase,
    ValidationPhase(..),
    InOutMatch(..),
    minFee,
    maxFee,
    minAdaTxOut,
    minLovelaceTxOut,
    mkPV1TxInfo,
    mkPV2TxInfo,
    pubKeyTxIns,
    scriptTxIns,
    maxMinAdaTxOut,
    -- * Actual validation
    validateTransaction,
    validateTransactionOffChain,
    checkValidInputs,
    -- * Script validation events
    ScriptType(..),
    ScriptValidationEvent(..),
    PV1.ExBudget(..),
    PV1.ExCPU(..),
    PV1.ExMemory(..),
    PV1.SatInt,
    ValidatorMode(..),
    getScript
    ) where

import Prelude hiding (lookup)

import Cardano.Api (Lovelace (..))
import Control.Lens (Fold, folding, toListOf, view, (^.))
import Control.Lens.Indexed (iforM_)
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), runExcept, runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), ask)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Either (fromRight)
import Data.Foldable (asum, fold, foldl', for_, traverse_)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Ada (Ada)
import Ledger.Ada qualified as Ada
import Ledger.Blockchain
import Ledger.Crypto
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Params (Params (pSlotConfig))
import Ledger.Slot qualified as Slot
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx hiding (pubKeyTxIns, scriptTxIns)
import Ledger.Validation (evaluateMinLovelaceOutput, fromPlutusTxOutUnsafe)
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value qualified as V
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx (toBuiltinData)
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Numeric qualified as P

-- | Context for validating transactions. We need access to the unspent
--   transaction outputs of the blockchain, and we can throw 'ValidationError's.
type ValidationMonad m = (MonadReader ValidationCtx m, MonadError ValidationError m, MonadWriter [ScriptValidationEvent] m)

data ValidationCtx = ValidationCtx { vctxIndex :: UtxoIndex, vctxParams :: Params }

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = UtxoIndex . unspentOutputs

-- | Update the index for the addition of a transaction.
insert :: CardanoTx -> UtxoIndex -> UtxoIndex
insert tx = UtxoIndex . updateUtxo tx . getIndex

-- | Update the index for the addition of only the collateral inputs of a failed transaction.
insertCollateral :: CardanoTx -> UtxoIndex -> UtxoIndex
insertCollateral tx = UtxoIndex . updateUtxoCollateral tx . getIndex

-- | Update the index for the addition of a block.
insertBlock :: Block -> UtxoIndex -> UtxoIndex
insertBlock blck i = foldl' (flip (eitherTx insertCollateral insert)) i blck

-- | Find an unspent transaction output by the 'TxOutRef' that spends it.
lookup :: MonadError ValidationError m => TxOutRef -> UtxoIndex -> m TxOut
lookup i index = case Map.lookup i $ getIndex index of
    Just t  -> pure t
    Nothing -> throwError $ TxOutRefNotFound i

-- | A monad for running transaction validation inside, which is an instance of 'ValidationMonad'.
newtype Validation a = Validation { _runValidation :: (ReaderT ValidationCtx (ExceptT ValidationError (Writer [ScriptValidationEvent]))) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader ValidationCtx, MonadError ValidationError, MonadWriter [ScriptValidationEvent])

-- | Run a 'Validation' on a 'UtxoIndex'.
runValidation :: Validation (Maybe ValidationErrorInPhase) -> ValidationCtx -> (Maybe ValidationErrorInPhase, [ScriptValidationEvent])
runValidation l ctx = runWriter $ fmap (either (\e -> Just (Phase1, e)) id) $ runExceptT $ runReaderT (_runValidation l) ctx

-- | Determine the unspent value that a ''TxOutRef' refers to.
lkpValue :: ValidationMonad m => TxOutRef -> m V.Value
lkpValue = fmap txOutValue . lkpTxOut

-- | Find an unspent transaction output by its reference. Assumes that the
--   output for this reference exists. If you want to handle the lookup error
--   you can use 'runLookup'.
lkpTxOut :: ValidationMonad m => TxOutRef -> m TxOut
lkpTxOut t = lookup t . vctxIndex =<< ask

-- | Filter to get only the script inputs.
scriptTxIns :: Fold [TxIn] TxIn
scriptTxIns = (\x -> folding x) . filter $ \case
    TxIn{ txInType = Just ConsumeScriptAddress{} } -> True
    _                                              -> False

-- | Filter to get only the pubkey inputs.
pubKeyTxIns :: Fold [TxIn] TxIn
pubKeyTxIns = folding (filter (\TxIn{ txInType = t } -> t == Just ConsumePublicKeyAddress))


-- | Validate a transaction in a 'ValidationMonad' context.
validateTransaction :: ValidationMonad m
    => Slot.Slot
    -> Tx
    -> m (Maybe ValidationErrorInPhase)
validateTransaction h t = do
    -- Phase 1 validation
    checkSlotRange h t
    _ <- lkpOutputs $ toListOf (inputs . scriptTxIns) t

    -- see note [Minting of Ada]
    emptyUtxoSet <- reader (Map.null . getIndex . vctxIndex)
    unless emptyUtxoSet (checkTransactionFee t)

    validateTransactionOffChain t

validateTransactionOffChain :: ValidationMonad m
    => Tx
    -> m (Maybe ValidationErrorInPhase)
validateTransactionOffChain t = do
    checkValuePreserved t
    checkPositiveValues t
    checkMinAdaInTxOutputs t
    checkFeeIsAda t

    -- see note [Minting of Ada]
    emptyUtxoSet <- reader (Map.null . getIndex . vctxIndex)
    unless emptyUtxoSet (checkMintingAuthorised t)

    checkValidInputs (toListOf (inputs . pubKeyTxIns)) t
    checkValidInputs (view collateralInputs) t

    (do
        -- Phase 2 validation
        checkValidInputs (toListOf (inputs . scriptTxIns)) t
        unless emptyUtxoSet (checkMintingScripts t)

        pure Nothing
        ) `catchError` (\e -> pure (Just (Phase2, e)))

-- | Check that a transaction can be validated in the given slot.
checkSlotRange :: ValidationMonad m => Slot.Slot -> Tx -> m ()
checkSlotRange sl tx =
    if Interval.member sl (txValidRange tx)
    then pure ()
    else throwError $ CurrentSlotOutOfRange sl

-- | Check if the inputs of the transaction consume outputs that exist, and
--   can be unlocked by the signatures or validator scripts of the inputs.
checkValidInputs :: ValidationMonad m => (Tx -> [TxIn]) -> Tx -> m ()
checkValidInputs getInputs tx = do
    let tid = txId tx
        sigs = tx ^. signatures
    outs <- lkpOutputs (getInputs tx)
    matches <- traverse (uncurry (matchInputOutput tid sigs)) outs
    traverse_ (checkMatch tx) matches

-- | Match each input of the transaction with the output that it spends.
lkpOutputs :: ValidationMonad m => [TxIn] -> m [(TxIn, TxOut)]
lkpOutputs = traverse (\t -> traverse (lkpTxOut . txInRef) (t, t))

{- note [Minting of Ada]

'checkMintingAuthorised' will never allow a transaction that mints Ada.
Ada's currency symbol is the empty bytestring, and it can never be matched by a
validator script whose hash is its symbol.

Therefore 'checkMintingAuthorised' should not be applied to the first transaction in
the blockchain.

-}

-- | Check whether each currency minted by the transaction is matched by
--   a corresponding minting policy script (in the form of a pay-to-script
--   output of the currency's address).
--
checkMintingAuthorised :: ValidationMonad m => Tx -> m ()
checkMintingAuthorised tx =
    let
        -- See note [Mint and Fee fields must have ada symbol].
        mintedCurrencies = filter ((/=) Ada.adaSymbol) $ V.symbols (txMint tx)

        mpsScriptHashes = Scripts.MintingPolicyHash . V.unCurrencySymbol <$> mintedCurrencies

        lockingScripts = Map.keys $ txMintScripts tx

        mintedWithoutScript = filter (\c -> c `notElem` lockingScripts) mpsScriptHashes
    in
        traverse_ (throwError . MintWithoutScript) mintedWithoutScript

-- TODO Needs to be change to support V2 minting policy scripts.
-- For now, this function runs the minting policy script with a V1
-- ScriptContext. However, transactions can contain V1 AND V2 scripts, so we
-- need to handle both.
checkMintingScripts :: forall m . ValidationMonad m => Tx -> m ()
checkMintingScripts tx = do
    txinfo <- mkPV1TxInfo tx
    iforM_ (Map.toList (txMintScripts tx)) $ \i (mph, mp) -> do
        let cs :: V.CurrencySymbol
            cs = V.mpsSymbol mph
            ctx :: Context
            ctx = Context $ toBuiltinData $ PV1.ScriptContext { PV1.scriptContextPurpose = PV1.Minting cs, PV1.scriptContextTxInfo = txinfo }
            ptr :: RedeemerPtr
            ptr = RedeemerPtr Mint (fromIntegral i)
        red <- case lookupRedeemer tx ptr of
            Just r  -> pure r
            Nothing -> throwError $ MissingRedeemer ptr

        case runExcept $ runMintingPolicyScript ctx mp red of
            Left e  -> do
                tell [mpsValidationEvent ctx mp red (Left e)]
                throwError $ ScriptFailure e
            res -> tell [mpsValidationEvent ctx mp red res]

-- | A matching pair of transaction input and transaction output, ensuring that they are of matching types also.
data InOutMatch =
    ScriptMatch
        Language
        TxOutRef
        Validator
        Redeemer
        Datum
    | PubKeyMatch TxId PubKey Signature
    deriving (Eq, Ord, Show)

-- | Match a transaction input with the output that it consumes, ensuring that
--   both are of the same type (pubkey or pay-to-script).
matchInputOutput :: ValidationMonad m
    => TxId
    -- ^ Hash of the transaction that is being verified
    -> Map.Map PubKey Signature
    -- ^ Signatures provided with the transaction
    -> TxIn
    -- ^ Input that allegedly spends the output
    -> TxOut
    -- ^ The unspent transaction output we are trying to unlock
    -> m InOutMatch
matchInputOutput txid mp txin txo = case (txInType txin, txOutDatumHash txo, txOutAddress txo) of
    (Just (ConsumeScriptAddress lang v r d), Just dh, Address{addressCredential=ScriptCredential vh}) -> do
        unless (datumHash d == dh) $ throwError $ InvalidDatumHash d dh
        case lang of
          PlutusV1 ->
              unless (PV1.validatorHash v == vh) $ throwError $ InvalidScriptHash v vh
          PlutusV2 ->
              unless (PV2.validatorHash v == vh) $ throwError $ InvalidScriptHash v vh

        pure $ ScriptMatch lang (txInRef txin) v r d
    (Just ConsumePublicKeyAddress, _, Address{addressCredential=PubKeyCredential pkh}) ->
        let sigMatches = flip fmap (Map.toList mp) $ \(pk,sig) ->
                if pubKeyHash pk == pkh
                then Just (PubKeyMatch txid pk sig)
                else Nothing
        in case asum sigMatches of
            Just m  -> pure m
            Nothing -> throwError $ SignatureMissing pkh
    _ -> throwError $ InOutTypeMismatch txin txo

-- | Check that a matching pair of transaction input and transaction output is
--   valid. If this is a pay-to-script output then the script hash needs to be
--   correct and script evaluation has to terminate successfully. If this is a
--   pay-to-pubkey output then the signature needs to match the public key that
--   locks it.
checkMatch :: ValidationMonad m => Tx -> InOutMatch -> m ()
checkMatch tx = \case
    ScriptMatch PlutusV1 txOutRef vl r d -> do
        txInfo <- mkPV1TxInfo tx
        let
            ptx' = PV1.ScriptContext { PV1.scriptContextTxInfo = txInfo, PV1.scriptContextPurpose = PV1.Spending txOutRef }
            vd = Context (toBuiltinData ptx')
        case runExcept $ runScript vd vl d r of
            Left e -> do
                tell [validatorScriptValidationEvent vd vl d r (Left e)]
                throwError $ ScriptFailure e
            res -> tell [validatorScriptValidationEvent vd vl d r res]
    ScriptMatch PlutusV2 txOutRef vl r d -> do
        txInfo <- mkPV2TxInfo tx
        let
            ptx' = PV2.ScriptContext { PV2.scriptContextTxInfo = txInfo, PV2.scriptContextPurpose = PV2.Spending txOutRef }
            vd = Context (toBuiltinData ptx')
        case runExcept $ runScript vd vl d r of
            Left e -> do
                tell [validatorScriptValidationEvent vd vl d r (Left e)]
                throwError $ ScriptFailure e
            res -> tell [validatorScriptValidationEvent vd vl d r res]
    PubKeyMatch msg pk sig -> unless (signedBy sig pk msg) $ throwError $ InvalidSignature pk sig

-- | Check if the value produced by a transaction equals the value consumed by it.
checkValuePreserved :: ValidationMonad m => Tx -> m ()
checkValuePreserved t = do
    inVal <- (P.+) (txMint t) <$> fmap fold (traverse (lkpValue . txInRef) (view inputs t))
    let outVal = txFee t P.+ foldMap txOutValue (txOutputs t)
    if outVal == inVal
    then pure ()
    else throwError $ ValueNotPreserved inVal outVal

-- | Check if all values produced and consumed by a transaction are non-negative.
checkPositiveValues :: ValidationMonad m => Tx -> m ()
checkPositiveValues t =
    if validValuesTx t
    then pure ()
    else throwError $ NegativeValue t

{-# INLINABLE minAdaTxOut #-}
-- An estimate of the minimum required Ada for each tx output.
--
-- TODO: Should be removed.
minAdaTxOut :: Ada
minAdaTxOut = Ada.lovelaceOf minTxOut

{-# INLINABLE minTxOut #-}
minTxOut :: Integer
minTxOut = 2_000_000

{-# INLINABLE maxMinAdaTxOut #-}
{-
maxMinAdaTxOut = maxTxOutSize * coinsPerUTxOWord
coinsPerUTxOWord = 34_482
maxTxOutSize = utxoEntrySizeWithoutVal + maxValSizeInWords + dataHashSize
utxoEntrySizeWithoutVal = 27
maxValSizeInWords = 500
dataHashSize = 10

These values are partly protocol parameters-based, but since this is used in on-chain code
we want a constant to reduce code size.
-}
maxMinAdaTxOut :: Ada
maxMinAdaTxOut = Ada.lovelaceOf 18_516_834

-- Minimum required Lovelace for each tx output.
--
minLovelaceTxOut :: Lovelace
minLovelaceTxOut = Lovelace minTxOut

-- | Check if each transaction outputs produced a minimum lovelace output.
checkMinAdaInTxOutputs :: ValidationMonad m => Tx -> m ()
checkMinAdaInTxOutputs t@Tx { txOutputs } = do
    params <- vctxParams <$> ask
    for_ txOutputs $ \txOut -> do
        let
            minAdaTxOut' = fromRight minAdaTxOut $
                fromPlutusTxOutUnsafe params txOut <&> \txOut' -> evaluateMinLovelaceOutput params txOut'
        if Ada.fromValue (txOutValue txOut) >= minAdaTxOut'
            then pure ()
            else throwError $ ValueContainsLessThanMinAda t txOut (Ada.toValue minAdaTxOut')

-- | Check if the fees are paid exclusively in Ada.
checkFeeIsAda :: ValidationMonad m => Tx -> m ()
checkFeeIsAda t =
    if (Ada.toValue $ Ada.fromValue $ txFee t) == txFee t
    then pure ()
    else throwError $ NonAdaFees t

-- | Minimum transaction fee.
minFee :: Tx -> V.Value
minFee = const (Ada.lovelaceValueOf 10)

-- | TODO Should be calculated based on the maximum script size permitted on
-- the Cardano blockchain.
maxFee :: Ada
maxFee = Ada.lovelaceOf 1_000_000

-- | Check that transaction fee is bigger than the minimum fee.
--   Skip the check on the first transaction (no inputs).
checkTransactionFee :: ValidationMonad m => Tx -> m ()
checkTransactionFee tx =
    if minFee tx `V.leq` txFee tx
    then pure ()
    else throwError $ TransactionFeeTooLow (txFee tx) (minFee tx)

-- | Create the data about the transaction which will be passed to a PV1
-- validator script.
mkPV1TxInfo :: ValidationMonad m => Tx -> m PV1.TxInfo
mkPV1TxInfo tx = do
    slotCfg <- pSlotConfig . vctxParams <$> ask
    txins <- traverse mkPV1TxInInfo $ view inputs tx
    let ptx = PV1.TxInfo
            { PV1.txInfoInputs = txins
            , PV1.txInfoOutputs = txOutputs tx
            -- See note [Mint and Fee fields must have ada symbol]
            , PV1.txInfoMint = Ada.lovelaceValueOf 0 <> txMint tx
            , PV1.txInfoFee = Ada.lovelaceValueOf 0 <> txFee tx
            , PV1.txInfoDCert = [] -- DCerts not supported in emulator
            , PV1.txInfoWdrl = [] -- Withdrawals not supported in emulator
            , PV1.txInfoValidRange = TimeSlot.slotRangeToPOSIXTimeRange slotCfg $ txValidRange tx
            , PV1.txInfoSignatories = fmap pubKeyHash $ Map.keys (tx ^. signatures)
            , PV1.txInfoData = Map.toList (tx ^. datumWitnesses)
            , PV1.txInfoId = txId tx
            }
    pure ptx

-- | Create the data about a transaction input which will be passed to a
-- PlutusV1 validator script.
mkPV1TxInInfo :: ValidationMonad m => TxIn -> m PV1.TxInInfo
mkPV1TxInInfo TxIn{txInRef} = do
    txOut <- lkpTxOut txInRef
    pure $ PV1.TxInInfo{PV1.txInInfoOutRef = txInRef, PV1.txInInfoResolved=txOut}

-- | Create the data about the transaction which will be passed to a PV2
-- validator script.
mkPV2TxInfo :: ValidationMonad m => Tx -> m PV2.TxInfo
mkPV2TxInfo tx = do
    slotCfg <- pSlotConfig . vctxParams <$> ask
    txIns <- traverse mkPV2TxInInfo $ view inputs tx
    txRefIns <- traverse mkPV2TxInInfo $ view referenceInputs tx
    let ptx = PV2.TxInfo
            { PV2.txInfoInputs = txIns
            , PV2.txInfoReferenceInputs = txRefIns
            , PV2.txInfoOutputs = txOutV1ToTxOutV2 <$> txOutputs tx
            -- See note [Mint and Fee fields must have ada symbol]
            , PV2.txInfoMint = Ada.lovelaceValueOf 0 <> txMint tx
            , PV2.txInfoFee = Ada.lovelaceValueOf 0 <> txFee tx
            , PV2.txInfoDCert = [] -- DCerts not supported in emulator
            , PV2.txInfoWdrl = AMap.empty -- Withdrawals not supported in emulator
            , PV2.txInfoValidRange = TimeSlot.slotRangeToPOSIXTimeRange slotCfg $ txValidRange tx
            , PV2.txInfoSignatories = fmap pubKeyHash $ Map.keys (tx ^. signatures)
            , PV2.txInfoData = AMap.fromList $ Map.toList (tx ^. datumWitnesses)
            , PV2.txInfoRedeemers = AMap.empty -- TODO Our tx must support ScriptPurpose in redeemers
            , PV2.txInfoId = txId tx
            }
    pure ptx

-- | Create the data about a transaction input which will be passed to a
-- PlutusV2 validator script.
mkPV2TxInInfo :: ValidationMonad m => TxIn -> m PV2.TxInInfo
mkPV2TxInInfo TxIn{txInRef} = do
    txOut <- lkpTxOut txInRef
    pure $ PV2.TxInInfo txInRef (txOutV1ToTxOutV2 txOut)

-- Temporary. Might not exist anymore once we remove our custom ledger rules
txOutV1ToTxOutV2 :: PV1.TxOut -> PV2.TxOut
txOutV1ToTxOutV2 (PV1.TxOut address val datum) =
    let v2Datum = maybe PV2.NoOutputDatum PV2.OutputDatumHash datum
     in PV2.TxOut address val v2Datum Nothing

data ScriptType = ValidatorScript Validator Datum | MintingPolicyScript MintingPolicy
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A script (MPS or validator) that was run during transaction validation
data ScriptValidationEvent =
    ScriptValidationEvent
        { sveScript   :: Script -- ^ The script applied to all arguments
        , sveResult   :: Either ScriptError (PV1.ExBudget, [Text]) -- ^ Result of running the script: an error or the 'ExBudget' and trace logs
        , sveRedeemer :: Redeemer
        , sveType     :: ScriptType -- ^ What type of script it was
        }
    | ScriptValidationResultOnlyEvent
        { sveResult   :: Either ScriptError (PV1.ExBudget, [Text])
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

validatorScriptValidationEvent
    :: Context
    -> Validator
    -> Datum
    -> Redeemer
    -> Either ScriptError (PV1.ExBudget, [Text])
    -> ScriptValidationEvent
validatorScriptValidationEvent ctx validator datum redeemer result =
    ScriptValidationEvent
        { sveScript = applyValidator ctx validator datum redeemer
        , sveResult = result
        , sveRedeemer = redeemer
        , sveType = ValidatorScript validator datum
        }

mpsValidationEvent
    :: Context
    -> MintingPolicy
    -> Redeemer
    -> Either ScriptError (PV1.ExBudget, [Text])
    -> ScriptValidationEvent
mpsValidationEvent ctx mps red result =
    ScriptValidationEvent
        { sveScript = applyMintingPolicyScript ctx mps red
        , sveResult = result
        , sveRedeemer = red
        , sveType = MintingPolicyScript mps
        }

data ValidatorMode = FullyAppliedValidators | UnappliedValidators
    deriving (Eq, Ord, Show)

-- | Get the script from a @ScriptValidationEvent@ in either fully applied or unapplied form.
getScript :: ValidatorMode -> ScriptValidationEvent -> Script
getScript FullyAppliedValidators ScriptValidationEvent{sveScript} = sveScript
getScript UnappliedValidators ScriptValidationEvent{sveType} =
    case sveType of
        ValidatorScript (Validator script) _    -> script
        MintingPolicyScript (MintingPolicy mps) -> mps
getScript _ ScriptValidationResultOnlyEvent{} = error "getScript: unexpected ScriptValidationResultOnlyEvent"

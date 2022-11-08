{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module PSGenerator.Common where

import Auth (AuthRole, AuthStatus)
import Cardano.Api.Shelley (Lovelace, NetworkId)
import Cardano.Slotting.Slot (EpochNo)
import Control.Applicative (empty, (<|>))
import Control.Lens (ix, (&), (.~), (^.))
import Control.Monad.Freer.Extras.Beam (BeamError, BeamLog)
import Control.Monad.Freer.Extras.Pagination (Page, PageQuery, PageSize)
import Control.Monad.Reader (MonadReader)
import Gist (Gist, GistFile, GistId, NewGist, NewGistFile, Owner)
import Language.PureScript.Bridge (BridgePart, DataConstructor (DataConstructor, _sigConstructor, _sigValues),
                                   DataConstructorArgs (Nullary, Record),
                                   Instance (Eq, Generic, GenericShow, Json, Ord), Language (Haskell), PSType,
                                   RecordEntry (RecordEntry), SumType (SumType),
                                   TypeInfo (TypeInfo, _typeModule, _typeName, _typePackage, _typeParameters), argonaut,
                                   equal, equal1, functor, genericShow, mkSumType, order, psTypeParameters, sumTypeInfo,
                                   typeModule, typeName, (^==))
import Language.PureScript.Bridge.Builder (BridgeData)
import Language.PureScript.Bridge.PSTypes (psInt, psNumber, psString)
import Language.PureScript.Bridge.SumType (sigConstructor, sigValues, sumTypeConstructors)
import Language.PureScript.Bridge.TypeParameters (A)
import Ledger (Address, BlockId, CardanoTx, Certificate, DatumFromQuery, DecoratedTxOut, OnChainTx, PaymentPubKey,
               PaymentPubKeyHash, PubKey, PubKeyHash, RedeemerPtr, ScriptTag, Signature, StakePubKey, StakePubKeyHash,
               Tx, TxId, TxIn, TxInType, TxInput, TxInputType, TxOut, TxOutRef, TxOutTx, UtxoIndex, ValidationPhase,
               Withdrawal)
import Ledger.Ada (Ada)
import Ledger.Constraints.OffChain (MkTxError, UnbalancedTx)
import Ledger.Credential (Credential, StakingCredential)
import Ledger.DCert (DCert)
import Ledger.Index (ExCPU, ExMemory, ValidationError)
import Ledger.Interval (Extended, Interval, LowerBound, UpperBound)
import Ledger.Params (Params)
import Ledger.Scripts (ScriptError)
import Ledger.Slot (Slot)
import Ledger.TimeSlot (SlotConfig, SlotConversionError)
import Ledger.Tx qualified as Tx (Language, Versioned)
import Ledger.Tx.CardanoAPI (FromCardanoError, ToCardanoError)
import Ledger.Value (AssetClass, CurrencySymbol, TokenName, Value)
import Ouroboros.Network.Magic (NetworkMagic)
import Playground.Types (ContractCall, FunctionSchema, KnownCurrency)
import Plutus.ChainIndex.Api (IsUtxoResponse, QueryResponse, TxosResponse, UtxosResponse)
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog)
import Plutus.ChainIndex.Tx (ChainIndexTx, ChainIndexTxOutputs, ReferenceScript)
import Plutus.ChainIndex.Tx qualified as ChainIndex
import Plutus.ChainIndex.Types (BlockNumber, Depth, Point, RollbackState, Tip, TxOutState, TxValidity)
import Plutus.ChainIndex.UtxoState (InsertUtxoFailed, InsertUtxoPosition, RollbackFailed)
import Plutus.Contract.Checkpoint (CheckpointError)
import Plutus.Contract.Effects (ActiveEndpoint, BalanceTxResponse, ChainIndexQuery, ChainIndexResponse, PABReq, PABResp,
                                WriteBalancedTxResponse)
import Plutus.Contract.Error (AssertionError, ContractError, MatchingError)
import Plutus.Contract.Resumable (IterationID, Request, RequestID, Response)
import Plutus.Script.Utils.V1.Typed.Scripts (ConnectionError, WrongOutTypeError)
import Plutus.Trace.Emulator.Types (ContractInstanceLog, ContractInstanceMsg, ContractInstanceTag, EmulatorRuntimeError,
                                    UserThreadMsg)
import Plutus.Trace.Scheduler (Priority, SchedulerLog, ThreadEvent, ThreadId)
import Plutus.Trace.Tag (Tag)
import Plutus.V1.Ledger.Api (DatumHash, MintingPolicy, StakeValidator, TxOut, Validator)
import Plutus.V2.Ledger.Tx qualified as PV2
import Schema (FormArgumentF, FormSchema)
import Wallet.API (WalletAPIError)
import Wallet.Emulator.Types qualified as EM
import Wallet.Rollup.Types (AnnotatedTx, BeneficialOwner, DereferencedInput, SequenceId, TxKey)
import Wallet.Types (ContractActivityStatus, ContractInstanceId, EndpointDescription, EndpointValue, Notification,
                     NotificationError)

psJson :: PSType
psJson = TypeInfo "web-common" "Data.RawJson" "RawJson" []

psNonEmpty :: MonadReader BridgeData m => m PSType
psNonEmpty =
    TypeInfo "purescript-lists" "Data.List.Types" "NonEmptyList" <$>
    psTypeParameters

psSet :: MonadReader BridgeData m => m PSType
psSet =
    TypeInfo "purescript-ordered-collections" "Data.Set" "Set" <$>
    psTypeParameters

psUUID :: PSType
psUUID = TypeInfo "web-common" "Data.UUID.Argonaut" "UUID" []

uuidBridge :: BridgePart
uuidBridge = do
    typeName ^== "UUID"
    typeModule ^== "Data.UUID" <|> typeModule ^== "Data.UUID.Types.Internal"
    pure psUUID

aesonValueBridge :: BridgePart
aesonValueBridge = do
    typeName ^== "Value"
    typeModule ^== "Data.Aeson.Types.Internal"
    pure psJson

aesonBridge :: BridgePart
aesonBridge =
    aesonValueBridge <|> uuidBridge

------------------------------------------------------------
nonEmptyBridge :: BridgePart
nonEmptyBridge = do
    typeName ^== "NonEmpty"
    typeModule ^== "GHC.Base"
    psNonEmpty

setBridge :: BridgePart
setBridge = do
    typeName ^== "Set"
    typeModule ^== "Data.Set.Internal"
    psSet

containersBridge :: BridgePart
containersBridge = nonEmptyBridge <|> setBridge

------------------------------------------------------------
psBigInteger :: PSType
psBigInteger = TypeInfo "web-common" "Data.BigInt.Argonaut" "BigInt" []

integerBridge :: BridgePart
integerBridge = do
    typeName ^== "Integer"
    pure psBigInteger

word32Bridge :: BridgePart
word32Bridge = do
    typeName ^== "Word32"
    typeModule ^== "GHC.Word"
    pure psInt

word64Bridge :: BridgePart
word64Bridge = do
    typeName ^== "Word64"
    typeModule ^== "GHC.Word"
    pure psBigInteger

digestBridge :: BridgePart
digestBridge = do
    typeName ^== "Digest"
    typeModule ^== "Crypto.Hash.Types"
    pure psString

byteStringBridge :: BridgePart
byteStringBridge = do
    typeName ^== "ByteString"
    typeModule ^== "Data.ByteString.Lazy.Internal" <|> typeModule ^== "Data.ByteString.Internal"
    pure psString

bultinByteStringBridge :: BridgePart
bultinByteStringBridge = do
    typeName ^== "BuiltinByteString"
    typeModule ^== "PlutusTx.Builtins.Internal"
    pure psString

scientificBridge :: BridgePart
scientificBridge = do
    typeName ^== "Scientific"
    typeModule ^== "Data.Scientific"
    pure psNumber

naturalBridge :: BridgePart
naturalBridge = do
    typeName ^== "Natural"
    typeModule ^== "GHC.Natural"
    pure psInt

satIntBridge :: BridgePart
satIntBridge = do
    typeName ^== "SatInt"
    typeModule ^== "Data.SatInt" <|> typeModule ^== "Ledger.Index"
    pure psInt

exBudgetBridge :: BridgePart
exBudgetBridge = do
    typeName ^== "ExBudget"
    typeModule ^== "PlutusCore.Evaluation.Machine.ExBudget"
    pure psJson

someCardanoApiTxBridge :: BridgePart
someCardanoApiTxBridge = do
    typeName ^== "SomeCardanoApiTx"
    typeModule ^== "Ledger.Tx.CardanoAPI.Internal"
    pure psJson

cardanoBuildTxBridge :: BridgePart
cardanoBuildTxBridge = do
    typeName ^== "CardanoBuildTx"
    typeModule ^== "Ledger.Tx.CardanoAPI.Internal"
    pure psJson

alonzoEraBridge :: BridgePart
alonzoEraBridge = do
    typeName ^== "AlonzoEra"
    typeModule ^== "Cardano.Ledger.Alonzo"
    pure psString

standardCryptoBridge :: BridgePart
standardCryptoBridge = do
    typeName ^== "StandardCrypto"
    typeModule ^== "Cardano.Ledger.Crypto"
    pure psString

applyTxErrorBridge :: BridgePart
applyTxErrorBridge = do
    typeName ^== "ApplyTxError"
    typeModule ^== "Cardano.Ledger.Shelley.API.Mempool"
    pure psString

basicFailureBridge :: BridgePart
basicFailureBridge = do
    typeName ^== "BasicFailure"
    typeModule ^== "Cardano.Ledger.Alonzo.Tools"
    pure psString

scriptFailureBridge :: BridgePart
scriptFailureBridge = do
    typeName ^== "ScriptFailure"
    typeModule ^== "Cardano.Ledger.Alonzo.Tools"
    pure psString

utxosPredicateFailureBridge :: BridgePart
utxosPredicateFailureBridge = do
    typeName ^== "UtxosPredicateFailure"
    typeModule ^== "Cardano.Ledger.Alonzo.Rules.Utxos"
    pure psString

exportTxBridge :: BridgePart
exportTxBridge = do
    typeName ^== "ExportTx"
    typeModule ^== "Plutus.Contract.Wallet"
    pure psJson

protocolParametersBridge :: BridgePart
protocolParametersBridge = do
    typeName ^== "ProtocolParameters"
    typeModule ^== "Cardano.Api.ProtocolParameters"
    pure psJson

miscBridge :: BridgePart
miscBridge =
        bultinByteStringBridge
    <|> byteStringBridge
    <|> integerBridge
    <|> word32Bridge
    <|> word64Bridge
    <|> scientificBridge
    <|> digestBridge
    <|> naturalBridge
    <|> satIntBridge
    <|> exBudgetBridge
    <|> someCardanoApiTxBridge
    <|> cardanoBuildTxBridge
    <|> alonzoEraBridge
    <|> standardCryptoBridge
    <|> applyTxErrorBridge
    <|> basicFailureBridge
    <|> scriptFailureBridge
    <|> utxosPredicateFailureBridge
    <|> exportTxBridge
    <|> protocolParametersBridge

------------------------------------------------------------

psAssocMap :: MonadReader BridgeData m => m PSType
psAssocMap =
    TypeInfo "plutus-playground-client" "PlutusTx.AssocMap" "Map" <$>
    psTypeParameters

dataBridge :: BridgePart
dataBridge = do
    typeName ^== "BuiltinData"
    typeModule ^== "PlutusTx.Builtins.Internal"
    pure psString

assocMapBridge :: BridgePart
assocMapBridge = do
    typeName ^== "Map"
    typeModule ^== "PlutusTx.AssocMap"
    psAssocMap

languageBridge :: BridgePart
languageBridge = dataBridge <|> assocMapBridge

------------------------------------------------------------
scriptHashBridge :: BridgePart
scriptHashBridge = do
    typeName ^== "ScriptHash"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

scriptBridge :: BridgePart
scriptBridge = do
    typeName ^== "Script"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

datumBridge :: BridgePart
datumBridge = do
    typeName ^== "Datum"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

redeemerHashBridge :: BridgePart
redeemerHashBridge = do
    typeName ^== "RedeemerHash"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

redeemerBridge :: BridgePart
redeemerBridge = do
    typeName ^== "Redeemer"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

validatorHashBridge :: BridgePart
validatorHashBridge = do
    typeName ^== "ValidatorHash"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

mpsHashBridge :: BridgePart
mpsHashBridge = do
    typeName ^== "MintingPolicyHash"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

stakeValidatorHashBridge :: BridgePart
stakeValidatorHashBridge = do
    typeName ^== "StakeValidatorHash"
    typeModule ^== "Plutus.V1.Ledger.Scripts"
    pure psString

ledgerBytesBridge :: BridgePart
ledgerBytesBridge = do
    typeName ^== "LedgerBytes"
    typeModule ^== "Plutus.V1.Ledger.Bytes"
    pure psString

walletIdBridge :: BridgePart
walletIdBridge = do
    typeName ^== "WalletId"
    typeModule ^== "Wallet.Emulator.Wallet"
    pure psString

ledgerBridge :: BridgePart
ledgerBridge =
        scriptBridge
    <|> scriptHashBridge
    <|> redeemerHashBridge
    <|> redeemerBridge
    <|> datumBridge
    <|> validatorHashBridge
    <|> mpsHashBridge
    <|> stakeValidatorHashBridge
    <|> ledgerBytesBridge
    <|> walletIdBridge

------------------------------------------------------------
headersBridge :: BridgePart
headersBridge = do
    typeModule ^== "Servant.API.ResponseHeaders"
    typeName ^== "Headers"
    -- Headers should have two parameters, the list of headers and the return type.
    psTypeParameters >>= \case
        [_, returnType] -> pure returnType
        _               -> empty

headerBridge :: BridgePart
headerBridge = do
    typeModule ^== "Servant.API.Header"
    typeName ^== "Header'"
    empty

servantBridge :: BridgePart
servantBridge = headersBridge <|> headerBridge

-- TODO: implement a proper SumType, this is a stub to make purescript compile
scriptAnyLangType :: SumType 'Haskell
scriptAnyLangType = SumType (
      TypeInfo {
        _typePackage = "crdn-p-1.33.0-c62ffc00"
      , _typeModule = "Cardano.Api.Script"
      , _typeName = "ScriptInAnyLang"
      , _typeParameters = []
    }
  ) [
      DataConstructor {_sigConstructor = "SimpleScriptLanguageV1", _sigValues = Nullary}
    , DataConstructor {_sigConstructor = "SimpleScriptLanguageV2", _sigValues = Nullary}
    , DataConstructor {_sigConstructor = "PlutusScriptLanguageV1", _sigValues = Nullary}
    , DataConstructor {_sigConstructor = "PlutusScriptLanguageV2", _sigValues = Nullary}
  ] [Eq,GenericShow,Json,Ord,Generic]

plutusTxOut :: SumType 'Haskell
plutusTxOut = equal . genericShow . argonaut $ mkSumType @Plutus.V1.Ledger.Api.TxOut

-- TODO: implement a proper SumType, this is a stub to make purescript compile
cardanoTxOut :: SumType 'Haskell
cardanoTxOut = plutusTxOut
   & sumTypeInfo .~
      TypeInfo {
        _typePackage = "crdn-p-1.33.0-c62ffc00"
      , _typeModule = "Cardano.Api.TxBody"
      , _typeName = "CardanoTxOut"
      , _typeParameters = []
    }

txOut :: SumType 'Haskell
txOut =
     equal . genericShow . argonaut $ mkSumType @Ledger.TxOut
       & sumTypeConstructors . ix 0 . sigConstructor .~ "TxOut"
       & sumTypeConstructors . ix 0 . sigValues .~
           Record (pure (RecordEntry "getTxOut" (cardanoTxOut ^. sumTypeInfo)))


------------------------------------------------------------
ledgerTypes :: [SumType 'Haskell]
ledgerTypes =
    [ order . genericShow . argonaut $ mkSumType @Tx.Language
    , order . genericShow . argonaut $ mkSumType @(Tx.Versioned A)
    , equal . genericShow . argonaut $ mkSumType @Slot
    , equal . genericShow . argonaut $ mkSumType @Ada
    , equal . genericShow . argonaut $ mkSumType @Params
    , equal . genericShow . argonaut $ mkSumType @NetworkId
    , equal . genericShow . argonaut $ mkSumType @NetworkMagic
    , equal . genericShow . argonaut $ mkSumType @Lovelace
    , equal . genericShow . argonaut $ mkSumType @EpochNo
    , equal . genericShow . argonaut $ mkSumType @SlotConfig
    , equal . genericShow . argonaut $ mkSumType @SlotConversionError
    , equal . genericShow . argonaut $ mkSumType @Certificate
    , equal . genericShow . argonaut $ mkSumType @Tx
    , plutusTxOut
    , equal . genericShow . argonaut $ mkSumType @CardanoTx
    , order . genericShow . argonaut $ mkSumType @TxId
    , order . equal . genericShow . argonaut $ mkSumType @TxIn
    , txOut
    , equal . genericShow . argonaut $ mkSumType @TxOutTx
    , order . genericShow . argonaut $ mkSumType @TxOutRef
    , equal . genericShow . argonaut $ mkSumType @OnChainTx
    , equal . genericShow . argonaut $ mkSumType @UtxoIndex
    , equal . genericShow . argonaut $ mkSumType @Value
    , equal . genericShow . argonaut $ mkSumType @Withdrawal
    , equal . genericShow . argonaut $ mkSumType @DatumFromQuery
    -- v2
    , equal . genericShow . argonaut $ mkSumType @PV2.OutputDatum
    , equal . genericShow . argonaut $ mkSumType @ReferenceScript
    , scriptAnyLangType
    , cardanoTxOut
    -- v2-end
    , functor . equal . genericShow . argonaut $ mkSumType @(Extended A)
    , functor . equal . genericShow . argonaut $ mkSumType @(Interval A)
    , functor . equal . genericShow . argonaut $ mkSumType @(LowerBound A)
    , functor . equal . genericShow . argonaut $ mkSumType @(UpperBound A)
    , genericShow . order . argonaut $ mkSumType @CurrencySymbol
    , genericShow . order . argonaut $ mkSumType @AssetClass
    , genericShow . order . argonaut $ mkSumType @MintingPolicy
    , genericShow . order . argonaut $ mkSumType @StakeValidator
    , genericShow . order . argonaut $ mkSumType @RedeemerPtr
    , genericShow . order . argonaut $ mkSumType @ScriptTag
    , genericShow . order . argonaut $ mkSumType @Signature
    , genericShow . order . argonaut $ mkSumType @TokenName
    , genericShow . order . argonaut $ mkSumType @TxInType
    , order . equal . genericShow . argonaut $ mkSumType @TxInputType
    , order . equal . genericShow . argonaut $ mkSumType @TxInput
    , genericShow . order . argonaut $ mkSumType @Validator
    , equal . genericShow . argonaut $ mkSumType @ScriptError
    , equal . genericShow . argonaut $ mkSumType @ValidationError
    , order . equal . genericShow . argonaut $ mkSumType @ValidationPhase
    , order . genericShow . argonaut $ mkSumType @Address
    , order . genericShow . argonaut $ mkSumType @BlockId
    , order . genericShow . argonaut $ mkSumType @DatumHash
    , order . genericShow . argonaut $ mkSumType @PubKey
    , order . genericShow . argonaut $ mkSumType @PubKeyHash
    , order . genericShow . argonaut $ mkSumType @PaymentPubKey
    , order . genericShow . argonaut $ mkSumType @PaymentPubKeyHash
    , order . genericShow . argonaut $ mkSumType @StakePubKey
    , order . genericShow . argonaut $ mkSumType @StakePubKeyHash
    , order . genericShow . argonaut $ mkSumType @Credential
    , order . genericShow . argonaut $ mkSumType @StakingCredential
    , order . genericShow . argonaut $ mkSumType @DCert
    , equal . genericShow . argonaut $ mkSumType @MkTxError
    , equal . genericShow . argonaut $ mkSumType @ContractError
    , equal . genericShow . argonaut $ mkSumType @ConnectionError
    , order . equal . genericShow . argonaut $ mkSumType @WrongOutTypeError
    , equal . genericShow . argonaut $ mkSumType @Notification
    , equal . genericShow . argonaut $ mkSumType @NotificationError
    , equal . genericShow . argonaut $ mkSumType @MatchingError
    , equal . genericShow . argonaut $ mkSumType @AssertionError
    , equal . genericShow . argonaut $ mkSumType @CheckpointError
    , order . genericShow . argonaut $ mkSumType @ContractInstanceId
    , order . equal . genericShow . argonaut $ mkSumType @ContractActivityStatus
    , equal . genericShow . argonaut $ mkSumType @ContractInstanceLog
    , equal . genericShow . argonaut $ mkSumType @UserThreadMsg
    , equal . genericShow . argonaut $ mkSumType @SchedulerLog
    , equal . genericShow . argonaut $ mkSumType @Tag
    , equal . genericShow . argonaut $ mkSumType @ContractInstanceMsg
    , equal . genericShow . argonaut $ mkSumType @ContractInstanceTag
    , equal . genericShow . argonaut $ mkSumType @EmulatorRuntimeError
    , order . equal . genericShow . argonaut $ mkSumType @ThreadEvent
    , equal . genericShow . argonaut $ mkSumType @ThreadId
    , equal . genericShow . argonaut $ mkSumType @(Request A)
    , equal . genericShow . argonaut $ mkSumType @(Response A)
    , order . genericShow . argonaut $ mkSumType @RequestID
    , order . equal . genericShow . argonaut $ mkSumType @Priority
    , order . genericShow . argonaut $ mkSumType @IterationID
    , equal . genericShow . argonaut $ mkSumType @ExCPU
    , equal . genericShow . argonaut $ mkSumType @ExMemory
    , equal . genericShow . argonaut $ mkSumType @PABReq
    , equal . genericShow . argonaut $ mkSumType @PABResp
    , equal . genericShow . argonaut $ mkSumType @ChainIndexQuery
    , equal . genericShow . argonaut $ mkSumType @ChainIndexResponse
    , equal . genericShow . argonaut $ mkSumType @IsUtxoResponse
    , equal . genericShow . argonaut $ mkSumType @(QueryResponse A)
    , equal . genericShow . argonaut $ mkSumType @TxosResponse
    , equal . genericShow . argonaut $ mkSumType @UtxosResponse
    , equal . genericShow . argonaut $ mkSumType @ChainIndexTx
    , equal . genericShow . argonaut $ mkSumType @ChainIndex.ChainIndexTxOut
    , equal . genericShow . argonaut $ mkSumType @ChainIndexTxOutputs
    , equal . genericShow . argonaut $ mkSumType @DecoratedTxOut
    , equal . genericShow . argonaut $ mkSumType @ChainIndexLog
    , equal . genericShow . argonaut $ mkSumType @ChainIndexError
    , equal . genericShow . argonaut $ mkSumType @BeamError
    , equal . genericShow . argonaut $ mkSumType @BeamLog
    , order . equal . genericShow . argonaut $ mkSumType @InsertUtxoPosition
    , equal . genericShow . argonaut $ mkSumType @InsertUtxoFailed
    , equal . genericShow . argonaut $ mkSumType @RollbackFailed
    , order . equal . genericShow . argonaut $ mkSumType @FromCardanoError
    , equal . genericShow . argonaut $ mkSumType @(Page A)
    , equal . genericShow . argonaut $ mkSumType @(PageQuery A)
    , equal . genericShow . argonaut $ mkSumType @PageSize
    , equal . genericShow . argonaut $ mkSumType @Tip
    , equal . genericShow . argonaut $ mkSumType @Point
    , equal . genericShow . argonaut $ mkSumType @(EndpointValue A)
    , equal . genericShow . argonaut $ mkSumType @BalanceTxResponse
    , equal . genericShow . argonaut $ mkSumType @WriteBalancedTxResponse
    , equal . genericShow . argonaut $ mkSumType @ActiveEndpoint
    , equal . genericShow . argonaut $ mkSumType @UnbalancedTx
    , order . equal . genericShow . argonaut $ mkSumType @TxValidity
    , equal . genericShow . argonaut $ mkSumType @TxOutState
    , equal . genericShow . argonaut $ mkSumType @(RollbackState A)
    , equal . genericShow . argonaut $ mkSumType @BlockNumber
    , equal . genericShow . argonaut $ mkSumType @Depth
    ]

walletTypes :: [SumType 'Haskell]
walletTypes =
    [ equal . genericShow . argonaut $ mkSumType @AnnotatedTx
    , equal . genericShow . argonaut $ mkSumType @DereferencedInput
    , equal . genericShow . argonaut $ mkSumType @EM.Wallet
    , equal . genericShow . argonaut $ mkSumType @EM.WalletNumber
    , equal . genericShow . argonaut $ mkSumType @WalletAPIError
    , equal . genericShow . argonaut $ mkSumType @ToCardanoError
    , order . genericShow . argonaut $ mkSumType @BeneficialOwner
    , order . genericShow . argonaut $ mkSumType @SequenceId
    , order . genericShow . argonaut $ mkSumType @TxKey
    ]

------------------------------------------------------------
playgroundTypes :: [SumType 'Haskell]
playgroundTypes =
    [ genericShow . equal . argonaut $ mkSumType @FormSchema
    , functor . genericShow . equal . argonaut $ mkSumType @(FunctionSchema A)
    , functor . equal . equal1 . genericShow . argonaut $ mkSumType @(FormArgumentF A)
    , genericShow . order . argonaut $ mkSumType @EndpointDescription
    , genericShow . equal . argonaut $ mkSumType @KnownCurrency
    , genericShow . equal . argonaut $ mkSumType @(ContractCall A)
    , order . equal . genericShow . argonaut $ mkSumType @GistId
    , equal . genericShow . argonaut $ mkSumType @Gist
    , equal . genericShow . argonaut $ mkSumType @GistFile
    , argonaut $ mkSumType @NewGist
    , argonaut $ mkSumType @NewGistFile
    , equal . genericShow . argonaut $ mkSumType @Owner
    , equal . genericShow . argonaut $ mkSumType @AuthStatus
    , order . equal . genericShow . argonaut $ mkSumType @AuthRole
    ]

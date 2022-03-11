{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
#if defined(__GHCJS__)
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
#endif

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map)
import Data.Word (Word32)
import GHC.Generics
import Ledger (ChainIndexTxOut, CurrencySymbol, Datum (..), DatumHash (..), Interval (..), MintingPolicy,
               MintingPolicyHash (..), POSIXTimeRange, PubKeyHash (..), Redeemer (..), TokenName, TxOutRef, Validator,
               ValidatorHash (..), Value, getPubKeyHash, unitRedeemer)
import Ledger.Constraints.OffChain (ScriptLookups (..), mkTx)
import Ledger.Constraints.TxConstraints (InputConstraint (..), OutputConstraint (..), TxConstraint (..),
                                         TxConstraints (..), UntypedConstraints)
import Ledger.Typed.TypeUtils (Any)
import Plutus.Contract.Wallet (ExportTx, export)
import Plutus.V1.Ledger.Ada qualified as Ada
import System.Environment (getArgs)
import System.Exit (die)

#if defined(ghcjs_HOST_OS)
-- hopefully the correct imports
import Codec.Serialise qualified as S
import Control.Exception (Exception, Handler (..), SomeException, catches, evaluate, throw, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.JSString qualified as JSString
import Data.JSString.Text
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C
import Foreign.Ptr
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Builtins (BuiltinByteString, BuiltinData, builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)

import Data.Foldable qualified as F
import Ledger.Address
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain qualified as OffChain
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Typed.Scripts.Validators (TypedValidator)
#endif

deriving instance Generic C.NetworkId
deriving instance ToJSON C.NetworkMagic
deriving instance FromJSON C.NetworkMagic

deriving instance ToJSON C.NetworkId
deriving instance FromJSON C.NetworkId

#if defined(ghcjs_HOST_OS)

{-
   If we are in GHCJS, make a synchronous callback and assign it to
   some global variable that we can access from the library.

   The default way of starting main is with the line from runmain.js:

       h$main(h$mainZCZCMainzimain);

   If we want to make the exports immediately available, we can try to
   remove the default main startup line, and stick the following at the
   end of the file

       h$runSync(h$mainZCZCMainzimain);

  We also need to make sure that the cryptography code
  compiled by emscripten also works, which requires waiting for the
  module to be initialized.

  We can do this as follows:

       h$runSync(h$mainZCZCMainzimain);

       console.log(plutus.constraints.mustBeSignedBy("2bb80d537b1da3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25b")));


 -}
main :: IO ()
main = initAPI =<< exportApi api

-- XXX start move to API

{-
   a JavaScript result value

   we use a wrapper that throws `Left x` as an exception, while `Right x`
   is returned normally
 -}
type JSResult = Either JSVal JSVal

-- a JavaScript data converter
data JSData a = JSData { convertIn   :: JSVal -> IO (Either String a)
                       , describeIn  :: Text
                       , convertOut  :: a -> IO JSVal
                       , describeOut :: Text
                       , dataName    :: Text
                       }

convertInBinaryData :: JSVal -> IO (Maybe ByteString)
convertInBinaryData x = do
  x' <- js_plutus_apps_convert_in_bytestring x
  if js_is_null x'
    then pure Nothing
    else let ptr = js_wrapped_buffer_to_ptr x'
             len = js_buffer_length x'
        in  Just <$> BSU.unsafePackCStringLen (ptr, len)

-- convert to Uint8Array
convertOutBinaryData :: ByteString -> IO JSVal
convertOutBinaryData bs = BSU.unsafeUseAsCStringLen bs $
  uncurry js_plutus_apps_convert_out_bytestring


foreign import javascript unsafe
  "$r = plutus_apps_convert_out_bytestring($1_1, $1_2, $2);"
  js_plutus_apps_convert_out_bytestring :: Ptr CChar -> Int -> IO JSVal

-- this is really too low-level, should be in ghcjs-prim / base
foreign import javascript unsafe
  "$r = plutus_apps_convert_in_bytestring($1);"
  js_plutus_apps_convert_in_bytestring :: JSVal -> IO JSVal

foreign import javascript unsafe
  "$r = $1.len;"
  js_buffer_length :: JSVal -> Int

-- convert the result from h$wrapBuffer to an Addr#
foreign import javascript unsafe
  "$r1 = $1; $r2 = 0;"
  js_wrapped_buffer_to_ptr :: JSVal -> Ptr CChar

foreign import javascript unsafe
  "$r = $1 === null;"
  js_is_null :: JSVal -> Bool

foreign import javascript unsafe
  "$r = plutus_apps_convert_in_maybe_int($1);"
  js_plutus_apps_convert_in_maybe_int :: JSVal -> IO JSVal


foreign import javascript unsafe
  "$r = $1|0;"
  js_get_word :: JSVal -> Word32

foreign import javascript unsafe
  "$r = null;"
  js_null :: JSVal

builtinDataJSData :: JSData BuiltinData
builtinDataJSData = JSData { convertIn = \v -> do
                                mb_bs <- convertInBinaryData v
                                pure $ case mb_bs of
                                  Just bs -> case S.deserialiseOrFail (BSL.fromStrict bs) of
                                    Right d -> Right (dataToBuiltinData d)
                                    _       -> Left "Could not deserialize"
                                  Nothing -> Left "unexpected value for binary data"
                           , describeIn = "a Uint8Array or hexadecimal encoded string"
                           , convertOut = convertOutBinaryData
                                          . BSL.toStrict
                                          . S.serialise
                                          . builtinDataToData
                           , describeOut = "a Uint8Array"
                           , dataName = "BuiltinData"
                           }

{-
  Marshall via Aeson instance. JSON objects are tagged with a type
  name so we can give resonable errors if things are mixed up.

  JSON looks like:

      { __internal_tag: "HaskellTypeName"
      , __internal_value: { aeson produced value }
      }

  The user is not supposed to construct/inspect these directly
  from the JS side.
 -}

data TaggedValue = TaggedValue { tvTag :: Text
                               , tvVal :: Aeson.Value
                               }

instance FromJSON TaggedValue where
  parseJSON (Aeson.Object v) = TaggedValue
        <$> v .: "__internal_tag"
        <*> v .: "__internal_value"
  parseJSON invalid =
    Aeson.prependFailure "parsing TaggedValue failed, "
                         (Aeson.typeMismatch "Object" invalid)

instance ToJSON TaggedValue where
  toJSON tv = Aeson.object [ "__internal_tag" Aeson..= toJSON (tvTag tv)
                           , "__internal_value" Aeson..= tvVal tv
                           ]

-- private JSON data (internal use only, structure expected to change between
-- releases), based on Aeson instance
taggedAesonJSData :: (Aeson.FromJSON a, Aeson.ToJSON a)
                  => Text -> JSData a
taggedAesonJSData name = JSData
  { convertIn = \x -> do
      mbVal <- fromJSVal x
      pure $ case mbVal of
                Just val ->
                  -- parse tagged object first, check tag name, parse value
                    (case Aeson.fromJSON val of
                      Aeson.Success tv ->
                        if tvTag tv == name
                        then (case Aeson.fromJSON (tvVal tv) of
                                    Aeson.Success v -> Right v
                                    Aeson.Error e   -> Left e)
                        else Left ("tag mismatch, expected " <> T.unpack name <> " but got " <> T.unpack (tvTag tv))
                      _ -> Left "failed parsing tagged JSON value")
                _ -> Left "invalid JSON value"
  , describeIn  = name <> " internal JSON value"
  , convertOut  = \x ->
      let val = Aeson.toJSON x
          tagged_val = Aeson.toJSON (TaggedValue name val)
      in toJSVal tagged_val
  , describeOut = name <> " internal JSON value"
  , dataName    = name
  }

-- public JSON data (directly used by API user) from Aeson instance
aesonJSData :: (Aeson.FromJSON a, Aeson.ToJSON a)
                  => Text -> JSData a
aesonJSData name = customAesonJSData Aeson.parseJSON Aeson.toJSON name

-- public JSON data (directly used by API user) with custom Aeson parser
customAesonJSData :: (Aeson.Value -> Aeson.Parser a)
                  -> (a -> Aeson.Value)
                  -> Text
                  -> JSData a
customAesonJSData fromAeson toAeson name = JSData
  { convertIn = \x -> do
      mbVal <- fromJSVal x
      pure $ case mbVal of
                Just val ->
                  case Aeson.parse fromAeson val of
                    Aeson.Success v -> Right v
                    Aeson.Error e   -> Left e
                _ -> Left "invalid JSON object"
  , describeIn  = name <> " JSON object"
  , convertOut  = toJSVal . toAeson
  , describeOut = name <> " JSON object"
  , dataName    = name
  }

pubKeyHashJSData :: (a -> PubKeyHash) -> (PubKeyHash -> a) -> Text -> JSData a
pubKeyHashJSData unwrap wrap name =
  bytestringHashJSData (getPubKeyHash . unwrap) (wrap . PubKeyHash) name

bytestringHashJSData :: (a -> BuiltinByteString) -> (BuiltinByteString -> a) -> Text -> JSData a
bytestringHashJSData unwrap wrap name = JSData
  { convertIn = \x -> do
      mb_bx <- convertInBinaryData x
      pure $ case mb_bx of
        Just bx -> Right (wrap (toBuiltin bx))
        Nothing -> Left "invalid binary data"
  , describeIn = name <> " Uint8Array or hexadecimal string"
  , convertOut = \x -> convertOutBinaryData . fromBuiltin . unwrap $ x
  , describeOut = name <> " Uint8Array"
  , dataName = name
  }


paymentPubKeyHashJSData :: JSData PaymentPubKeyHash
paymentPubKeyHashJSData = pubKeyHashJSData unPaymentPubKeyHash
                                           PaymentPubKeyHash
                                           "PaymentPubKeyHash"

stakePubKeyHashJSData :: JSData StakePubKeyHash
stakePubKeyHashJSData = pubKeyHashJSData unStakePubKeyHash
                                           StakePubKeyHash
                                           "StakePubKeyHash"

mintingPolicyHashJSData :: JSData MintingPolicyHash
mintingPolicyHashJSData = bytestringHashJSData (\(MintingPolicyHash x) -> x)
                                               MintingPolicyHash
                                               "MintingPolicyHash"

scriptLookupsJSData :: JSData (ScriptLookups Any)
scriptLookupsJSData = taggedAesonJSData "ScriptLookups"

rawScriptLookupsJSData :: JSData (ScriptLookups Any)
rawScriptLookupsJSData = aesonJSData "ScriptLookups"

txConstraintsJSData :: JSData UntypedConstraints
txConstraintsJSData = taggedAesonJSData "UntypedConstraints"

valueJSData :: JSData Value
valueJSData = customAesonJSData fromAeson toAeson "Value XXX"
  where
    fromAeson :: Aeson.Value -> Aeson.Parser Value
    fromAeson = Aeson.withArray "Value-0" parseElems
    parseElems v = makeValue <$> mapM parseElem (F.toList v)
    parseElem :: Aeson.Value -> Aeson.Parser (CurrencySymbol, TokenName, Integer)
    parseElem = Aeson.withArray "Value-1" $ \v ->
      case F.toList v of
        [c, tn, i] -> (,,) <$> Aeson.parseJSON c
                           <*> Aeson.parseJSON tn
                           <*> Aeson.parseJSON i
        _ -> fail "Expected three-element array"
    makeValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
    makeValue = mconcat . map (\(c, tn, i) -> Value.singleton c tn i)
    toAeson v = Aeson.toJSON (Value.flattenValue v)

pOSIXTimeRangeJSData :: JSData POSIXTimeRange
pOSIXTimeRangeJSData = customAesonJSData fromAeson toAeson "POSIXTimeRange"
  where
    fromAeson :: Aeson.Value -> Aeson.Parser POSIXTimeRange
    fromAeson = Aeson.withObject "POSIXTimeRange" $ \v ->
        Interval <$> v .: "from" <*> v .: "to"
    toAeson v = Aeson.object [ "from" Aeson..= Aeson.toJSON (ivFrom v)
                             , "to"   Aeson..= Aeson.toJSON (ivTo v)
                             ]

-- XXX this is the same as datumJSData, generalise
redeemerJSData :: JSData Redeemer
redeemerJSData = JSData { convertIn = \v -> do
                          mb_bs <- convertInBinaryData v
                          pure $ case mb_bs of
                            Just bs -> case S.deserialiseOrFail (BSL.fromStrict bs) of
                              Right d -> Right (Redeemer $ dataToBuiltinData d)
                              _       -> Left "Could not deserialize"
                            Nothing -> Left "unexpected value for binary data"
                      , describeIn = "a Uint8Array or hexadecimal encoded string"
                      , convertOut = convertOutBinaryData
                                    . BSL.toStrict
                                    . S.serialise
                                    . builtinDataToData
                                    . getRedeemer
                      , describeOut = "a Uint8Array"
                      , dataName = "Redeemer"
                      }



data InvalidArgumentException = InvalidArgumentException String
  deriving (Eq, Show, Exception)

handleConvertIn :: JSData a -> JSVal -> IO a
handleConvertIn jsd v = do
  mb_x <- convertIn jsd v
  case mb_x of
    Right x -> pure x
    Left e  -> throwIO (InvalidArgumentException $ T.unpack (describeIn jsd) ++ "\n" ++ e)

handleResult :: JSData r -> r -> IO JSResult
handleResult jsd x =
  (Right <$> (evaluate =<< convertOut jsd x)) `catches` handlers
  where
    handlers = [Handler (\ (InvalidArgumentException msg) ->
      pure $ Left $ jsval ("Invalid Argument: " <> JSString.pack msg)),
                Handler (\ (ex :: SomeException)  -> pure $ Left $ jsval ("Exception: " <> JSString.pack (show ex)))]

mkApi1 :: (a -> r)
       -> JSData a
       -> JSData r
       -> IO (JSVal -> IO JSResult)
mkApi1 f in1 out = pure $ \js_x ->
    handleResult out . f =<< handleConvertIn in1 js_x

mkApi2 :: (a -> b -> r)
       -> JSData a
       -> JSData b
       -> JSData r
       -> IO (JSVal -> JSVal -> IO JSResult)
mkApi2 f in1 in2 out = pure $ \js_x1 js_x2 -> do
  x1 <- handleConvertIn in1 js_x1
  x2 <- handleConvertIn in2 js_x2
  handleResult out (f x1 x2)

mkApi3 :: (a -> b -> c -> r)
       -> JSData a
       -> JSData b
       -> JSData c
       -> JSData r
       -> IO (JSVal -> JSVal -> JSVal -> IO JSResult)
mkApi3 f in1 in2 in3 out = pure $ \js_x1 js_x2 js_x3 -> do
  x1 <- handleConvertIn in1 js_x1
  x2 <- handleConvertIn in2 js_x2
  x3 <- handleConvertIn in3 js_x3
  handleResult out (f x1 x2 x3)

mkApi4 :: (a -> b -> c -> d -> r)
       -> JSData a
       -> JSData b
       -> JSData c
       -> JSData d
       -> JSData r
       -> IO (JSVal -> JSVal -> JSVal -> JSVal -> IO JSResult)
mkApi4 f in1 in2 in3 in4 out = pure $ \js_x1 js_x2 js_x3 js_x4 -> do
  x1 <- handleConvertIn in1 js_x1
  x2 <- handleConvertIn in2 js_x2
  x3 <- handleConvertIn in3 js_x3
  x4 <- handleConvertIn in4 js_x4
  handleResult out (f x1 x2 x3 x4)

api :: [Pair]
api =
  [ "constraints" .=
    [ -- addTxIn :: TxOutRef -> i -> TxConstraints i o -> TxConstraints i o

      "addTxIn"                         .= mkApi3 Constraints.addTxIn
                                                  txOutRefJSData
                                                  builtinDataJSData
                                                  txConstraintsJSData
                                                  txConstraintsJSData
      -- mustPayToTheScript :: forall i o. PlutusTx.ToData o => o -> Value -> TxConstraints i o
    , "mustPayToTheScript"              .= mkApi2 Constraints.mustPayToTheScript
                                                  builtinDataJSData
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustPayToPubKey :: forall i o. PaymentPubKeyHash -> Value -> TxConstraints i o
      "mustPayToPubKey"                 .= mkApi2 Constraints.mustPayToPubKey
                                                  paymentPubKeyHashJSData
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustPayToPubKeyAddress :: forall i o. PaymentPubKeyHash -> StakePubKeyHash -> Value -> TxConstraints i o
      "mustPayToPubKeyAddress"          .= mkApi3 Constraints.mustPayToPubKeyAddress
                                                  paymentPubKeyHashJSData
                                                  stakePubKeyHashJSData
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustPayWithDatumToPubKey :: forall i o. PaymentPubKeyHash -> Datum -> Value -> TxConstraints i o
      "mustPayWithDatumToPubKey"        .= mkApi3 Constraints.mustPayWithDatumToPubKey
                                                  paymentPubKeyHashJSData
                                                  datumJSData
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustPayWithDatumToPubKeyAddress :: forall i o. PaymentPubKeyHash -> StakePubKeyHash -> Datum -> Value -> TxConstraints i o
      "mustPayWithDatumToPubKeyAddress" .= mkApi4 Constraints.mustPayWithDatumToPubKeyAddress
                                                  paymentPubKeyHashJSData
                                                  stakePubKeyHashJSData
                                                  datumJSData
                                                  valueJSData
                                                  txConstraintsJSData
      -- mustMintCurrency :: forall i o. MintingPolicyHash -> TokenName -> Integer -> TxConstraints i o
    , "mustMintCurrency"                .= mkApi3 Constraints.mustMintCurrency
                                                  mintingPolicyHashJSData
                                                  tokenNameJSData
                                                  integerJSData
                                                  txConstraintsJSData
    , -- mustMintCurrencyWithRedeemer :: forall i o. MintingPolicyHash -> Redeemer -> TokenName -> Integer -> TxConstraints i o
      "mustMintCurrencyWithRedeemer"    .= mkApi4 Constraints.mustMintCurrencyWithRedeemer
                                                  mintingPolicyHashJSData
                                                  redeemerJSData
                                                  tokenNameJSData
                                                  integerJSData
                                                  txConstraintsJSData
    , -- mustMintValue :: forall i o. Value -> TxConstraints i o
      "mustMintValue"                   .= mkApi1 Constraints.mustMintValue
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustMintValueWithRedeemer :: forall i o. Redeemer -> Value -> TxConstraints i o
      "mustMintValueWithRedeemer"       .= mkApi2 Constraints.mustMintValueWithRedeemer
                                                  redeemerJSData
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustSpendAtLeast :: forall i o. Value -> TxConstraints i o
      "mustSpendAtLeast"                .= mkApi1 Constraints.mustSpendAtLeast
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustSpendPubKeyOutput :: forall i o. TxOutRef -> TxConstraints i o
      "mustSpendPubKeyOutput"           .= mkApi1 Constraints.mustSpendPubKeyOutput
                                                  txOutRefJSData
                                                  txConstraintsJSData
    , -- mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o
      "mustSpendScriptOutput"           .= mkApi2 Constraints.mustSpendScriptOutput
                                                  txOutRefJSData
                                                  redeemerJSData
                                                  txConstraintsJSData
    , -- mustValidateIn :: forall i o. POSIXTimeRange -> TxConstraints i o
      "mustValidateIn"                  .= mkApi1 Constraints.mustValidateIn
                                                  pOSIXTimeRangeJSData
                                                  txConstraintsJSData
    , -- mustBeSignedBy :: forall i o. PaymentPubKeyHash -> TxConstraints i o
      "mustBeSignedBy"                  .= mkApi1 Constraints.mustBeSignedBy
                                                  paymentPubKeyHashJSData
                                                  txConstraintsJSData
    , -- mustProduceAtLeast :: forall i o. Value -> TxConstraints i o
      "mustProduceAtLeast"              .= mkApi1 Constraints.mustProduceAtLeast
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustIncludeDatum :: forall i o. Datum -> TxConstraints i o
      "mustIncludeDatum"                .= mkApi1 Constraints.mustIncludeDatum
                                                  datumJSData
                                                  txConstraintsJSData
    , -- mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
      "mustPayToOtherScript"            .= mkApi3 Constraints.mustPayToOtherScript
                                                  validatorHashJSData
                                                  datumJSData
                                                  valueJSData
                                                  txConstraintsJSData
    , -- mustHashDatum :: DatumHash -> Datum -> TxConstraints i o
      "mustHashDatum"                   .= mkApi2 Constraints.mustHashDatum
                                                  datumHashJSData
                                                  datumJSData
                                                  txConstraintsJSData
    , -- mustSatisfyAnyOf :: forall i o. [TxConstraints i o] -> TxConstraints i o
      "mustSatisfyAnyOf"                .= mkApi1 Constraints.mustSatisfyAnyOf
                                                  txConstraintsListJSData
                                                  txConstraintsJSData
    ]
  , "offchain" .=
    [ -- just converts the "external" JSON for ScriptLookups to the "internal"
      "rawLookups"                      .= mkApi1 id
                                                  rawScriptLookupsJSData
                                                  scriptLookupsJSData
    , -- typedValidatorLookups :: TypedValidator a -> ScriptLookups a
      "typedValidatorLookups"           .= mkApi1 OffChain.typedValidatorLookups
                                                  typedValidatorJSData
                                                  scriptLookupsJSData
    , -- unspentOutputs :: Map TxOutRef ChainIndexTxOut -> ScriptLookups a
      "unspentOutputs"                  .= mkApi1 OffChain.unspentOutputs
                                                  chainIndexTxOutMapJSData
                                                  scriptLookupsJSData
    , -- mintingPolicy :: MintingPolicy -> ScriptLookups a
      "mintingPolicy"                   .= mkApi1 OffChain.mintingPolicy
                                                  mintingPolicyJSData
                                                  scriptLookupsJSData
    , -- otherScript :: Validator -> ScriptLookups a
      "otherScript"                     .= mkApi1 OffChain.otherScript
                                                  validatorJSData
                                                  scriptLookupsJSData
    , -- otherData :: Datum -> ScriptLookups a
      "otherData"                       .= mkApi1 OffChain.otherData
                                                  datumJSData
                                                  scriptLookupsJSData
    , -- ownPaymentPubKeyHash :: PaymentPubKeyHash -> ScriptLookups a
      "ownPaymentPubKeyHash"            .= mkApi1 OffChain.ownPaymentPubKeyHash
                                                  paymentPubKeyHashJSData
                                                  scriptLookupsJSData
    , -- ownStakePubKeyHash :: StakePubKeyHash -> ScriptLookups a
      "ownStakePubKeyHash"              .= mkApi1 OffChain.ownStakePubKeyHash
                                                  stakePubKeyHashJSData
                                                  scriptLookupsJSData
    , -- combineScriptLookups :: [ScriptLookups a] -> ScriptLookups a
      "combineScriptLookups"            .= mkApi1 combineScriptLookups
                                                  scriptLookupsListJSData
                                                  scriptLookupsJSData
    , -- mkTx' :: ScriptLookups Any -> UntypedConstraints -> OffChain.UnbalancedTx
      "mkTx"                            .= mkApi2 mkTx'
                                                  scriptLookupsJSData
                                                  txConstraintsJSData
                                                  unbalancedTxJSData
    , "exportTx"                        .= mkApi3 exportTx
                                                  protocolParametersJSData
                                                  networkIdJSData
                                                  unbalancedTxJSData
                                                  exportTxJSData
    , -- paymentPubKey :: PaymentPubKey -> ScriptLookups a
      "paymentPubKey"                   .= mkApi1 OffChain.paymentPubKey
                                                  paymentPubKeyJSData
                                                  scriptLookupsJSData
    , -- adjustUnbalancedTx :: UnbalancedTx -> UnbalancedTx
      "adjustUnbalancedTx"              .= mkApi1 OffChain.adjustUnbalancedTx
                                                  unbalancedTxJSData
                                                  unbalancedTxJSData
    ]
  ]

combineScriptLookups :: [ScriptLookups Any] -> ScriptLookups Any
combineScriptLookups xs = mconcat xs

mkTx' :: ScriptLookups Any -> UntypedConstraints -> OffChain.UnbalancedTx
mkTx' lookups constraints =
  case OffChain.mkTx lookups constraints of
    Left err   -> throw (InvalidArgumentException (show err))
    Right ubtx -> ubtx

exportTx :: C.ProtocolParameters -> C.NetworkId -> OffChain.UnbalancedTx -> ExportTx
exportTx pparams networkid ubtx =
  case export pparams networkid ubtx of
    Left err    -> throw (InvalidArgumentException (show err))
    Right exptx -> exptx

unbalancedTxJSData :: JSData OffChain.UnbalancedTx
unbalancedTxJSData = taggedAesonJSData "UnbalancedTx"

exportTxJSData :: JSData ExportTx
exportTxJSData = aesonJSData "ExportTx"

paymentPubKeyJSData :: JSData PaymentPubKey
paymentPubKeyJSData = taggedAesonJSData "PaymentPubKey"

protocolParametersJSData :: JSData C.ProtocolParameters
protocolParametersJSData = aesonJSData "ProtocolParameters"

networkIdJSData :: JSData C.NetworkId
networkIdJSData = JSData { convertIn = \v -> do
                              v' <- js_plutus_apps_convert_in_maybe_int v
                              pure (Right (if js_is_null v'
                                    then C.Mainnet
                                    else C.Testnet . C.NetworkMagic . js_get_word $ v'))
                         , describeIn = "null for mainnet or a number for the testnet id"
                         , convertOut = \v -> case v of
                                           C.Testnet (C.NetworkMagic x) -> toJSVal x
                                           C.Mainnet                    -> pure js_null
                         , describeOut = "null for mainnet or a number for the testnet id"
                         , dataName = "NetworkId"
                         }

datumJSData :: JSData Datum
datumJSData = JSData { convertIn = \v -> do
                          mb_bs <- convertInBinaryData v
                          pure $ case mb_bs of
                            Just bs -> case S.deserialiseOrFail (BSL.fromStrict bs) of
                              Right d -> Right (Datum $ dataToBuiltinData d)
                              _       -> Left "Could not deserialize"
                            Nothing -> Left "unexpected value for binary data"
                      , describeIn = "a Uint8Array or hexadecimal encoded string"
                      , convertOut = convertOutBinaryData
                                    . BSL.toStrict
                                    . S.serialise
                                    . builtinDataToData
                                    . getDatum
                      , describeOut = "a Uint8Array"
                      , dataName = "Datum"
                      }



datumHashJSData :: JSData DatumHash
datumHashJSData = bytestringHashJSData (\(DatumHash x) -> x)
                                               DatumHash
                                               "DatumHash"


-- validatorJSData :: JSData Plutus.V1.Ledger.Scripts.Validator
validatorJSData :: JSData Validator
validatorJSData = aesonJSData "Validator"

-- mintingPolicyJSData :: JSData Plutus.V1.Ledger.Scripts.MintingPolicy
mintingPolicyJSData :: JSData MintingPolicy
mintingPolicyJSData = aesonJSData "MintingPolicy"

-- typedValidatorJSData :: JSData (Ledger.Typed.Scripts.Validators.TypedValidator Any)
typedValidatorJSData :: JSData (TypedValidator Any)
typedValidatorJSData = aesonJSData "TypedValidator"

txConstraintsListJSData :: JSData [UntypedConstraints]
txConstraintsListJSData =
  JSData { convertIn = \x -> do
                mbVal <- fromJSVal x
                pure $ case mbVal of
                          Just (Aeson.Array vs) ->
                            -- parse tagged object first, check tag name, parse value
                            let convertSingle :: Aeson.Value -> Either String UntypedConstraints
                                convertSingle v
                                  = case Aeson.fromJSON v of
                                      Aeson.Error e -> Left ("error parsing tagged value: " <> e <> "\n" <> show v)
                                      Aeson.Success tv ->
                                        if tvTag tv == "UntypedConstraints"
                                        then case Aeson.fromJSON (tvVal tv) of
                                                    Aeson.Success c -> Right c
                                                    Aeson.Error e   -> Left e
                                        else Left ("tag mismatch, expected UntypedConstraints but got " <> T.unpack (tvTag tv))
                            in mapM convertSingle (F.toList vs)
                          _ -> Left "invalid JSON value"
           , describeIn = "an array of tagged UntypedConstraints values"
           , convertOut = \xs -> toJSVal (Aeson.toJSON (map Aeson.toJSON xs)) -- xx s
           , describeOut = "an array of tagged UntypedConstraints values"
           , dataName = "[UntypedConstraints]"
           }

scriptLookupsListJSData :: JSData [ScriptLookups Any]
scriptLookupsListJSData =
  JSData { convertIn = \x -> do
                mbVal <- fromJSVal x
                pure $ case mbVal of
                          Just (Aeson.Array vs) ->
                            -- parse tagged object first, check tag name, parse value
                            let convertSingle :: Aeson.Value -> Either String (ScriptLookups Any)
                                convertSingle v
                                  = case Aeson.fromJSON v of
                                      Aeson.Error e -> Left ("error parsing tagged value: " <> e <> "\n" <> show v)
                                      Aeson.Success tv ->
                                        if tvTag tv == "ScriptLookups"
                                        then case Aeson.fromJSON (tvVal tv) of
                                                    Aeson.Success sl -> Right sl
                                                    Aeson.Error e    -> Left e
                                        else Left ("tag mismatch, expected ScriptLookups but got " <> T.unpack (tvTag tv))
                            in mapM convertSingle (F.toList vs)
                          _ -> Left "invalid JSON value"
           , describeIn = "an array of tagged ScriptLookups values"
           , convertOut = \xs -> toJSVal (Aeson.toJSON (map Aeson.toJSON xs))
           , describeOut = "an array of tagged ScriptLookups values"
           , dataName = "[ScriptLookups]"
           }


validatorHashJSData :: JSData ValidatorHash
validatorHashJSData = bytestringHashJSData (\(ValidatorHash x) -> x)
                                               ValidatorHash
                                               "ValidatorHash"

chainIndexTxOutMapJSData :: JSData (Map TxOutRef ChainIndexTxOut)
chainIndexTxOutMapJSData = aesonJSData "Map_TxOutRef_ChainIndexTxOut"

txOutRefJSData :: JSData TxOutRef
txOutRefJSData = aesonJSData "TxOutRef"

-- XXX should we accept string as input?
integerJSData :: JSData Integer
integerJSData = aesonJSData "Integer"

tokenNameJSData :: JSData TokenName
tokenNameJSData = aesonJSData "TokenName"

-- XXX end move to API module

-- XXX start move to Util module

data Pair = Pair JSString (IO JSVal)

(.=) :: ApiExport a => JSString -> a -> Pair
key .= e = Pair key (exportApi e)

class ApiExport a where
    exportApi :: a -> IO JSVal

instance ApiExport JSVal where
    exportApi x = pure x

instance ApiExport Aeson.Value where
    exportApi v = toJSVal v

instance ApiExport Double where
    exportApi v = toJSVal v

instance ApiExport (JSVal -> JSVal) where
    exportApi f = jsval <$> syncCallback1' (\x -> pure (f x))

instance ApiExport (JSVal -> JSVal -> JSVal) where
    exportApi f = jsval <$> syncCallback2' (\x1 x2 -> pure (f x1 x2))

instance ApiExport (JSVal -> JSVal -> JSVal -> JSVal) where
    exportApi f = jsval <$> syncCallback3' (\x1 x2 x3 -> pure (f x1 x2 x3))

instance ApiExport (JSVal -> IO JSVal) where
    exportApi f = jsval <$> syncCallback1' f

instance ApiExport (JSVal -> JSVal -> IO JSVal) where
    exportApi f = jsval <$> syncCallback2' f

instance ApiExport (JSVal -> JSVal -> JSVal -> IO JSVal) where
    exportApi f = jsval <$> syncCallback3' f

instance ApiExport (JSVal -> JSResult) where
    exportApi f = jsval <$> syncCallback1' (\x -> wrapErrors (f x))

instance ApiExport (JSVal -> JSVal -> JSResult) where
    exportApi f = jsval <$> syncCallback2' (\x1 x2 -> wrapErrors (f x1 x2))

instance ApiExport (JSVal -> JSVal -> JSVal -> JSResult) where
    exportApi f = do
      v <- jsval <$> syncCallback3' (\x1 x2 x3 -> wrapErrors (f x1 x2 x3))
      js_handle_errors_3 v

instance ApiExport (JSVal -> IO JSResult) where
    exportApi f = do
      v <- jsval <$> syncCallback1' (\x -> wrapErrors =<< f x)
      js_handle_errors_1 v

instance ApiExport (JSVal -> JSVal -> IO JSResult) where
    exportApi f = do
      v <- jsval <$> syncCallback2' (\x1 x2 -> wrapErrors =<< f x1 x2)
      js_handle_errors_2 v

instance ApiExport (JSVal -> JSVal -> JSVal -> IO JSResult) where
    exportApi f = do
      v <- jsval <$> syncCallback3' (\x1 x2 x3 -> wrapErrors =<< f x1 x2 x3)
      js_handle_errors_3 v

instance ApiExport (JSVal -> JSVal -> JSVal -> JSVal -> IO JSResult) where
    exportApi f = do
      v <- jsval <$> syncCallback1' (\x -> wrapErrors =<< f' x)
      js_handle_errors_4 =<< js_wrap_call_4 v
        where
          -- syncCallback only exists for 1..3 arguments. Use a helper function
          -- that takes arguments from an array to get around this.
          f' :: JSVal -> IO JSResult
          f' x = do
            x1 <- js_get_arg x 0
            x2 <- js_get_arg x 1
            x3 <- js_get_arg x 2
            x4 <- js_get_arg x 3
            f x1 x2 x3 x4

foreign import javascript unsafe
  "$r = $1[$2];"
  js_get_arg :: JSVal -> Int -> IO JSVal

foreign import javascript unsafe
  "$r = plutus_apps_wrap_call_4($1);"
  js_wrap_call_4 :: JSVal -> IO JSVal


wrapErrors :: JSResult -> IO JSVal
wrapErrors (Left err) = js_error_val err
wrapErrors (Right v)  = js_success_val v

foreign import javascript unsafe
  "$r = { error: false, value: $1};"
  js_success_val :: JSVal -> IO JSVal

foreign import javascript unsafe
  "$r = { error: true, value: $1};"
  js_error_val :: JSVal -> IO JSVal

foreign import javascript unsafe
  "$r = plutus_apps_handle_errors_1($1);"
  js_handle_errors_1 :: JSVal -> IO JSVal

foreign import javascript unsafe
  "$r = plutus_apps_handle_errors_2($1);"
  js_handle_errors_2 :: JSVal -> IO JSVal

foreign import javascript unsafe
  "$r = plutus_apps_handle_errors_3($1);"
  js_handle_errors_3 :: JSVal -> IO JSVal


foreign import javascript unsafe
  "$r = plutus_apps_handle_errors_4($1);"
  js_handle_errors_4 :: JSVal -> IO JSVal


instance ApiExport [Pair] where
    exportApi pairs = do
        obj <- js_newObj
        mapM_ (addApi obj) pairs
        pure obj
      where
          addApi :: JSVal -> Pair -> IO ()
          addApi obj (Pair key val) = js_setProp obj key =<< val

instance ApiExport a => ApiExport (IO a) where
     exportApi x = exportApi =<< x

initAPI :: JSVal -> IO ()
initAPI v = js_initAPI v

foreign import javascript unsafe "$1[$2] = $3;" js_setProp :: JSVal -> JSString -> JSVal -> IO ()
foreign import javascript unsafe "$r = {};" js_newObj :: IO JSVal
foreign import javascript unsafe "plutus_apps_init_API($1);" js_initAPI :: JSVal -> IO ()

-- XXX end move to Util module

#else

-- command line version, for testing only
main :: IO ()
main = do
    -- minimal command line parser to avoid pulling in more than the JS lib would use
    args <- getArgs
    case args of
        [pparams, nwid, lookups, constraints] -> do
            let nwid' = if nwid == "mainnet"
                        then C.Mainnet
                        else C.Testnet . C.NetworkMagic . read $ nwid
                readJSONFile file = do
                    bs <- BSL.readFile file
                    case Aeson.eitherDecode bs of
                        Left err -> die ("error decoding JSON file " ++ file ++ " " ++ show err)
                        Right x  -> pure x
            pparams'     <- readJSONFile pparams
            lookups'     <- readJSONFile lookups
            constraints' <- readJSONFile constraints
            let result = makeTransaction pparams' nwid' lookups' constraints'
            BSL.putStrLn (Aeson.encode $ fmap Aeson.toJSON result)
        _ -> do
            die "usage: pab-mktx protocol-parameters.json network-id scriptlookups.json txconstraints.json"


makeTransaction :: C.ProtocolParameters
                -> C.NetworkId
                -> ScriptLookups Any
                -> UntypedConstraints
                -> Either Aeson.Value ExportTx
makeTransaction params nwid lookups tx_constraints =
    case mkTx lookups tx_constraints of
        Left err   -> Left (Aeson.toJSON err)
        Right ubtx -> either (Left . Aeson.toJSON) Right (export params nwid ubtx)

#endif

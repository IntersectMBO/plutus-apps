{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
#if defined(__GHCJS__)
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

-- XXX remove these warnings after code cleanup
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-matches -Wno-name-shadowing -Wno-redundant-constraints #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
#endif

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Aeson as Aeson hiding ((.=)) -- why is this hiding needed?
import Data.ByteString.Lazy qualified as BSL
import Data.Word (Word32)
import GHC.Generics
import Ledger.Constraints.OffChain (ScriptLookups (..), mkTx)
import Ledger.Constraints.TxConstraints (InputConstraint (..), OutputConstraint (..), TxConstraint (..),
                                         TxConstraints (..), UntypedConstraints)
import Ledger.Typed.TypeUtils (Any)
import Plutus.Contract.Wallet (ExportTx, export)
import System.Environment (getArgs)
import System.Exit (die)

import Ledger (unitRedeemer)
import Plutus.V1.Ledger.Ada qualified as Ada

#if defined(ghcjs_HOST_OS)
-- hopefully the correct imports
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import Data.JSString
import Data.ByteString (ByteString)
import PlutusTx.Builtins (BuiltinData)
import Data.Maybe

import qualified Ledger.Constraints as Constraints

#endif

deriving instance Generic C.NetworkId
deriving instance ToJSON C.NetworkMagic
deriving instance FromJSON C.NetworkMagic

deriving instance ToJSON C.NetworkId
deriving instance FromJSON C.NetworkId

{-
   How to get some input data?
     protocol-parameters: protocol-parameters.json in the plutus-use-cases/scripts directory
     network-id:          use 'mainnet' or a number indicating the testnet id
     scriptlookups:       generate some with pab-mktx-gendata
     txconstraints:       generate some with pab-mktx-gendata

 -}

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

   The following seems to work in a standalone test, if we add it to
   the all.js file instead of the standard 'h$main(h$mainZCZCMainzimain)':

       h$runSync(h$mainZCZCMainzimain);
       console.log(plutus_mkTx({ thisValueIs: "invalid" }));

   The correct input is a JSON object with protocolParameters, networkId,
   scriptLookups and constraints keys.

   I forgot if NetworkId had a FromJSON instance, otherwise we can
   make the networkId field a 'Maybe Word32' and convert accordingly, like
   in the non-GHCJS main:

               let nwid' = if nwid == "mainnet"
                        then C.Mainnet
                        else C.Testnet . C.NetworkMagic . read $ nwid
 -}
main :: IO ()
main = do
  exportCallback <- syncCallback1' makeTransaction'
  js_exportAPI (jsval exportCallback)


data MakeTransaction = MakeTransaction
                     { protocolParameters :: C.ProtocolParameters
                     , networkId          :: C.NetworkId
                     , scriptLookups      :: ScriptLookups Any
                     , constraints        :: UntypedConstraints
                     }
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- our wrapped makeTransaction function takes a JSON value and returns one
makeTransaction' :: JSVal -> IO JSVal
makeTransaction' x = do
  mbVal <- fromJSVal x
  let r = case mbVal of
            Just val ->
              case Aeson.fromJSON val of
                    Aeson.Success mt -> makeTransaction (protocolParameters mt)
                                                        (networkId mt)
                                                        (scriptLookups mt)
                                                        (constraints mt)
                    Aeson.Error e    -> Left (toJSON e)
            Nothing -> Left (toJSON ("invalid argument"::String))
  toJSVal (Aeson.toJSON r)

{-
  export the API to a global variable here, 'window' in case of
  browser, 'global' in case of nodejs. we can also link a js file
  with js-sources and have a function for this.

  If we want our library to work with 'require' in nodejs we should
  modify the 'exports' object instead.

  And we might want to wrap the whole script into its own scope later

 -}
foreign import javascript unsafe
  "(typeof window !== 'undefined' ? window : global).plutus_mkTx = $1;"
  js_exportAPI :: JSVal -> IO ()
#else

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
#endif

{-
  The function we intend to expose to JavaScript

    - All parameters can be supplied as a JSON value based on their
      Aeson FromJSON/ToJSON instances.
    - The result/error is also returned as a JSON value, based on the
      standard schema.

  Questions:
    - Is this the correct functionality to have in JS
    - We use the types
          UntypedConstraints
          ScriptLookups Any
      Are these the best types to use here, or do we need anything to support
      other BuiltinData related types?
    - How do we query the chain index in JS, do we need to make more Haskell
      functionality available for this?

 -}

makeTransaction :: C.ProtocolParameters
                -> C.NetworkId
                -> ScriptLookups Any
                -> UntypedConstraints
                -> Either Aeson.Value ExportTx
makeTransaction params nwid lookups tx_constraints =
    case mkTx lookups tx_constraints of
        Left err   -> Left (Aeson.toJSON err)
        Right ubtx -> either (Left . Aeson.toJSON) Right (export params nwid ubtx)

-- XXX start move to API module

{-
  Do we handle the types we need to pass in individually, or use some
  kind of generic approach (there are some swagger schema stuffs)

  Types we need to marshall:

  in only:
    BuiltinData

      looks like we need to CBOR this stuff up

      newtype BuiltinData = BuiltinData PLC.Data
        deriving newtype (Haskell.Show, Haskell.Eq, Haskell.Ord)


        data Data =
        Constr Integer [Data]
      | Map [(Data, Data)]
      | List [Data]
      | I Integer
      | B BS.ByteString
      deriving stock (Show, Eq, Ord, Generic)
      deriving anyclass (NFData)

    Datum ( Plutus.V1.Ledger.Scripts )

      -- | 'Datum' is a wrapper around 'Data' values which are used as data in transaction outputs.
      newtype Datum = Datum { getDatum :: BuiltinData  }
        deriving stock (Generic, Haskell.Show)
        deriving newtype (Haskell.Eq, Haskell.Ord, Eq, ToData, FromData, UnsafeFromData)
        deriving (ToJSON, FromJSON, Serialise, NFData) via PLC.Data
        deriving Pretty via PLC.Data

    DatumHash ( Plutus.V1.Ledger.Scripts )

        pass in as Uint8Array or hex-string

        -- | Script runtime representation of a @Digest SHA256@.
        newtype DatumHash =
            DatumHash Builtins.BuiltinByteString
            deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
            deriving stock (Generic)
            deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, ToData, FromData, UnsafeFromData)
            deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey, NFData)

    PaymentPubKeyHash ( Ledger.Address )

            probably pass in as Uint8Array or hex-string

            ??? XXX find definition

    StakePubKeyHash ( Ledger.Address )

            probably pass in as Uint8Array or hex-string

            ??? XXX find definition

    ValidatorHash ( Plutus.V1.Ledger.Scripts )

        pass in as Uint8Array or hex-string

        -- | Script runtime representation of a @Digest SHA256@.
        newtype ValidatorHash =
            ValidatorHash Builtins.BuiltinByteString
            deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
            deriving stock (Generic)
            deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, ToData, FromData, UnsafeFromData)
            deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey, NFData)

    Value ( Plutus.V1.Ledger.Value )

      probably make some user-friendly JSON representation for this

      newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
          deriving stock (Generic)
          deriving anyclass (ToJSON, FromJSON, Hashable, NFData)
          deriving newtype (Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
          deriving Pretty via (PrettyShow Value)

    MintingPolicyHash ( Plutus.V1.Ledger.Scripts )

        pass in as Uint8Array or hex-string

        -- | Script runtime representation of a @Digest SHA256@.
        newtype MintingPolicyHash =
            MintingPolicyHash Builtins.BuiltinByteString
            deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
            deriving stock (Generic)
            deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, ToData, FromData, UnsafeFromData)
            deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey)


    TokenName ( Plutus.V1.Ledger.Value )

        pass in as Uint8Array (assumes UTF-8 encoding) or JS string

        -- | ByteString of a name of a token, shown as UTF-8 string when possible
        newtype TokenName = TokenName { unTokenName :: PlutusTx.BuiltinByteString }
            deriving (Serialise) via LedgerBytes
            deriving stock (Generic)
            deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
            deriving anyclass (Hashable, NFData)
            deriving Pretty via (PrettyShow TokenName)

    Integer
      - string or javascript number
      - javascript numbers may overflow, always return string?

    Redeemer ( Plutus.V1.Ledger.Scripts )

      - pass in cbor encoded Uint8Array / hex-string? how do we get a redeemer in the first place?

      -- | 'Redeemer' is a wrapper around 'Data' values that are used as redeemers in transaction inputs.
      newtype Redeemer = Redeemer { getRedeemer :: BuiltinData }
        deriving stock (Generic, Haskell.Show)
        deriving newtype (Haskell.Eq, Haskell.Ord, Eq)
        deriving (ToJSON, FromJSON, Serialise, NFData, Pretty) via PLC.Data

    POSIXTimeRange ( Plutus.V1.Ledger.Time )

      - for posix timestamps, accept integer, integer-string, number, JS Date object?

      -- data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
      --    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
      --    deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)

      -- POSIXTime JSON instance parses an integer value


    Map TxOutRef ( Plutus.V1.Ledger.Tx ) ChainIndexTxOut ( Ledger.Tx )

         this one is complicated
         not sure how we'd get the data for making this value in JS
         should we use the swagger OpenApi.ToSchema for the conversion?

      -- | A reference to a transaction output. This is a
      -- pair of a transaction reference, and an index indicating which of the outputs
      -- of that transaction we are referring to.
      data TxOutRef = TxOutRef {
          txOutRefId  :: TxId,
          txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
          }
          deriving stock (Show, Eq, Ord, Generic)
          deriving anyclass (Serialise, ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)


      data ChainIndexTxOut =
          PublicKeyChainIndexTxOut { _ciTxOutAddress :: Address
                                  , _ciTxOutValue   :: Value
                                  }
        | ScriptChainIndexTxOut { _ciTxOutAddress   :: Address
                                , _ciTxOutValidator :: Either ValidatorHash Validator
                                , _ciTxOutDatum     :: Either DatumHash Datum
                                , _ciTxOutValue     :: Value
                                }
        deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON, OpenApi.ToSchema)


    TypedValidator ( Ledger.Typed.Scripts )

  in/out:
    UnbalancedTx ( Ledger.Constraints.Offchain )

      data UnbalancedTx =
          UnbalancedTx
              { unBalancedTxTx                  :: Tx
              , unBalancedTxRequiredSignatories :: Map PaymentPubKeyHash (Maybe PaymentPubKey)
              , unBalancedTxUtxoIndex           :: Map TxOutRef ScriptOutput
              , unBalancedTxValidityTimeRange   :: POSIXTimeRange
              }
          deriving stock (Eq, Generic, Show)
          deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

    UntypedTxConstraints ( Ledger.Constraints.TxConstraints )

      data TxConstraints i o =
          TxConstraints
              { txConstraints :: [TxConstraint]
              , txOwnInputs   :: [InputConstraint i]
              , txOwnOutputs  :: [OutputConstraint o]
              }
          deriving stock (Haskell.Show, Generic)

    ScriptLookups ( Ledger.Constraints.OffChain )

        via tagged aeson?

        data ScriptLookups a =
        ScriptLookups
            { slMPS                  :: Map MintingPolicyHash MintingPolicy
            -- ^ Minting policies that the script interacts with
            , slTxOutputs            :: Map TxOutRef ChainIndexTxOut
            -- ^ Unspent outputs that the script may want to spend
            , slOtherScripts         :: Map ValidatorHash Validator
            -- ^ Validators of scripts other than "our script"
            , slOtherData            :: Map DatumHash Datum
            -- ^ Datums that we might need
            , slPaymentPubKeyHashes  :: Map PaymentPubKeyHash PaymentPubKey
            -- ^ Public keys that we might need
            , slTypedValidator       :: Maybe (TypedValidator a)
            -- ^ The script instance with the typed validator hash & actual compiled program
            , slOwnPaymentPubKeyHash :: Maybe PaymentPubKeyHash
            -- ^ The contract's payment public key hash, used for depositing tokens etc.
            , slOwnStakePubKeyHash   :: Maybe StakePubKeyHash
            -- ^ The contract's stake public key hash (optional)
            } deriving stock (Show, Generic)
              deriving anyclass (ToJSON, FromJSON)

 -}

-- XXX catch all exceptions and have a wrapper that raises the
--     Left value as an exception
type JSResult = Either JSVal JSVal

-- a JavaScript data converter
data JSData a = JSData { convertIn   :: Maybe (JSVal -> IO (Maybe a))
                       , describeIn  :: JSString
                       , convertOut  :: Maybe (a -> IO JSVal)
                       , describeOut :: JSString
                       , dataName    :: JSString
                       }


-- XXX convert hex string or Uint8Array to ByteString
convertInBinaryData :: JSVal -> IO (Maybe ByteString)
convertInBinaryData x = undefined

-- XXX convert to Uint8Array
convertOutBinaryData :: ByteString -> IO JSVal
convertOutBinaryData x = undefined

jsBuiltinData :: JSData BuiltinData
jsBuiltinData = JSData { convertIn = Just (\x -> undefined) -- XXX first use convertInBinaryData, then CBOR decode it?
                       , describeIn = "a Uint8Array or hexadecimal encoded string"
                       , convertOut = Just undefined -- convertOutBinaryData
                       , describeOut = "a Uint8Array"
                       , dataName = "BuiltinData"
                       }


jsTxConstraints :: JSData UntypedConstraints
jsTxConstraints = taggedAesonJSData "TxConstraints"

jsScriptLookups :: JSData (ScriptLookups Any)
jsScriptLookups = taggedAesonJSData "ScriptLookups"

{-
  Marshall via Aeson instance. JSON objects are tagged with a type
  name so we can give resonable errors if things are mixed up.

  JSON looks like:

      { __tag: HaskellTypeName
      , __val: { aeson produced value }
      }

  The user is not supposed to construct/inspect these directly
  from the JS side.
 -}
taggedAesonJSData :: (Aeson.FromJSON a, Aeson.ToJSON a)
                  => JSString -> JSData a
taggedAesonJSData name = JSData
  { convertIn = Just $ \x -> do
      mbVal <- fromJSVal x
      pure $ case mbVal of
            -- XXX we should move the actual value to val.__val, and check that val.__tag == name
            -- XXX we should be able to report mismatched tags here, so Maybe is probably not a good result value
            Just val -> 
              case Aeson.fromJSON val of
                    Aeson.Success v -> Just v
                    Aeson.Error e   -> Nothing
            Nothing -> Nothing
  , describeIn  = name <> " JSON object"
  , convertOut  = undefined
  , describeOut = name <> " JSON object"
  , dataName    = name
  }

mkApi1 :: (a -> r)
       -> JSData a
       -> JSData r
       -> IO (JSVal -> IO JSVal)
mkApi1 f in1 out = pure $ \js_x -> do
  mb_x <- fromJust (convertIn in1) js_x
  case mb_x of
    Nothing -> undefined -- should be able to raise JS exception
    Just x -> fromJust (convertOut out) (f x)

mkApi2 :: (a -> b -> r)
       -> JSData a
       -> JSData b
       -> JSData r
       -> IO (JSVal -> JSVal -> IO JSVal)
mkApi2 f in1 in2 out = undefined

api :: [Pair]
api =
  [ "constraints" .=
    [ -- mustPayToTheScript :: forall i o. PlutusTx.ToData o => o -> Value -> TxConstraints i o
      "mustPayToTheScript"              .= jsid -- undefined
    , -- mustPayToPubKey :: forall i o. PaymentPubKeyHash -> Value -> TxConstraints i o
      "mustPayToPubKey"                 .= jsid -- undefined
    , -- mustPayToPubKeyAddress :: forall i o. PaymentPubKeyHash -> StakePubKeyHash -> Value -> TxConstraints i o
      "mustPayToPubKeyAddress"          .= jsid -- undefined
    , -- mustPayWithDatumToPubKey :: forall i o. PaymentPubKeyHash -> Datum -> Value -> TxConstraints i o
      "mustPayWithDatumToPubKey"        .= jsid -- undefined
    , -- mustPayWithDatumToPubKeyAddress :: forall i o. PaymentPubKeyHash -> StakePubKeyHash -> Datum -> Value -> TxConstraints i o
      "mustPayWithDatumToPubKeyAddress" .= jsid -- undefined
      -- mustMintCurrency :: forall i o. MintingPolicyHash -> TokenName -> Integer -> TxConstraints i o
    , "mustMintCurrency"                .= jsid -- undefined
    , -- mustMintCurrencyWithRedeemer :: forall i o. MintingPolicyHash -> Redeemer -> TokenName -> Integer -> TxConstraints i o
      "mustMintCurrencyWithRedeemer"    .= jsid -- undefined
    , -- mustMintValue :: forall i o. Value -> TxConstraints i o
      "mustMintValue"                   .= jsid -- undefined
    , -- mustMintValueWithRedeemer :: forall i o. Redeemer -> Value -> TxConstraints i o
      "mustMintValueWithRedeemer"       .= jsid -- undefined
    , -- mustSpendAtLeast :: forall i o. Value -> TxConstraints i o
      "mustSpendAtLeast"                .= jsid -- undefined
    , -- mustSpendPubKeyOutput :: forall i o. TxOutRef -> TxConstraints i o
      "mustSpendPubKeyOutput"           .= jsid -- undefined
    , -- mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o
      "mustSpendScriptOutput"           .= jsid -- undefined
    , -- mustValidateIn :: forall i o. POSIXTimeRange -> TxConstraints i o
      "mustValidateIn"                  .= jsid -- undefined
    , -- mustBeSignedBy :: forall i o. PaymentPubKeyHash -> TxConstraints i o
      "mustBeSignedBy"                  .= jsid -- undefined
    , -- mustProduceAtLeast :: forall i o. Value -> TxConstraints i o
      "mustProduceAtLeast"              .= jsid -- undefined
    , -- mustIncludeDatum :: forall i o. Datum -> TxConstraints i o
      "mustIncludeDatum"                .= jsid -- undefined
    , -- mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
      "mustPayToOtherScript"            .= jsid -- undefined
    , -- mustHashDatum :: DatumHash -> Datum -> TxConstraints i o
      "mustHashDatum"                   .= jsid -- undefined
    , -- mustSatisfyAnyOf :: forall i o. [TxConstraints i o] -> TxConstraints i o
      "mustSatisfyAnyOf"                .= jsid -- undefined
    ]
  , "offchain" .=
    [ -- typedValidatorLookups :: TypedValidator a -> ScriptLookups a
      "typedValidatorLookups"           .= jsid -- undefined
    , -- unspentOutputs :: Map TxOutRef ChainIndexTxOut -> ScriptLookups a
      "unspentOutputs"                  .= jsid -- undefined
    , -- mintingPolicy :: MintingPolicy -> ScriptLookups a
      "mintingPolicy"                   .= jsid -- undefined
    , -- otherScript :: Validator -> ScriptLookups a
      "otherScript"                     .= jsid -- undefined
    , -- otherData :: Datum -> ScriptLookups a
      "otherData"                       .= jsid -- undefined
    , -- ownPaymentPubKeyHash :: PaymentPubKeyHash -> ScriptLookups a
      "ownPaymentPubKeyHash"            .= jsid -- undefined
    , -- ownStakePubKeyHash :: StakePubKeyHash -> ScriptLookups a
      "ownStakePubKeyHash"              .= jsid -- undefined 
    , -- makeTransaction :: C.ProtocolParameters -> C.NetworkId -> ScriptLookups Any -> UntypedConstraints -> Either Aeson.Value ExportTx
      "mkTx"                            .= jsid -- undefined
    , -- paymentPubKey :: PaymentPubKey -> ScriptLookups a
      "paymentPubKey"                   .= jsid -- undefined
    , -- adjustUnbalancedTx :: UnbalancedTx -> UnbalancedTx
      "adjustUnbalancedTx"              .= jsid -- undefined
    ]
  ]

jsid :: JSVal -> JSVal
jsid x = x

-- XXX end move to API module

-- XXX start move to Util module

data Pair = Pair JSString (IO JSVal)

(.=) :: ApiExport a => JSString -> a -> Pair
key .= exp = Pair key (exportApi exp)

class ApiExport a where
    exportApi :: a -> IO JSVal

instance ApiExport JSVal where
    exportApi x = pure x

instance ApiExport Aeson.Value where
    exportApi v = toJSVal v

instance ApiExport Double where
    exportApi v = toJSVal v

-- instance ApiExport String where
--    exportApi xs = toJSVal (toJSString xs)

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
foreign import javascript unsafe "jslib_util_initAPI($1);" js_initAPI :: JSVal -> IO ()

-- XXX end move to Util module
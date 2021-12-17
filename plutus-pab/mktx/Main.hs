{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
#if defined(__GHCJS__)
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
#endif

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Aeson as Aeson
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

#if defined(__GHCJS__)
-- hopefully the correct imports
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
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

#if defined(__GHCJS__)

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

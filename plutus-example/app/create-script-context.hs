{-# LANGUAGE GADTs #-}
import Prelude

import Cardano.Api

import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Lazy qualified as LB
import Data.Foldable
import Data.Word
import Options.Applicative
import Options.Applicative qualified as Opt

import PlutusExample.ScriptContextChecker

main :: IO ()
main = runScriptContextCmd =<< customExecParser pref opts

pref :: ParserPrefs
pref = Opt.prefs showHelpOnEmpty

opts :: ParserInfo ScriptContextCmd
opts = Opt.info (parseScriptContextCmd <**> Opt.helper) Opt.fullDesc

parseScriptContextCmd :: Parser ScriptContextCmd
parseScriptContextCmd = parseGenerateDummy <|> parseGenerateTxBody
 where
  parseGenerateDummy :: Parser ScriptContextCmd
  parseGenerateDummy =
    GenerateDummyScriptContextRedeemer
      <$> pPlutusScriptLanguage
      <*> strOption
            ( long "out-file"
            <> metavar "FILE"
            <> help "Create a dummy script context redeemer. Redeeemer output filepath."
            <> Opt.completer (Opt.bashCompleter "file")
            )


  parseGenerateTxBody :: Parser ScriptContextCmd
  parseGenerateTxBody =
    GenerateScriptContextRedeemerTxBody
      <$> strOption ( long "generate-tx"
                    <> metavar "FILE"
                    <> help "Create a script context from a tx body."
                    <> Opt.completer (Opt.bashCompleter "file")
                    )
      <*> pPlutusScriptLanguage
      <*> pConsensusModeParams
      <*> pNetworkId
      <*> strOption ( long "out-file"
                    <> metavar "FILE"
                    <> help "Redeeemer output filepath."
                    <> Opt.completer (Opt.bashCompleter "file")
                    )

data ScriptContextCmd
  = GenerateDummyScriptContextRedeemer
      AnyScriptLanguage --TODO: Replace type with LedgerPlutusVersion when it becomes available
      FilePath
  | GenerateScriptContextRedeemerTxBody
      FilePath
      AnyScriptLanguage --TODO: Replace type with LedgerPlutusVersion when it becomes available
      AnyConsensusModeParams
      NetworkId
      FilePath

runScriptContextCmd :: ScriptContextCmd -> IO ()
runScriptContextCmd (GenerateDummyScriptContextRedeemer (AnyScriptLanguage sVer) outFp) =
  case sVer of
    PlutusScriptLanguage PlutusScriptV1 -> LB.writeFile outFp sampleTestV1ScriptContextDataJSON
    PlutusScriptLanguage PlutusScriptV2 -> LB.writeFile outFp sampleTestV2ScriptContextDataJSON
    err -> error $ "GenerateDummyScriptContextRedeemer: cannot create a redeemer for a non-Plutus script." <>
                   " Script type: " <> show err
runScriptContextCmd (GenerateScriptContextRedeemerTxBody txbodyfile (AnyScriptLanguage sVer) cModeParams nid outFp) = do
    case sVer of
      SimpleScriptLanguage _ -> error "runScriptContextCmd: Not possible to specify a simple script"
      PlutusScriptLanguage pScriptVer -> do

        eTxBodyRedeemer <- runExceptT $ createAnyCustomRedeemerBsFromTxFp pScriptVer txbodyfile cModeParams nid
        case eTxBodyRedeemer of
          Left err -> error $ "Error creating redeemer from: " <> txbodyfile <>
                              " Error: " <> show err
          Right redeemer -> liftIO $ LB.writeFile outFp redeemer


pPlutusScriptLanguage :: Parser AnyScriptLanguage
pPlutusScriptLanguage =
  Opt.flag' (AnyScriptLanguage $ PlutusScriptLanguage PlutusScriptV1)
    (  Opt.long "plutus-v1"
    <> Opt.help "Specify the version of the script context you are trying to recreate."
    ) <|>
  Opt.flag' (AnyScriptLanguage $ PlutusScriptLanguage PlutusScriptV2)
    (  Opt.long "plutus-v2"
    <> Opt.help "Specify the version of the script context you are trying to recreate."
    )

pConsensusModeParams :: Parser AnyConsensusModeParams
pConsensusModeParams = asum
  [ Opt.flag' (AnyConsensusModeParams ShelleyModeParams)
      (  Opt.long "shelley-mode"
      <> Opt.help "For talking to a node running in Shelley-only mode."
      )
  , Opt.flag' ()
      (  Opt.long "byron-mode"
      <> Opt.help "For talking to a node running in Byron-only mode."
      )
       *> pByronConsensusMode
  , Opt.flag' ()
      (  Opt.long "cardano-mode"
      <> Opt.help "For talking to a node running in full Cardano mode (default)."
      )
       *> pCardanoConsensusMode
  , -- Default to the Cardano consensus mode.
    pure . AnyConsensusModeParams . CardanoModeParams $ EpochSlots defaultByronEpochSlots
  ]
 where
   pCardanoConsensusMode :: Parser AnyConsensusModeParams
   pCardanoConsensusMode = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots
   pByronConsensusMode :: Parser AnyConsensusModeParams
   pByronConsensusMode = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

pNetworkId :: Parser NetworkId
pNetworkId =
  pMainnet' <|> fmap Testnet pTestnetMagic
 where
   pMainnet' :: Parser NetworkId
   pMainnet' =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

pEpochSlots :: Parser EpochSlots
pEpochSlots =
  EpochSlots <$>
    Opt.option Opt.auto
      (  Opt.long "epoch-slots"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of slots per epoch for the Byron era."
      <> Opt.value defaultByronEpochSlots -- Default to the mainnet value.
      <> Opt.showDefault
      )


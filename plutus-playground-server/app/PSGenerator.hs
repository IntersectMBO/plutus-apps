{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PSGenerator
    ( generate
    ) where

import Auth qualified
import Control.Applicative ((<|>))
import Control.Lens (itraverse, set, (&))
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.Except.Extras (mapError)
import Control.Monad.Freer.Extras.Log qualified as Log
import Control.Monad.IO.Class (MonadIO)
import Crowdfunding qualified
import CrowdfundingSimulations qualified
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Encode.Pretty qualified as JSON
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding qualified as T (decodeUtf8, encodeUtf8)
import Data.Time.Units (Second)
import ErrorHandling qualified
import ErrorHandlingSimulations qualified
import Game qualified
import GameSimulations qualified
import HelloWorld qualified
import HelloWorldSimulations qualified
import Interpreter qualified as Webghc
import Language.Haskell.Interpreter (CompilationError, InterpreterError, InterpreterResult (InterpreterResult),
                                     SourceCode (SourceCode), Warning, result, warnings)
import Language.PureScript.Bridge (BridgePart, Language (Haskell), SumType, buildBridge, equal, genericShow, mkSumType,
                                   order, writePSTypesWith)
import Language.PureScript.Bridge.CodeGenSwitches (ForeignOptions (ForeignOptions), genForeign,
                                                   unwrapSingleConstructors)
import Language.PureScript.Bridge.TypeParameters (A)
import Ledger.CardanoWallet qualified as CW
import Ledger.Tx.CardanoAPI (ToCardanoError)
import PSGenerator.Common qualified
import Playground.API qualified as API
import Playground.Interpreter qualified as PI
import Playground.Types (CompilationResult (CompilationResult), ContractCall, ContractDemo (ContractDemo),
                         Evaluation (Evaluation), EvaluationResult, FunctionSchema, KnownCurrency,
                         PlaygroundError (InterpreterError), Simulation (Simulation), SimulatorAction, SimulatorWallet,
                         contractDemoContext, contractDemoEditorContents, contractDemoName, contractDemoSimulations,
                         functionSchema, knownCurrencies, program, simulationActions, simulationWallets, sourceCode,
                         wallets)
import Playground.Usecases (crowdFunding, errorHandling, game, starter, vesting)
import Playground.Usecases qualified as Usecases
import Plutus.Contract.Checkpoint (CheckpointKey, CheckpointLogMsg)
import Schema (FormSchema, formArgumentToJson)
import Servant ((:<|>))
import Servant.PureScript (HasBridge, Settings, _generateSubscriberAPI, apiModuleName, defaultBridge, defaultSettings,
                           languageBridge, writeAPIModuleWithSettings)
import Starter qualified
import StarterSimulations qualified
import System.FilePath ((</>))
import Vesting qualified
import VestingSimulations qualified
import Wallet.API (WalletAPIError)
import Wallet.Emulator.Chain qualified as EM
import Wallet.Emulator.LogMessages qualified as EM
import Wallet.Emulator.MultiAgent qualified as EM
import Wallet.Emulator.NodeClient qualified as EM
import Wallet.Emulator.Wallet qualified as EM
import Wallet.Rollup.Types (AnnotatedTx, BeneficialOwner, DereferencedInput, SequenceId, TxKey)

myBridge :: BridgePart
myBridge =
    PSGenerator.Common.aesonBridge <|>
    PSGenerator.Common.containersBridge <|>
    PSGenerator.Common.languageBridge <|>
    PSGenerator.Common.ledgerBridge <|>
    PSGenerator.Common.servantBridge <|>
    PSGenerator.Common.miscBridge <|>
    defaultBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.playgroundTypes <>
    [ (genericShow <*> (equal <*> mkSumType)) (Proxy @CompilationResult)
    , (genericShow <*> (equal <*> mkSumType)) (Proxy @Warning)
    , (genericShow <*> (equal <*> mkSumType)) (Proxy @SourceCode)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @EM.Wallet)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @CW.WalletNumber)
    , (genericShow <*> (equal <*> mkSumType)) (Proxy @Simulation)
    , (genericShow <*> (equal <*> mkSumType)) (Proxy @ContractDemo)
    , (genericShow <*> (equal <*> mkSumType)) (Proxy @SimulatorWallet)
    , (genericShow <*> mkSumType) (Proxy @CompilationError)
    , (genericShow <*> mkSumType) (Proxy @Evaluation)
    , (genericShow <*> mkSumType) (Proxy @EvaluationResult)
    , (genericShow <*> mkSumType) (Proxy @EM.EmulatorEvent')
    , (genericShow <*> mkSumType) (Proxy @(EM.EmulatorTimeEvent A))
    , (genericShow <*> mkSumType) (Proxy @EM.ChainEvent)
    , (genericShow <*> mkSumType) (Proxy @Log.LogLevel)
    , (genericShow <*> mkSumType) (Proxy @(Log.LogMessage A))
    , (genericShow <*> mkSumType) (Proxy @EM.WalletEvent)
    , (genericShow <*> mkSumType) (Proxy @EM.NodeClientEvent)
    , (genericShow <*> mkSumType) (Proxy @PlaygroundError)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @WalletAPIError)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ToCardanoError)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @SequenceId)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @AnnotatedTx)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @DereferencedInput)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @BeneficialOwner)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxKey)
    , (genericShow <*> mkSumType) (Proxy @InterpreterError)
    , (genericShow <*> (equal <*> mkSumType)) (Proxy @(InterpreterResult A))
    , (genericShow <*> mkSumType) (Proxy @CheckpointLogMsg)
    , (genericShow <*> mkSumType) (Proxy @CheckpointKey)
    , (genericShow <*> mkSumType) (Proxy @EM.RequestHandlerLogMsg)
    , (genericShow <*> mkSumType) (Proxy @EM.TxBalanceMsg)
    ]

mySettings :: Settings
mySettings =
    (defaultSettings & set apiModuleName "Playground.Server")
        {_generateSubscriberAPI = False}

multilineString :: Text -> Text -> Text
multilineString name value =
    "\n\n" <> name <> " :: String\n" <> name <> " = \"\"\"" <> value <> "\"\"\""

jsonExport :: ToJSON a => Text -> a -> Text
jsonExport name value =
    multilineString name (T.decodeUtf8 . BSL.toStrict $ JSON.encodePretty value)

sourceCodeExport :: Text -> SourceCode -> Text
sourceCodeExport name (SourceCode value) = multilineString name value

psModule :: Text -> Text -> Text
psModule name body = "module " <> name <> " where" <> body

------------------------------------------------------------
writeUsecases :: FilePath -> IO ()
writeUsecases outputDir = do
    let usecases =
            sourceCodeExport "vesting" vesting <>
            sourceCodeExport "game" game <>
            sourceCodeExport "crowdFunding" crowdFunding <>
            sourceCodeExport "errorHandling" errorHandling <>
            sourceCodeExport "starter" starter <>
            jsonExport "contractDemos" contractDemos
        usecasesModule = psModule "Playground.Usecases" usecases
    BS.writeFile
        (outputDir </> "Playground" </> "Usecases.purs")
        (T.encodeUtf8 usecasesModule)

------------------------------------------------------------
writeTestData :: FilePath -> IO ()
writeTestData outputDir = do
    let ContractDemo { contractDemoContext
                     , contractDemoSimulations
                     , contractDemoEditorContents
                     } = head contractDemos
    BSL.writeFile
        (outputDir </> "compilation_response.json")
        (JSON.encodePretty contractDemoContext)
    void $
        itraverse
            (\index ->
                 writeSimulation
                     (outputDir </> "evaluation_response" <>
                      show index <> ".json")
                     contractDemoEditorContents)
            contractDemoSimulations

writeSimulation :: FilePath -> SourceCode -> Simulation -> IO ()
writeSimulation filename sourceCode simulation = do
    result <- runExceptT $ runSimulation sourceCode simulation
    case result of
        Left err   -> fail $ "Error evaluating simulation: " <> show err
        Right json -> BSL.writeFile filename json

maxInterpretationTime :: Second
maxInterpretationTime = 80

runSimulation ::
       (MonadMask m, MonadError PlaygroundError m, MonadIO m)
    => SourceCode
    -> Simulation
    -> m BSL.ByteString
runSimulation sourceCode Simulation {simulationActions, simulationWallets} = do
    let evaluation =
            Evaluation
                { sourceCode
                , wallets = simulationWallets
                , program =
                      toJSON . encodeToText $ toExpression <$> simulationActions
                }
    expr <- PI.evaluationToExpr evaluation
    result <- mapError InterpreterError $ Webghc.compile maxInterpretationTime False (SourceCode expr)
    interpreterResult <- PI.decodeEvaluation result
    pure $ JSON.encodePretty interpreterResult

encodeToText :: ToJSON a => a -> Text
encodeToText = T.decodeUtf8 . BSL.toStrict . JSON.encode

toExpression :: SimulatorAction -> Maybe (ContractCall Text)
toExpression = traverse (fmap encodeToText . formArgumentToJson)

------------------------------------------------------------
generate :: FilePath -> IO ()
generate outputDir = do
    writeAPIModuleWithSettings
        mySettings
        outputDir
        myBridgeProxy
        (Proxy
             @(API.API
               :<|> Auth.FrontendAPI))
    writePSTypesWith
        (genForeign (ForeignOptions {unwrapSingleConstructors = True}))
        outputDir
        (buildBridge myBridge)
        myTypes
    writeUsecases outputDir
    writeTestData outputDir
    putStrLn $ "Done: " <> outputDir

------------------------------------------------------------
contractDemos :: [ContractDemo]
contractDemos =
    [ mkContractDemo
        "Hello, world"
        Usecases.helloWorld
        HelloWorldSimulations.simulations
        HelloWorld.schemas
        HelloWorld.registeredKnownCurrencies
    , mkContractDemo
          "Starter"
          Usecases.starter
          StarterSimulations.simulations
          Starter.schemas
          Starter.registeredKnownCurrencies
    , mkContractDemo
          "Game"
          Usecases.game
          GameSimulations.simulations
          Game.schemas
          Game.registeredKnownCurrencies
    , mkContractDemo
          "Vesting"
          Usecases.vesting
          VestingSimulations.simulations
          Vesting.schemas
          Vesting.registeredKnownCurrencies
    , mkContractDemo
          "Crowd Funding"
          Usecases.crowdFunding
          CrowdfundingSimulations.simulations
          Crowdfunding.schemas
          Crowdfunding.registeredKnownCurrencies
    , mkContractDemo
          "Error Handling"
          Usecases.errorHandling
          ErrorHandlingSimulations.simulations
          ErrorHandling.schemas
          ErrorHandling.registeredKnownCurrencies
    ]

mkContractDemo ::
       Text
    -> SourceCode
    -> [Simulation]
    -> [FunctionSchema FormSchema]
    -> [KnownCurrency]
    -> ContractDemo
mkContractDemo contractDemoName contractDemoEditorContents contractDemoSimulations functionSchema knownCurrencies =
    ContractDemo
        { contractDemoName
        , contractDemoEditorContents
        , contractDemoSimulations
        , contractDemoContext =
              InterpreterResult
                  { warnings = []
                  , result =
                        CompilationResult
                            {functionSchema, knownCurrencies}
                  }
        }

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Main(main) where

import Cardano.Api qualified as C
import Data.Default (Default (..))
import Data.Monoid (Sum (..))
import Options.Applicative
import Plutus.Contracts.Crowdfunding qualified as Crowdfunding
import Plutus.Contracts.Uniswap.Trace qualified as Uniswap
import Plutus.Trace (Command (..), ScriptsConfig (..), ValidatorMode (..), showStats, writeScriptsTo)
import Spec.Currency qualified as Currency
import Spec.Escrow qualified as Escrow
import Spec.Future qualified as Future
import Spec.Game.Alonzo qualified as Game
import Spec.GameStateMachine qualified as GameStateMachine
import Spec.MultiSig qualified as MultiSig
import Spec.MultiSigStateMachine qualified as MultiSigStateMachine
import Spec.PingPong qualified as PingPong
import Spec.Prism qualified as Prism
import Spec.PubKey qualified as PubKey
import Spec.Stablecoin qualified as Stablecoin
import Spec.TokenAccount qualified as TokenAccount
import Spec.Vesting qualified as Vesting

writeWhat :: Command -> String
writeWhat (Scripts FullyAppliedValidators) = "scripts (fully applied)"
writeWhat (Scripts UnappliedValidators)    = "scripts (unapplied)"
writeWhat Transactions{}                   = "transactions"
writeWhat MkTxLogs                         = "mkTx logs"

pathParser :: Parser FilePath
pathParser = strArgument (metavar "SCRIPT_PATH" <> help "output path")

protocolParamsParser :: Parser FilePath
protocolParamsParser = strOption (long "protocol-parameters" <> short 'p' <> help "Path to protocol parameters JSON file" <> showDefault <> value "protocol-parameters.json")

networkIdParser :: Parser C.NetworkId
networkIdParser =
    let p = C.Testnet . C.NetworkMagic <$> option auto (long "network-magic" <> short 'n' <> help "Cardano network magic. If none is specified, mainnet addresses are generated.")
    in p <|> pure C.Mainnet

commandParser :: Parser Command
commandParser = hsubparser $ mconcat [scriptsParser, transactionsParser, mkTxParser]

scriptsParser :: Mod CommandFields Command
scriptsParser =
    command "scripts" $
    info
        (Scripts <$> flag FullyAppliedValidators UnappliedValidators (long "unapplied-validators" <> short 'u' <> help "Write the unapplied validator scripts" <> showDefault))
        (fullDesc <> progDesc "Write fully applied validator scripts")

transactionsParser :: Mod CommandFields Command
transactionsParser =
    command "transactions" $
    info
        (Transactions <$> networkIdParser <*> protocolParamsParser)
        (fullDesc <> progDesc "Write partial transactions")

mkTxParser :: Mod CommandFields Command
mkTxParser =
    command "mktx" $
    info
        (pure MkTxLogs)
        (fullDesc <> progDesc "Write logs for 'mkTx' calls")

progParser :: ParserInfo ScriptsConfig
progParser =
    let p = ScriptsConfig <$> pathParser <*> commandParser
    in info
        (p <**> helper)
        (fullDesc
        <> progDesc "Run a number of emulator traces and write all validator scripts and/or partial transactions to SCRIPT_PATH"
        <> header "plutus-use-cases-scripts - extract validators and partial transactions from emulator traces"
        )

main :: IO ()
main = execParser progParser >>= writeScripts

writeScripts :: ScriptsConfig -> IO ()
writeScripts config = do
    putStrLn $ "Writing " <> writeWhat (scCommand config) <> " to: " <> scPath config
    (Sum size, exBudget) <- foldMap (uncurry3 (writeScriptsTo config))
        [ -- TODO: The revert of input-output-hk/cardano-node#3206 prevents us from using traces
          --       for the auction contract for now. Uncomment the following code whenever we
          --       have a proper implementation.
          --  ("auction_1", Auction.auctionTrace1, view emulatorConfig Auction.options)
          --, ("auction_2", Auction.auctionTrace2, view emulatorConfig Auction.options)
          ("crowdfunding-success", Crowdfunding.successfulCampaign, def)
        , ("currency", Currency.currencyTrace, def)
        , ("escrow-redeem_1", Escrow.redeemTrace, def)
        , ("escrow-redeem_2", Escrow.redeem2Trace, def)
        , ("escrow-refund", Escrow.refundTrace, def)
        , ("future-increase-margin", Future.increaseMarginTrace, def)
        , ("future-settle-early", Future.settleEarlyTrace, def)
        , ("future-pay-out", Future.payOutTrace, def)
        , ("game-success", Game.successTrace, def)
        , ("game-sm-success_1", GameStateMachine.successTrace, def)
        , ("game-sm-success_2", GameStateMachine.successTrace2, def)
        , ("multisig-success", MultiSig.succeedingTrace, def)
        , ("multisig-failure", MultiSig.failingTrace, def)
        , ("multisig-sm", MultiSigStateMachine.lockProposeSignPay 3 2, def)
        , ("ping-pong_1", PingPong.pingPongTrace, def)
        , ("ping-pong_2", PingPong.twoPartiesTrace, def)
        , ("prism", Prism.prismTrace, def)
        , ("pubkey", PubKey.pubKeyTrace, def)
        , ("stablecoin_1", Stablecoin.stablecoinTrace, def)
        , ("stablecoin_2", Stablecoin.maxReservesExceededTrace, def)
        , ("token-account", TokenAccount.tokenAccountTrace, def)
        , ("vesting", Vesting.retrieveFundsTrace, def)
        , ("uniswap", Uniswap.uniswapTrace, def)
        ]
    if size > 0 then
        putStrLn $ "Total " <> showStats size exBudget
    else pure ()

-- | `uncurry3` converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

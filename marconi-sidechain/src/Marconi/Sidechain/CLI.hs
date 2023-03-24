module Marconi.Sidechain.CLI where

import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs))
import Options.Applicative qualified as Opt

parseCli :: IO CliArgs
parseCli = Opt.execParser . programParser =<< Cli.getGitSha

programParser :: String -> Opt.ParserInfo CliArgs
programParser gitSha = Opt.info
  (Opt.helper <*> Cli.commonVersionOption gitSha <*> parserCliArgs)
  (Cli.marconiDescr "marconi-sidechain")

parserCliArgs :: Opt.Parser CliArgs
parserCliArgs = CliArgs
  <$> Cli.commonSocketPath
  <*> Opt.strOption
        (  Opt.long "node-config-path"
        <> Opt.help "Path to node configuration which you are connecting to."
        )
  <*> Cli.commonDbDir
  <*> Cli.commonMaybePort
  <*> Cli.pNetworkId
  <*> Cli.commonMaybeTargetAddress

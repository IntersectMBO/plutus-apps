
import Prelude

import Cardano.Api

import System.Directory
import System.FilePath.Posix ((</>))

import PlutusExample.PlutusVersion1.AlwaysFails as V1 (alwaysFailsScript)
import PlutusExample.PlutusVersion1.AlwaysSucceeds as V1 (alwaysSucceedsScript)
import PlutusExample.PlutusVersion1.CustomDatumRedeemerGuess as V1 (customGuessScript)
import PlutusExample.PlutusVersion1.DatumRedeemerGuess as V1 (guessScript, guessScriptStake)
import PlutusExample.PlutusVersion1.Loop as V1 (loopScript)
import PlutusExample.PlutusVersion1.MintingScript as V1 (apiExamplePlutusMintingScript)
import PlutusExample.PlutusVersion1.RedeemerContextScripts as V1 (scriptContextTextPayingScript, scriptContextTestMintingScript)
import PlutusExample.PlutusVersion1.Sum as V1 (sumScript)


import PlutusExample.PlutusVersion2.AlwaysFails as V2 (alwaysFailsScript)
import PlutusExample.PlutusVersion2.AlwaysSucceeds as V2 (alwaysSucceedsScript)
import PlutusExample.PlutusVersion2.CustomDatumRedeemerGuess as V2 (customGuessScript)
import PlutusExample.PlutusVersion2.DatumRedeemerGuess as V2 (guessScript, guessScriptStake)
import PlutusExample.PlutusVersion2.Loop as V2 (loopScript)
import PlutusExample.PlutusVersion2.MintingScript as V2 (apiExamplePlutusMintingScript)
import PlutusExample.PlutusVersion2.RequireRedeemer as V2 (requireRedeemerScript)
import PlutusExample.PlutusVersion2.Sum as V2 (sumScript)

main :: IO ()
main = do
  let v1dir = "generated-plutus-scripts/v1"
      v2dir = "generated-plutus-scripts/v2"
  createDirectoryIfMissing True v1dir
  createDirectoryIfMissing True v2dir

  _ <- writeFileTextEnvelope (v1dir </> "always-fails.plutus") Nothing V1.alwaysFailsScript
  _ <- writeFileTextEnvelope (v1dir </> "always-succeeds-spending.plutus") Nothing V1.alwaysSucceedsScript
  _ <- writeFileTextEnvelope (v1dir </> "guess-42-datum-42-txin.plutus") Nothing V1.guessScript
  _ <- writeFileTextEnvelope (v1dir </> "guess-42-stake.plutus") Nothing V1.guessScriptStake
  _ <- writeFileTextEnvelope (v1dir </> "custom-guess-42-datum-42.plutus") Nothing V1.customGuessScript
  _ <- writeFileTextEnvelope (v1dir </> "anyone-can-mint.plutus") Nothing V1.apiExamplePlutusMintingScript
  _ <- writeFileTextEnvelope (v1dir </> "sum.plutus") Nothing V1.sumScript
  _ <- writeFileTextEnvelope (v1dir </> "loop.plutus") Nothing V1.loopScript
  _ <- writeFileTextEnvelope (v1dir </> "context-equivalance-test.plutus") Nothing V1.scriptContextTextPayingScript
  _ <- writeFileTextEnvelope (v1dir </> "minting-context-equivalance-test.plutus") Nothing V1.scriptContextTestMintingScript

  _ <- writeFileTextEnvelope (v2dir </> "always-fails.plutus") Nothing V2.alwaysFailsScript
  _ <- writeFileTextEnvelope (v2dir </> "always-succeeds-spending.plutus") Nothing V2.alwaysSucceedsScript
  _ <- writeFileTextEnvelope (v2dir </> "guess-42-datum-42-txin.plutus") Nothing V2.guessScript
  _ <- writeFileTextEnvelope (v2dir </> "guess-42-stake.plutus") Nothing V2.guessScriptStake
  _ <- writeFileTextEnvelope (v2dir </> "custom-guess-42-datum-42.plutus") Nothing V2.customGuessScript
  _ <- writeFileTextEnvelope (v2dir </> "anyone-can-mint.plutus") Nothing V2.apiExamplePlutusMintingScript
  _ <- writeFileTextEnvelope (v2dir </> "sum.plutus") Nothing V2.sumScript
  _ <- writeFileTextEnvelope (v2dir </> "loop.plutus") Nothing V2.loopScript
  _ <- writeFileTextEnvelope (v2dir </> "required-redeemer.plutus") Nothing V2.requireRedeemerScript

  return ()

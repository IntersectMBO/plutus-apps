
import Prelude

import Cardano.Api

import System.Directory
import System.FilePath.Posix ((</>))

import PlutusExample.PlutusVersion1.AlwaysFails (alwaysFailsScript)
import PlutusExample.PlutusVersion1.AlwaysSucceeds (alwaysSucceedsScript)
import PlutusExample.PlutusVersion1.CustomDatumRedeemerGuess
import PlutusExample.PlutusVersion1.DatumRedeemerGuess (guessScript, guessScriptStake)
import PlutusExample.PlutusVersion1.Loop (loopScript)
import PlutusExample.PlutusVersion1.MintingScript (apiExamplePlutusMintingScript)
import PlutusExample.PlutusVersion1.RedeemerContextScripts
import PlutusExample.PlutusVersion1.Sum (sumScript)

import PlutusExample.PlutusVersion2.EcdsaSecp256k1Loop (v2EcdsaLoopScript)
import PlutusExample.PlutusVersion2.MintingScript (v2mintingScript)
import PlutusExample.PlutusVersion2.RedeemerContextEquivalence (v2ScriptContextEquivalenceScript, v2mintEquivScript)
import PlutusExample.PlutusVersion2.RequireRedeemer (requireRedeemerScript)
import PlutusExample.PlutusVersion2.SchnorrSecp256k1Loop (v2SchnorrLoopScript)
import PlutusExample.PlutusVersion2.StakeScript (v2StakeScript)

main :: IO ()
main = do
  let v1dir = "generated-plutus-scripts/v1"
      v2dir = "generated-plutus-scripts/v2"
  createDirectoryIfMissing True v1dir
  createDirectoryIfMissing True v2dir

  _ <- writeFileTextEnvelope (v1dir </> "always-fails.plutus") Nothing alwaysFailsScript
  _ <- writeFileTextEnvelope (v1dir </> "always-succeeds-spending.plutus") Nothing alwaysSucceedsScript
  _ <- writeFileTextEnvelope (v1dir </> "guess-42-datum-42-txin.plutus") Nothing guessScript
  _ <- writeFileTextEnvelope (v1dir </> "guess-42-stake.plutus") Nothing guessScriptStake
  _ <- writeFileTextEnvelope (v1dir </> "custom-guess-42-datum-42.plutus") Nothing customGuessScript
  _ <- writeFileTextEnvelope (v1dir </> "anyone-can-mint.plutus") Nothing apiExamplePlutusMintingScript
  _ <- writeFileTextEnvelope (v1dir </> "sum.plutus") Nothing sumScript
  _ <- writeFileTextEnvelope (v1dir </> "loop.plutus") Nothing loopScript
  _ <- writeFileTextEnvelope (v1dir </> "context-equivalance-test.plutus") Nothing scriptContextTextPayingScript
  _ <- writeFileTextEnvelope (v1dir </> "minting-context-equivalance-test.plutus") Nothing scriptContextTestMintingScript


  _ <- writeFileTextEnvelope (v2dir </> "required-redeemer.plutus") Nothing requireRedeemerScript
  _ <- writeFileTextEnvelope (v2dir </> "minting-script.plutus") Nothing v2mintingScript
  _ <- writeFileTextEnvelope (v2dir </> "stake-script.plutus") Nothing v2StakeScript
  _ <- writeFileTextEnvelope (v2dir </> "context-equivalence-test.plutus") Nothing v2ScriptContextEquivalenceScript
  _ <- writeFileTextEnvelope (v2dir </> "minting-context-equivalance-test.plutus") Nothing v2mintEquivScript
  _ <- writeFileTextEnvelope (v2dir </> "ecdsa-secp256k1-loop.plutus") Nothing v2EcdsaLoopScript
  _ <- writeFileTextEnvelope (v2dir </> "schnorr-secp256k1-loop.plutus") Nothing v2SchnorrLoopScript

  return ()

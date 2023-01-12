
<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-12

## Removed

- `mkUntypedMintingPolicyV1` replaced by a version agnostic function
- `mkUntypedMintingPolicyV2` replaced by a version agnostic function
- `mkUntypedStakeValidatorV1` replaced by a version agnostic function
- `mkUntypedStakeValidatorV2` replaced by a version agnostic function
- `mkUntypedValidatorV1` replaced by a version agnostic function
- `mkUntypedValidatorV2` replaced by a version agnostic function

## Added

- `Plutus.Script.Utils.Typed.ScriptContext` a type class that allow the creation
  of an untyped minting policy, stake validator or validator.
- an instance of `Plutus.Script.Utils.Typed.ScriptContext` for `Plutus. ledger.V1.Ledger.Context.ScriptContext`
- an instance of `Plutus.Script.Utils.Typed.ScriptContext` for `Plutus. ledger.V2.Ledger.Context.ScriptContext`

## Changed

- The default implementation of functions in the `IsScriptContext` typeclass now
  log which data they are trying to decode, to ease debugging when an invalid
  binary representation of a redeemer / value or script context is sent.

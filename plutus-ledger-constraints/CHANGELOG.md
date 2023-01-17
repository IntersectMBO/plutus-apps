
<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-12

## Added

- `ValidityInterval` is a type of interval — where the lower bound is closed and the upper bound is open — to provide a correct by construction tool for constraints.
- Functions to construct `ValidityInterval` and to convert it to the plutus `Interval` and backwards.
- `mustValidateInTimeRange` constraint as an alternative to `mustValidateIn` but it takes `ValidityInterval POSIXTime` instead.
- `mustValidateInSlotRange` constraint.

## Changed

- `MustValidateIn POSIXTimeRange` constraint was replaced with `MustValidateInTimeRange !(ValidityInterval POSIXTime)` to make the constraint's interface more precise by using `ValidityInterval` instead of `POSIXTimeRange` according to https://github.com/input-output-hk/plutus-apps/blob/main/doc/adr/0013-tx-validity-time-range-fix.rst.
- Removed off-chain checks of `MustIncludeDatumInTxWithHash` and `MustIncludeDatumInTx`. They now only verify
  the datums when used on-chain.

## Deprecated

- `mustValidateIn` was deprecated according to https://github.com/input-output-hk/plutus-apps/blob/main/doc/adr/0013-tx-validity-time-range-fix.rst.

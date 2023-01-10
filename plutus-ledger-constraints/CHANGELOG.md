
<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-10

## Changed

- Removed off-chain checks of `MustIncludeDatumInTxWithHash` and `MustIncludeDatumInTx`. They now only verify
  the datums when used on-chain.

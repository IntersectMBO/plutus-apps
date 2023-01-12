<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Removed

- A bullet item for the Removed category.

-->

### Added

- `ValidityInterval` is a type of interval — where the lower bound is closed and the upper bound is open — to provide a correct by construction tool for constraints.
- Functions to construct `ValidityInterval` and to convert it to the plutus `Interval` and backwards.
- `mustValidateInTimeRange` constraint as an alternative to `mustValidateIn` but it takes `ValidityInterval POSIXTime` instead.
- `mustValidateInSlotRange` constraint.

### Changed

- `MustValidateIn POSIXTimeRange` constraint was replaced with `MustValidateInTimeRange !(ValidityInterval POSIXTime)` to make the constraint's interface more precise by using `ValidityInterval` instead of `POSIXTimeRange` according to https://github.com/input-output-hk/plutus-apps/blob/main/doc/adr/0013-tx-validity-time-range-fix.rst.


### Deprecated

- `mustValidateIn` was deprecated according to https://github.com/input-output-hk/plutus-apps/blob/main/doc/adr/0013-tx-validity-time-range-fix.rst.

<!--
### Fixed

- A bullet item for the Fixed category.

-->
<!--
### Security

- A bullet item for the Security category.

-->

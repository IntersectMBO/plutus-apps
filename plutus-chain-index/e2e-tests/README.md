# Chain-index end-to-end tests

## Sanity API tests

### Pre-requisites:
- You have a testnet instance of chain-index running somewhere
- Chain-index is synced to at least `min_slot` in the environment file `sanity-tests/testnet-env.json`
- Update URL in environment file if needed

### Run tests:
- `nvm use` (not required for `nix develop` users)
- `npm i`
- `npm test`

## CI (todo)

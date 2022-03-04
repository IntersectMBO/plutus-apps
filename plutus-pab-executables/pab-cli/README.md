# PAB deployment CLI

This CLI provides dApp developpers a way to run their PAB webserver, which contain the builtin contracts, alongside the required components (mock or real).

The tool generates the correct configuration for each required component and ensures they are in sync.

Use `nix-shell deployment.nix` to have access to this tool.

## Usage

```
PAB CLI. This script allows the user to run the PAB in a hosted scenario, and provides good defaults for common usecases.

THIS IS AN EXPERIMENT! DO NOT USE IN A PRODUCTION ENVIRONMENT.

For any possible enhancements and suggestions, submit an issue on https://github.com/input-output-hk/plutus-apps/issues.

Usage: pab-cli
    pab-cli mocknet --pab-exe=<exe> [--pab-dir=<dir>] [--pab-db-pool-size=<size>] [--pab-port=<port>] [--pab-rollback-history=<n>] [--pab-passphrase] ([--pab-resume-from-block-id=<blockid> --pab-resume-fr
om-slot=<slot>]) [--chain-index-port=<port>] [--node-dir=<dir>] [--node-port=<port>] [--wallet-port=<port>]
    pab-cli (mainnet | testnet) wbe [(--pab-exe=<exe> [--pab-dir=<dir>] [--pab-db-pool-size=<size>] [--pab-port=<port>] [--pab-rollback-history=<n>] [--pab-passphrase] ([--pab-resume-from-block-id=<blocki
d> --pab-resume-from-slot=<slot>]))] [--chain-index-port=<port>] [--node-dir=<dir>] [--node-port=<port>] [--wallet-port=<port>]
    pab-cli (mainnet | testnet) remotewallet [(--pab-exe=<exe> [--pab-dir=<dir>] [--pab-db-pool-size=<size>] [--pab-port=<port>] [--pab-rollback-history=<n>] [--pab-passphrase] ([--pab-resume-from-block-i
d=<blockid> --pab-resume-from-slot=<slot>]))] [--chain-index-port=<port>] [--node-dir=<dir>] [--node-port=<port>]
    pab-cli -h|--help

Options:
    -h --help                             show this
    --pab-exe <exe>                       PAB executable with builtin contracts. Ex. "$(cabal list-bin plutus-pab-examples)"
    --pab-dir <dir>                       PAB output directory for config, logs, db, etc.
                                          [default: /tmp/pab]
    --pab-db-pool-size <size>             PAB database pool size
                                          [default: 20]
    --pab-port <port>                     PAB webserver port number
                                          [default: 9080]
    --pab-passphrase                      PAB wallet passphrase
    --pab-rollback-history <n>            PAB rollback history
    --pab-resume-from-block-id <blockid>  Block number to resume syncing from
    --pab-resume-from-slot <slot>         Slot number to resume syncing from
    --chain-index-port <port>             chain index port number
                                          [default: 9083]
    --node-dir <ndir>                     node output directory config, logs, db, etc.
                                          [default: /tmp/cardano-node]
    --node-port <port>                    node port number
                                          [default:9082]
    --wallet-port <port>                  wallet server port number
                                          [default:9081]
```

The tool currently supports the following three scenarios:

1- You want to run your PAB webserver with mocked components (node, wallet and chain-index).

2- You want to run the cardano-wallet backend, cardano node, chain-index and (optionally) your PAB webserver on the Cardano mainnet or testnet.

3- You want to run the cardano node, chain-index and (optionally) your PAB webserver on the Cardano mainnet or testnet.

## Contribution

If you see a potential enhancement (or problem), please submit an issue on [https://github.com/input-output-hk/plutus-apps/issues](https://github.com/input-output-hk/plutus-apps/issues).

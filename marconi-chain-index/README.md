# marconi-chain-index

`marconi-chain-index` is a chain indexer with a predefined set of indexers from which the user can choose from.
Currently, this executable only acts as a chain-indexer, and doesn't provide a server from which clients can issue requests to.

## Prerequisites

* [GHC](https://www.haskell.org/downloads/) (`==8.10.7`)
* [Nix](https://nixos.org/download.html) (`>=2.5.1`)
  * Enable [IOHK's binary cache](https://iohk.zendesk.com/hc/en-us/articles/900000673963-Installing-Nix-on-Linux-distribution-and-setting-up-IOHK-binaries) or else you will build the world!
* [cardano-node](https://github.com/input-output-hk/cardano-node) ([==1.35.4](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4))

## How to build from source

### Cabal build

TODO

### Nix build

The `marconi-chain-index` executable is available as a nix flake.

If inside the `plutus-apps` repository, you can run from the top-level:

```
$ nix build .#marconi-chain-index
```

Or you may run from anywhere:

```
$ nix build github:input-output-hk/plutus-apps#marconi-chain-index
```

Both commands will produce a `result` directory containing the executable
`result/bin/marconi-chain-index`.

### Cabal+Nix build

To build `marconi-chain-index` from the source files, use the following commands:

```sh
git clone git@github.com:input-output-hk/plutus-apps.git
nix develop
cabal clean && cabal update # Optional, but makes sure you start clean
cabal build marconi-chain-index
```

The above process will build the executable in your local environment at this location:

```sh
cabal exec -- which marconi-chain-index
```

Or you can run the executable directly with:

```sh
cabal run marconi-chain-index:exe:marconi-chain-index -- --help
```

## Command line summary

Run `marconi-chain-index`, `$(cabal exec -- which marconi-chain-index) --help` or `cabal run marconi-chain-index:exe:marconi-chain-index -- --help` for a general synopsis of the command line options depending on your installation method.

See [this automatically generated golden file](./test/Spec/Golden/Cli/marconi-chain-index___help.help) for the up-to-date help command output.

## How to run

We are assuming that:

* you have a local running cardano-node instance.
* you've set the following environment variables:
  * `CARDANO_NODE_SOCKET_PATH`: socket path of your local cardano-node instance
  * `MARCONI_DB_DIRECTORY`: directory in which to create the various SQLite database files

The most minimal way to run the executable is as follows:

```sh
$(cabal exec -- which marconi-chain-index) \
    --testnet-magic 1 \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --db-dir "$MARCONI_DB_DIRECTORY" \
```

From the last chainpoint (if none, from genesis), this command will fetch blocks from the local node, extract required data and index them in the database.

Using the `--addresses-to-index`, you can instruct Marconi to index target addresses.
By default, all addresses are indexed in the database.

Some example addresses from pre-production-testnet are:

```
addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc \
addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah \
addr_test1wpzvcmq8yuqnnzerzv0u862hmc4tc8xlm74wtsqmh56tgpc3pvx0f \
addr_test1wrn2wfykuhswv4km08w0zcl5apmnqha0j24fa287vueknasq6t4hc \
addr_test1wr9gquc23wc7h8k4chyaad268mjft7t0c08wqertwms70sc0fvx8w \
```

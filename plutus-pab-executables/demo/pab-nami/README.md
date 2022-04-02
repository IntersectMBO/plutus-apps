# PAB-Nami simple demo

This is a very simple demo application featuring the use of a browser/light wallet (Nami) alongside the PAB (Plutus Application Backend).

## Context

In this demo, we want to showcase a very minimal example of how to integrate the PAB with a light wallet. It uses the `PayToWallet` contract in the PAB which has *no* Plutus on-chain validation code. Therefore, we simply use the PAB to construct a partial/unbalanced transaction and make it available to a frontend application. It doesn't use the local Cardano node, nor the chain index. Here's an outline of the general interactions:

1. The frontend application should have access to a light/browser wallet (in this case, Nami)
2. The frontend application activates the `PayToWallet` contract throught the PAB.
3. The frontend application calls the `PayToWallet` contract PAB endpoint with the recipient's payment key hash and stake key hash, and the amount to send in Lovelace.
4. The PAB constructs a partial/unbalanced transaction and makes it available through it's contract instance's status endpoint.
5. The frontend application fetches this partial/unbalanced transaction. It then balances it and signs it using the Nami wallet api.
6. Finally, the frontend application submits the final transaction using the Nami wallet api (which in turn, uses the blockfrost API).

## Run the demo

The instructions were tested inside the `plutus-apps`'s `nix-shell`.

The demo contains two parts: the PAB application in `plutus-pab-executables/demo/pab-nami/pab` and the frontend application in `plutus-pab-executables/demo/pab-nami/client` which interacts with the PAB and the Nami wallet.

### Setup Nami wallet

1. Install the Nami wallet browser extension (currently, the Nami wallet is only available in Chrome-based browsers)
2. Setup your wallet password and seed phrase as required.
3. Create a second account in your wallet in order to test the demo.
4. In Nami wallet's `Settings > Network`, select the Cardano testnet.
5. Add some funds to your main account using the [Cardano faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) (when you have finished using your test tokens, please return them to the faucet following the instructions in their website).
6.

### Run the PAB

Although this demo doesn't use a Cardano node directly, the PAB requires a connection to a Cardano Node socket. Follow the instructions [here](https://developers.cardano.org/docs/get-started/installing-cardano-node) to install and run a Cardano Testnet Node. *The node need not be synced for the demo to work.*

Ensure that your `pscSocketPath` value in `plutus-pab-executables/demo/pab-nami/pab/plutus-pab.yaml` is pointing to your Cardano Node socket.

To run PAB, you have two options:

1- Use `cabal`:

From a command line interface, make sure you're in the root folder of the `plutus-apps` repository, and run the following commands:

```
# 'Migrate' command to initialise the PAB database.
$ cabal run plutus-pab-executables:exe:plutus-pab-nami-demo -- migrate --config plutus-pab-executables/demo/pab-nami/pab/plutus-pab.yaml

# Run the PAB webserver
$ cabal run plutus-pab-executables:exe:plutus-pab-nami-demo -- webserver --config plutus-pab-executables/demo/pab-nami/pab/plutus-pab.yaml
```

2- Or use the script provided by `nix-shell`:

Go to the frontend application's directory:

```
$ cd plutus-pab-executables/demo/pab-nami/client
```
and run the script

```
$ pab-nami-demo-server
```

### Run the demo frontend

From another instance of `nix-shell`, run the following command to launch the frontend application:

```
# Go the frontend application's directory (plutus-pab-executables/demo/pab-nami/client)

# This is a temporary measure for using function in Nami's non-public api.
# This will be removed in the next iterations.
$ git clone --depth 1 --branch 2.3.6 https://github.com/Berry-Pool/nami-wallet.git lib/nami-wallet

# Install NPM and Spago dependencies and run the dev server
$ npm start
```

Open the browser with the frontend application's URL (`http://localhost:8009`) and you should see two fields: the recipient's Bech32 Cardano address and an amount to send to it in Lovelace.

[Possible errors]
====

1. If your PAB gives an error like `plutus-pab-nami-demo: Network.Socket.connect: <socket: 15>: does not exist (No such file or directory)`, this means it can't connect to your Cardano Node. Ensure that your node is running and the `pscSocketPath` value in `plutus-pab-executables/demo/pab-nami/pab/plutus-pab.yaml` is pointing to your Cardano Node socket!

1. If you see `ERR_SSL_PROTOCOL_ERROR` or `SSL_ERROR_RX_RECORD_TOO_LONG`, then you probably tried to access the website through HTTPS. Use HTTP.

1. If you see a blank page, check you're browser's console. If you see `window.cardano is undefined`, then that means that the Nami wallet browser extension was not correctly installed.

====

From the Nami wallet interface, change to your *second* account, click on the `Receive` tab, copy the shown Cardano address and paste it in the application's form. Then, go back to your *first* account which contains your funds. Choose a lovelace amount (minimum of 2_000_000 Lovelace) and click on `Make payment`. The application should show the transaction id that was submitted to the Cardano testnet. After waiting for a bit, you should see the funds change in Nami wallet's interface.


## Development

### Frontend

The frontend is written in Purescript. When inserting new dependencies in `spago.dhall`, run `spago install` and `spago2nix generate`. Don't forget to commit the file changes.

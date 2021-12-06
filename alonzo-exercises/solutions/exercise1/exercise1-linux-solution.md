## Exercise 1 solution

### Using Linux binaries

Create a working directory and get the cardano-node and cardano-cli binaries

    mkdir cardano
    wget https://hydra.iohk.io/build/7191568/download/1/cardano-node-1.28.0-linux.tar.gz
    tar -xf cardano-node-1.28.0-linux.tar.gz
    rm cardano-node-1.28.0-linux.tar.gz

Copy the binaries to a directory that is in the PATH

    cp cardano* /usr/local/bin
    rm cardano*

Verify your version

    cardano-node --version
    > cardano-cli 1.28.0 - darwin-x86_64 - ghc-8.10
    > git rev 708de685d49ec6af4b2d8b3cbfa0eca0e9e43edf

Get the configuration files for the Alonzo testnet

    wget https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-config.json
    wget https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-byron-genesis.json
    wget https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-shelley-genesis.json
    wget https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-alonzo-genesis.json
    wget https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-topology.json

Start a pasive node

    cardano-node run \
    --topology alonzo-purple-topology.json \
    --database-path db \
    --socket-path db/node.socket \
    --host-addr 0.0.0.0 \
    --port 3001 \
    --config alonzo-purple-config.json \

Add `CARDANO_NODE_SOCKET_PATH` to the `.bashrc` file

    echo export CARDANO_NODE_SOCKET_PATH=~/cardano/db/node.socket >> .bashrc
    source .bashrc

Query the tip of the blockchain. Verify that the node is syncing.

	cardano-cli query tip --testnet-magic 8

	{
	    "epoch": 89,
	    "hash": "53279f7b8ac99a53cfb7ccac592be1ce49b28d87c145f6edd6426b398bc9b502",
	    "slot": 639812,
	    "block": 31401,
	    "era": "Alonzo"
	}

Again

	cardano-cli query tip --testnet-magic 8
	{
	    "epoch": 89,
	    "hash": "40b6f07b0e0d93ed150a9605c81ab81a939304ceb8b04350a564f5c836b26183",
	    "slot": 641150,
	    "block": 31475,
	    "era": "Alonzo"
	}

Verify you are in sync

https://explorer.alonzo-purple.dev.cardano.org/en

Use cardano-cli to generate payment keys and address.

	cardano-cli address key-gen \
	--verification-key-file payment.vkey \
	--signing-key-file payment.skey

	cardano-cli stake-address key-gen \
	--verification-key-file stake.vkey \
	--signing-key-file stake.skey

	cardano-cli address build \
	--payment-verification-key-file payment.vkey \
	--stake-verification-key-file stake.vkey \
	--out-file payment.addr \
	--testnet-magic 8

	cat payment.addr
	addr_test1qzsdrv4juj5hmc3dqeq9wc7kpjwmu69zwjz9j29m3l88m26hdfyr7r54f67e7c9de70ld7gzfwmhqpudvkyyvkse2gmq0cvrdj

	cardano-cli stake-address build \
	--stake-verification-key-file stake.vkey \
	--out-file stake.addr \
	--testnet-magic 8

	cat stake.addr
	stake_test1uptk5jplp625a0vlvzkul8lklypyhdmsq7xktzzxtgv4ydsk4g6es

	export ADDRESS=addr_test1qzsdrv4juj5hmc3dqeq9wc7kpjwmu69zwjz9j29m3l88m26hdfyr7r54f67e7c9de70ld7gzfwmhqpudvkyyvkse2gmq0cvrdj


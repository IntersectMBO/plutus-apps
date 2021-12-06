#  Exercise 1
## Alonzo White testnet

Building a Cardano passive-node and the **cardano-cli** using Nix

#### 1. Fork [cardano-node](https://github.com/input-output-hk/cardano-node)
Clone the IOHK repo for the cardano-node

    git clone git@github.com:input-output-hk/cardano-node.git

#### 2. Checkout to the right tag

For the Alonzo White testnet we will be working with a particular tag, checkout to `tag\alonzo-white-1.0` and create a new branch `alonzo-white_1_0` (note that using underscore for the _1_0 in the branch will prevent ambiguitiy)

    git checkout tag/alonzo-white-1.0 -b alonzo-white_1_0

**Please check and use the correct tag -- the node version will be updated during the course of the testnet**

#### 3. Build node and cli

We now can use Nix to build the node and the cli

    nix-build -A scripts.alonzo-white.node -o result/alonzo-white/cardano-node-alonzo-white
    nix-build -A cardano-cli -o result/alonzo-white/cardano-cli

#### 4. Add the cli SOCKET

To make quicker access to the cli, we have to add its socket to our `bashrc`. Replace `yourPath` with the path where you have cloned the repo in step 1

    echo export CARDANO_NODE_SOCKET_PATH=~/yourPath/cardano-node/result/alonzo-white/state-node-alonzo-white/node.socket >> ~/.bashrc
    source ~/.bashrc

Additionally you can copy `cardano-cli`(see **NOTE**) and `cardano-node-alonzo-white` bins to your `/usr/local/bin/` path for quick access to these commands as follows

```
$ cd /usr/local/bin/
$ sudo cp ~/yourPath/cardano-node/result/alonzo-white/cardano-cli/bin/cardano-cli ./
$ sudo cp ~/yourPath/cardano-node/result/alonzo-white/cardano-node-alonzo-white/bin/cardano-node-alonzo-white ./
```

now verify that the commands are accessible, expected output:

```
$ cardano-cli --version
cardano-cli 1.27.0 - linux-x86_64 - ghc-8.10
git rev 7cf540dafa0ca496526e0614fa3ef6262e85c70d

$ cardano-node-alonzo-white --help
Starting: /nix/store/c7cnd5nbf4332wxrllqvrkpdfk8w68x2-cardano-node-exe-cardano-node-1.27.0/bin/cardano-node run
--config /nix/store/5mm9vmf4xrbhwl9rc14pi8kr51g5daan-config-0-0.json
--database-path state-node-alonzo-white/db-alonzo-white
--topology /nix/store/dr0203ii399zqplq547f6gqldk9s5dhv-topology.yaml
--host-addr 0.0.0.0
--port 3001
--socket-path state-node-alonzo-white/node.socket
.
.
.
```

**NOTE.-** Whenever there is an update to the testnet that implies updating the nodes, you should as well remove and update your socket and cli, repeating Step 4.  

**NOTE.-** The version information that is output will depend on the node tag.

#### 5. Run a node

We will start a passive-node, this is a relay that can communicate with the testnet but will not participate in the creation of blocks. 

If you are joining before the Hard Fork (HF), before afternoon UTC time of July 14, 2021 you can log all the info of the node to capture the transition of the HF (see exercise 2), you will see the transition from Mary era to Alonzo White era.

```
$ cd result/alonzo-white
$ cardano-node-alonzo-white
```


#### 6. Query last block (new Terminal)

To verify that we are in sync with the testnet we will use the cli and query the tip of the blockchain. Leave the node running and in a new Terminalenter

    $ cardano-cli query tip --testnet-magic 7

we should see something like this

```
{
    "epoch": 194,
    .
    .
    .
    "era": "Alonzo"
}
```

wait a moment and query the newest tip again

```
{
    "epoch": 264,
    .
    .
    .
    "era": "Alonzo"
}
```

Great, the node is up and running!

#### 7. Generate keys

Lastly we will generate payment & stake keys as well as payment & stake addresses. 

To generate a payment address follow

```
## generate payment verification and signing keys
cardano-cli address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey

## generate stake verification and signing keys
cardano-cli stake-address key-gen \
--verification-key-file stake.vkey \
--signing-key-file stake.skey

## generate payment address (from payment verification and stake verification keys)
cardano-cli address build \
--payment-verification-key-file payment.vkey \
--stake-verification-key-file stake.vkey \
--out-file payment.addr \
--testnet-magic 7
```

Print the payment address on Terminal, and then export it

```
cat payment.addr
addr_test1qzcejjek8d7ah5pnk5lapvq9gjk8hr5t0kyepe0yltusruw04ecqwsgesl2sl65kjyxqkxj24mdgwdpd8e2n6np04u5smq6c3r
export ADDRESS=addr_test1qzcejjek8d7ah5pnk5lapvq9gjk8hr5t0kyepe0yltusruw04ecqwsgesl2sl65kjyxqkxj24mdgwdpd8e2n6np04u5smq6c3r
```

Now generate a staking address

```
cardano-cli stake-address build \
--stake-verification-key-file stake.vkey \
--out-file stake.addr \
--testnet-magic 7
```

verify the staking address

```
cat stake.addr
stake_test1ur86uuq8gyvc04g0a2tfzrqtrf92ak58xsknu4fafsh672g3kvfmg
```

Sweet!

Now claim your test Ada from the "faucet"!

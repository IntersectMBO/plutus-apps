# Sample solution exercise 3

## Part 1 Create a new set of keys and address

Set up the node socket path

    export CARDANO_NODE_SOCKET_PATH=$HOME/alonzo-purple/socket
    export MAGIC="--testnet-magic 8"

Generate necessary keys

    cardano-cli address key-gen \
    --verification-key-file payment2.vkey \
    --signing-key-file payment2.skey

    cardano-cli stake-address key-gen \
    --verification-key-file stake2.vkey \
    --signing-key-file stake2.skey

    cardano-cli address build \
    --payment-verification-key-file payment2.vkey \
    --stake-verification-key-file stake2.vkey \
    --out-file payment2.addr \
    ${MAGIC}


And check the UTxO for the payment address
    
    cardano-cli query utxo ${MAGIC} --address $(cat payment.addr)

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    55ba9e9542b8962a6689f69242158d665bde7e610e4e5af9ebe9e080ad2024a2     1        10000000 lovelace + TxOutDatumHashNone
    7b4956b103d47908318ee92aa0790ff4b36fe7940991f0be350c9085fc4da175     1        100000000000 lovelace + TxOutDatumHashNone



Build the transaction using `transaction build` (recommended)
    
    ADDR=7b4956b103d47908318ee92aa0790ff4b36fe7940991f0be350c9085fc4da175#1

    cardano-cli transaction build \
    --alonzo-era \
    --tx-in ${ADDR} \
    --tx-out $(cat payment2.addr)+25000000000 \
    --change-address $(cat payment.addr) \
    ${MAGIC} \
    --out-file tx.raw

or using `transaction build-raw`

    cardano-cli transaction build-raw \
    --alonzo-era \
    --fee 200000 \
    --tx-in ${ADDR} \
    --tx-out $(cat payment2.addr)+25000000000 \
    --tx-out $(cat payment.addr)+74999800000 \
    --out-file tx.raw

Sign the transaction

    cardano-cli transaction sign \
    ${MAGIC} \
    --signing-key-file payment.skey \
    --tx-body-file tx.raw \
    --out-file tx.sign

And submit it to the Testnet

    cardano-cli transaction submit ${MAGIC}  --tx-file tx.sign


Check that the result is what you expect

    cardano-cli query utxo ${MAGIC}  --address $(cat payment2.addr)

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7d721d8a0cc2f3d87f44d4df22d6815f58fb67f421b283462ff3b823c36f34a6     1        25000000000 lovelace + TxOutDatumHashNone

## PART 2  Lock funds in a script

All transaction outputs that are locked by non-native scripts must include
the hash of an additional “datum”. **A non-native script-locked output that does not include a datum hash is unspendable**

Later, the actual datum needs to be supplied by the transaction spending that output, and can be used to encode state, for example.

### Generate a random number and save it to a file to use it as datum.

Use https://www.random.org/integers/ to generate a random number and save it in a file --> `random_datum.txt`

### Use the CLI to hash your random number and save the hash to `random_datum_hash.txt`

    cardano-cli transaction hash-script-data --script-data-value $(cat random_datum.txt) > random_datum_hash.txt

    cat random_datum_hash.txt
    269499980312e03ec5c82239ca527534a15c0da27f026f15dc706e7d0cd6770d

### Generate the script address

    cardano-cli address build --payment-script-file AlwaysSucceeds.plutus ${MAGIC} --out-file script.addr

### Generate protocol-parameters file

    cardano-cli query protocol-parameters ${MAGIC} > pparams.json

### Send funds to the script address, we must include the datum hash


Build the transaction using `transaction build` (recommended)

      ADDR2=7d721d8a0cc2f3d87f44d4df22d6815f58fb67f421b283462ff3b823c36f34a6#1
      
      cardano-cli transaction build \
      --alonzo-era \
      --tx-in ${ADDR2} \
      --tx-out $(cat script.addr)+10000000000 \
      --tx-out-datum-hash $(cat random_datum_hash.txt) \
      --change-address $(cat payment2.addr) \
      ${MAGIC} \
      --protocol-params-file pparams.json \
      --out-file tx.raw
    
or using `transaction build-raw`

    cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in ${ADDR2} \
    --tx-out $(cat script.addr)+10000000000 \
    --tx-out-datum-hash $(cat random_datum_hash.txt) \
    --tx-out  $(cat payment2.addr)+14999000000 \
    --fee 1000000 \
    --protocol-params-file pparams.json \
    --out-file tx.raw

When building the transaction it is important to follow the proper order for inputs and outputs, **Find details running `cardano-cli transaction build --help`.**

In this case we want the script address to get **funds and a datum**, therefore we first use  `--tx-out $(cat script.addr)+10000000000` followed by `--tx-out-datum-hash $(cat random_datum_hash.txt)`

So now we can sign and submit the transaction

    cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file payment2.skey ${MAGIC} --out-file tx.sign

    cardano-cli transaction submit ${MAGIC} --tx-file tx.sign

### Your node will show the txid and the size of your transaction:

    [VM:cardano.node.Mempool:Info:171] [2021-06-16 17:10:27.00 UTC] fromList [("tx",Object (fromList [("txid",String "txid: TxId {_unTxId = SafeHash \"fb169b24bbf66e9a1f779ab87fef63321a616d64e69e5e0206a6b8e8af4c7802\"}")])),("mempoolSize",Object (fromList [("bytes",Number 305.0),("numTxs",Number 1.0)])),("kind",String "TraceMempoolAddedTx")]

### Query the script address balance

We can use the tx hash and/or the datum hash to identify "our" UTxO at the script. The last one, in this case:

    cardano-cli query utxo ${MAGIC} --address $(cat script.addr)
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2c80e8db5205ca6bc60688b951406c929c098daa2665d5767567c39ff58e80f9     0        10140000 lovelace + TxOutDatumHashNone
    abfed05c8c7f68c415266a42b413ee3c29a295c29d1e3e472138af082155a0f2     0        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "86fc1f5caf574b1ac9e6caeb09174455880e0bf6aaef0763108f37dff5878c7d"
    fb169b24bbf66e9a1f779ab87fef63321a616d64e69e5e0206a6b8e8af4c7802     0        10000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "269499980312e03ec5c82239ca527534a15c0da27f026f15dc706e7d0cd6770d"

So the script address has a UTxO that can ONLY be spent when providing the VALUE that hashes to `269499980312e03ec5c82239ca527534a15c0da27f026f15dc706e7d0cd6770d`

## PART 3 CAN WE SPEND FROM THE SCRIPT?



* The user specifies the UTxO entries containing funds sufficient to cover a percentage (usually 100 or more) of the total transaction fee. These inputs are only collected in the case of script validation failure, and are called collateral inputs `--tx-in-collateral`. In the case of script validation success, the fee specified in the fee field of the transaction is collected, but the collateral is not.

     COLLATERAL=3064e641ab188863eb3ec7755d1c06ba4c95b3db5575a87998c95c7c8053430a#1

Let's build the transaction.  Again, it is important to observe the proper order for inputs and outputs, check  `cardano-cli transaction build --help`

     ADDR3=fb169b24bbf66e9a1f779ab87fef63321a616d64e69e5e0206a6b8e8af4c7802#0

     cardano-cli transaction build \
     --alonzo-era \
     --tx-in ${ADDR3} \
     --tx-in-script-file AlwaysSucceeds.plutus \
     --tx-in-datum-value $(cat random_datum.txt) \
     --tx-in-redeemer-value $(cat random_datum.txt) \
     --tx-in-collateral ${COLLATERAL} \
     --change-address $(cat payment2.addr) \
     --protocol-params-file pparams.json \
     ${MAGIC} \
     --out-file tx.raw

Or if using `transaction build-raw`, it is necessary to do some more work.

* The exact budget to run a script is expressed in terms of computational resources, and
included in the transaction data as `--tx-in-execution-units`

* The exact total fee a transaction is paying is also specified in the transaction data. For a transaction to be valid, this fee must cover the script-running resource budget at the current price, as well as the size-based part of the required fee. If the fee is not sufficient to
cover the resource budget specified (eg. if the resource price increased), the transaction is considered invalid and will not appear on the ledger (will not be included in a valid block). No fees will be collected in this case. This is in contrast with the gas model, where, if prices go
up, a greater fee will be charged - up to the maximum available funds, even if they are not sufficient to cover the cost of the execution of the contract.


```
    cardano-cli transaction build-raw \
    --alonzo-era \
    --fee 801000000 \
    --tx-in ${ADDR3} \
    --tx-in-script-file AlwaysSucceeds.plutus \
    --tx-in-datum-value $(cat random_datum.txt) \
    --tx-in-redeemer-value 0 \
    --tx-in-execution-units "(200000000,200000000)" \
    --tx-in-collateral ${COLLATERAL} \
    --tx-out $(cat payment2.addr)+9199000000 \
    --protocol-params-file pparams.json\
    --out-file tx.raw
```

Sign the transaction

    cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file collateral_payment.skey ${MAGIC} --out-file tx.sign
    cardano-cli transaction submit ${MAGIC} --tx-file tx.sign

Transaction successfully submitted.

    [VM:cardano.node.Mempool:Info:40] [2021-06-16 19:43:30.87 UTC] fromList [("txs",Array [Object (fromList [("txid",String "txid: TxId {_unTxId = SafeHash \"989096df8785235740efbc7a1de0df17bd2123a64536172a11f5c464d822818a\"}")])]),("mempoolSize",Object (fromList [("bytes",Number 0.0),("numTxs",Number 0.0)])),("kind",String "TraceMempoolRemoveTxs")]

Check the receiving address balance:

    cardano-cli query utxo ${MAGIC} --address $(cat payment2.addr)

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    3064e641ab188863eb3ec7755d1c06ba4c95b3db5575a87998c95c7c8053430a     0        74198600000 lovelace + TxOutDatumHashNone
    55ba9e9542b8962a6689f69242158d665bde7e610e4e5af9ebe9e080ad2024a2     1        10000000 lovelace + TxOutDatumHashNone
    989096df8785235740efbc7a1de0df17bd2123a64536172a11f5c464d822818a     0        9199000000 lovelace + TxOutDatumHashNone

And we can confirm that we have spent the UTxO at the script:

    cardano-cli query utxo ${MAGIC} --address $(cat script.addr)
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2c80e8db5205ca6bc60688b951406c929c098daa2665d5767567c39ff58e80f9     0        10140000 lovelace + TxOutDatumHashNone
    9fef4121976a73551a3945107af1b4b70c554f9b2004f42bf00d45444ebdf90a     1        100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ae5703d02f1771ba1e1a5543e5b17a8ae257b7b2b5d446b4b3694084ecd7b80c"
    abfed05c8c7f68c415266a42b413ee3c29a295c29d1e3e472138af082155a0f2     0        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "86fc1f5caf574b1ac9e6caeb09174455880e0bf6aaef0763108f37dff5878c7d"

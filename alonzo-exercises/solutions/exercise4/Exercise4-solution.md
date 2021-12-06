## EXERCISE 4 (Items 1, 2 and 3)

__Clone the Alonzo-testnet repository__

    git clone https://github.com/input-output-hk/Alonzo-testnet.git

__Compile helloworld.hs__

    cd Alonzo-testnet/resources/plutus-sources/plutus-helloworld
    cabal build -w ghc-8.10.4
    cat plutus-helloworld.cabal | grep executable
    > executable plutus-helloworld
    > executable plutus-helloworld-bytestring
    cabal run plutus-helloworld -- 1 helloworld2.plutus


    Writing output to: helloworld2.plutus
    "Log output"
    []
    "Ex Budget"
    ExBudget {_exBudgetCPU = ExCPU 9814000, _exBudgetMemory = ExMemory 2260}

__Build the script address__

    cardano-cli address build --payment-script-file helloworld2.plutus --testnet-magic 8 --out-file helloworld2.addr

    cardano-cli query utxo --testnet-magic 8 --address $(cat helloworld2.addr)
                           TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    978681fa039c391bf37b1f2ba57312a10e2b823cf68c01fb627973b36064e452     1        512000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4250ea713ad7ba3b121621a8d14d8e39a4300065314b7ce9a40526acf992c8e3"
    dd58557b3e780703126a60e340fb13a681b47793771e5980dbb0ef479c905db6     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ff92136db7aec02e06f0d93ed5cbea5d33360061e5d3cab51a827d65fdeb33ad"

__Lock some funds in the script__

    cardano-cli query utxo --testnet-magic 8 --address $(cat ~/cardano/whiteWallet1/payment.addr)

                           TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    674670674b874c27a26fde0edf65c4075d729f2cc72cc0a041e788b227c63852     1        499000000 lovelace + TxOutDatumHashNone
    c07a6b91ae3d17421c1d47854b52e702dfeb9289c164a8f227f41fc974441e31     0        6123800000 lovelace + TxOutDatumHashNone
    c07a6b91ae3d17421c1d47854b52e702dfeb9289c164a8f227f41fc974441e31     5        500000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     0        1500000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     3        10000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     5        10000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     6        20000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     7        20000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     8        30000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     9        40000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c    10        50000000 lovelace + TxOutDatumHashNone

__Hash the datum value__

`79600447942433` represents `hello world` message converted to an Integer and shortened to fit within the 8-byte limit for an `int` datum.

    cardano-cli transaction hash-script-data --script-data-value 79600447942433 > helloworld_hash.txt

__Build the transaction to lock 98 ADA__

    cardano-cli transaction build \
    --alonzo-era \
    --tx-in 674670674b874c27a26fde0edf65c4075d729f2cc72cc0a041e788b227c63852#1 \
    --tx-out $(cat helloworld2.addr)+98000000 \
    --tx-out-datum-hash $(cat helloworld_hash.txt) \
    --change-address $(cat ~/cardano/whiteWallet1/payment.addr) \
    --testnet-magic 8 \
    --out-file tx.raw

__Sign and submit__

    cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file ~/cardano/whiteWallet1/payment.skey --out-file tx.sign

    cardano-cli transaction submit --testnet-magic 8 --tx-file tx.sign

__Check the balances__

    cardano-cli query utxo --testnet-magic 8 --address $(cat ~/cardano/whiteWallet1/payment.addr)

                           TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    9f411a81c28fb3b9927cad1d3da8831711e33280c39142c1c6d480e88084a514     1        400000000 lovelace + TxOutDatumHashNone
    c07a6b91ae3d17421c1d47854b52e702dfeb9289c164a8f227f41fc974441e31     0        6123800000 lovelace + TxOutDatumHashNone
    c07a6b91ae3d17421c1d47854b52e702dfeb9289c164a8f227f41fc974441e31     5        500000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     0        1500000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     3        10000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     5        10000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     6        20000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     7        20000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     8        30000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     9        40000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c    10        50000000 lovelace + TxOutDatumHashNone

__Note the script has 98 ADA locked__

    cardano-cli query utxo --testnet-magic 8 --address $(cat helloworld2.addr)


                           TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    978681fa039c391bf37b1f2ba57312a10e2b823cf68c01fb627973b36064e452     1        512000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4250ea713ad7ba3b121621a8d14d8e39a4300065314b7ce9a40526acf992c8e3"
    9f411a81c28fb3b9927cad1d3da8831711e33280c39142c1c6d480e88084a514     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    dd58557b3e780703126a60e340fb13a681b47793771e5980dbb0ef479c905db6     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ff92136db7aec02e06f0d93ed5cbea5d33360061e5d3cab51a827d65fdeb33ad"

__Spend from the script__

    cardano-cli transaction build \
    --alonzo-era \
    --protocol-params-file ~/cardano/pparams.json \
    --tx-in 9f411a81c28fb3b9927cad1d3da8831711e33280c39142c1c6d480e88084a514#0 \
    --tx-in-script-file helloworld2.plutus \
    --tx-in-datum-value 79600447942433 \
    --tx-in-redeemer-value 79600447942433 \
    --tx-in-collateral cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c#3 \
    --change-address $(cat payment.addr) \
    --testnet-magic 8 \
    --out-file tx.raw

    cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file ~/cardano/whiteWallet1/payment.skey --out-file tx.sign

    cardano-cli transaction submit --testnet-magic 8 --tx-file tx.sign


__Confirm we have redeemed succesfully__


    cardano-cli query utxo --testnet-magic 8 --address $(cat ~/cardano/whiteWallet1/payment.addr)
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bc95f0701a75b5991e81eaf23399bc904b94f62e375d924e3075dadbb71ac371     0        98000000 lovelace + TxOutDatumHashNone
    bc95f0701a75b5991e81eaf23399bc904b94f62e375d924e3075dadbb71ac371     1        100000000 lovelace + TxOutDatumHashNone
    c07a6b91ae3d17421c1d47854b52e702dfeb9289c164a8f227f41fc974441e31     0        6123800000 lovelace + TxOutDatumHashNone
    c07a6b91ae3d17421c1d47854b52e702dfeb9289c164a8f227f41fc974441e31     5        500000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     0        1500000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     3        10000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     5        10000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     6        20000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     7        20000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     8        30000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c     9        40000000 lovelace + TxOutDatumHashNone
    cdecdad235b235a0dd616290ecddb9df01f9226dda961f2012d770caa45f504c    10        50000000 lovelace + TxOutDatumHashNone


__The script address shows that we have succesfully spent funds from the script__

    cardano-cli query utxo --testnet-magic 8 --address $(cat helloworld2.addr)
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    978681fa039c391bf37b1f2ba57312a10e2b823cf68c01fb627973b36064e452     1        512000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4250ea713ad7ba3b121621a8d14d8e39a4300065314b7ce9a40526acf992c8e3"
    dd58557b3e780703126a60e340fb13a681b47793771e5980dbb0ef479c905db6     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ff92136db7aec02e06f0d93ed5cbea5d33360061e5d3cab51a827d65fdeb33ad"

__The node logged the transaction__

    [CLR:cardano.node.Mempool:Info:41] [2021-07-16 06:58:47.93 UTC] fromList [("txs",Array [Object (fromList [("txid",String "txid: TxId {_unTxId = SafeHash \"bc95f0701a75b5991e81eaf23399bc904b94f62e375d924e3075dadbb71ac371\"}")])]),("mempoolSize",Object (fromList [("bytes",Number 0.0),("numTxs",Number 0.0)])),("kind",String "TraceMempoolRemoveTxs")]

db-utils
===
A [SQLite](https://www.sqlite.org/index.html) small utility haskell project for [marconi](../marconi)

## What does it do
In summary, db-utils, is an internal tool used to acquire valid [Bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#Bech32) [Shelley addresses](https://cips.cardano.org/cips/cip19/#shelleyaddresses) with [Utxos](https://iohk.io/en/research/library/papers/the-extended-utxo-model/).

We store the valid addresses alongside utxos table in marconi SQLite instance in shelleyaddresses table.

``` sh
$ DB sqlite3 ./.marconidb/utxodb ".tables" ".exit"
frequtxos         shelleyaddresses  spent             utxos
```

``` sh
$ DB sqlite3 ./.marconidb/utxodb ".schema shelleyaddresses" ".exit"
CREATE TABLE shelleyaddresses (address text not null, frequency int not null);
```

``` sh
$ sqlite3 ./.marconidb/utxodb "select * from shelleyaddresses limit 5;" ".exit"
addr1q837de0y7j3ncegph2a8mc0e86q9evwtekd3ejhlpr97wclrumj7fa9r83jsrw460hslj05qtjcuhnvmrn907zxtua3skv7yyl|2535
addr1qxyxudgzljnnaqghm8hlnpp36uvfr68a8k6uemumgjdcua4y7d04xcx9hnk05lnl6m9ptd9h3pj9vvg2xe4j354uh8vsarpydn|2321
addr1qytr6ma495fkqpfnd7gk5kwmtfdh084xvzn7rv83ha87qq6yfm3y8yv39lcrqc6ej3zdzvef4aj3dv3pq2snakkcwscsfyrn3g|2317
addr1qxt2ggq005kfm3uwe89emy3ka2zgdtrpxfarvz6033l3fqvk5ssq7lfvnhrcajwtnkfrd65ys6kxzvn6xc95lrrlzjqsjttk32|1353
addr1qyhat6v7w65799pkc8ff3mjcwk79kqs8gv8t4expd67f9seqksv3earfx6skxkdhe4hcekjkj0x333dd76u8re8cmg2qwrdzn2|1155

```

### Why do we need this
The need for this utility is from the desire to test marconi-mamba in a live network with valid addresses that may potentially correspond to a large list of Utxos.  Here are the high level requirements:

+ acquire valid Bech32 Shelley addresses from a given [Cardano network](https://docs.cardano.org/explore-cardano/cardano-network/about-the-cardano-network).
+ rank the addresses based on the number of links they have to Utxos
+ store the addresses for future used
+ addresses need to be store in Text format
+ the utility may not cause any down time to the cardano node

## Installation and execution

+ build the project
+ execute the project

### Assumption
+ cardano node is running
+ marconi is running

### Build

``` sh
git clone git@github.com:input-output-hk/plutus-apps.git
cd plutus-apps
cabal build marconi
cabal build marconi-mamba
cabal build db-utils
```
### Execution

``` sh
cabal run db-utils-exe -- --help
```
Note:
Examine the default CLI parameters from above,  adjust accordingly and run the program.


``` sh
cabal run db-utils-exe --  --testnet-magic 2
```


## Caution
This utility is under heavy development and is for internal use at this time!

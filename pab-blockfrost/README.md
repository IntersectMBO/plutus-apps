# Blockfrost endpoint integration for the PAB

How to integrate this version of plutus-apps into your contracts:

## Usage

Add `pab-blockfrost` to plutus-apps' subdir list of your `cabal.project` file.

## PAB Config 
 
In the PAB [configuration file](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/test-node/testnet/pab-config.yml) of your smart-contract you have the option to choose querying blockfrost or chain-index.

To choose chain index, you don´t need to do anything different. For instance, the following is a valid configuration:

```yaml
chainIndexConfig:
   ciBaseUrl: http://localhost:9083
   ciWatchedAddresses: []
```

To choose blockfrost, you have to replace the chain-index configuration for the following configuration:

```yaml
blockfrostConfig:
  bfTokenPath: ./testnet-token
```
where bfTokenPath is the path to the file that holds you blockfrost token.

Keep in mind you can't have both configurations at the same time.
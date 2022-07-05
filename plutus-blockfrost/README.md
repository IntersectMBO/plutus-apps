Blockfrost endpoint integration for the PAB

How to integrate this version of plutus-apps into your contracts:

1. Change the location for the plutus-apps repository on the .project file to: https://github.com/joinplank/plutus-apps.git
2. Add `plutus-blockfrost` to the list of subdirs
3. Use the latest tag: `c188ec0be7e14ffe9b8cbe6c6d11e7227094909a`
4. Add a `testnet-token` file on the base directory where you will run the PAB containing the blockfrost API key for the testnet. You can get a key for free [here](https://blockfrost.io/auth/signin).
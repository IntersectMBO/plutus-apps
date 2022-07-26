# Building

## Server

### stack

```sh
stack build plutus-playground-server
stack exec -- plutus-playground-server psgenerator ./plutus-playground-client/generated
stack exec -- plutus-playground-server webserver
```

### nix

From the `nix-shell` simply run:
```sh
plutus-playground-server
```
If it doesn't exist, `plutus-playground-server` will generate the PureScript files inside `plutus-playground-client/generated` which are needed by the client to work properly. To re-generate the files on demand, pass a `-g` flag like so `plutus-playground-server -g`.

## Testing

Tests should be run with nix:

```sh
nix build -L -f default.nix plutus-apps.haskell.packages.plutus-playground-server.checks
```

You can then see the test output by looking in `result/test-stdout`:

```sh
cat result/test-stdout
```

# Introduction

This is a proof of concept JavaScript library for generating Plutus transactions with a given set of constraints and lookups. It exposes functionality from the `plutus-ledger-constraints` package.

# Usage

Build the `mktx-lib` executable with nix/cabal and then make the JavaScript distribution by running `mktx-dist.sh`. This creates the library `mktx-lib.js` and a test/example program `mktx-lib-test.js`. Include `mktx-lib.js` in your project to use the library.

Before transactions can be exported, the emscripten runtime needs to be initialized. It's possible to wait for initialization as follows:

```
Module['onRuntimeInitialized'] = function() {
    // run plutus code here
};
```

Signing a transaction depends on the current protocol parameters. These have to be supplied as a JSON object. Make sure that you keep the values up to date. The data in the test program was taken from the `plutus-use-cases` project (`protocol-parameters.json`).

# Design

This section gives an overview of some design decisions for this project.

## Project structure

While GHC(JS) supports `foreign export` declarations, these are limited to top-level functions. That makes them quite impractical to use for a larger JavaScript API. In addition to this, cabal does not yet support the `foreign-library` stanza for a GHCJS library.

Therefore, the project is an `executable`, with `main` just setting up the JavaScript API. This allows us to have an hierarchical API, with for example `plutus.constraint` for constraints, and `plutus.offchain` for offchain transaction construction.

The `main` action sets up the callbacks, JavaScript functions that call Haskell functionality, and then calls `plutus_apps_init_API` to initialise the global `plutus` object.

## JavaScript API

The functionality of the library mostly comes from the `plutus-ledger-constraints` package, in particular the module `Ledger.Constraints` for creating the transaction constraints and `Ledger.Constraints.OffChain` for manipulating script lookups and creating the transaction itself.

In JavaScript, the functionality is grouped in the `plutus.constraints` and `plutus.offchain` modules. The exposed JavaScript functions are mostly a direct equivalent of the Haskell implementation. Most of the work of this library is converting the arguments and return datatypes.

Most required Haskell types have an `aeson` instance for conversion between JSON and Haskell. However the JSON instance is typically not very friendly to use, and quite fragile, in the sense that any change in the underlying Haskell types also changes the JSON structure. We don't want the user to be affected by these internals.

For this reason, the workflow for using this library is as follows:

  1. construct initial transaction constraints using the `plutus.constraints` API functions that take "friendly" inputs and return a transaction constraints object.
  2. combine/manipulate the transaction constraints as needed, also using the `plutus.constraints` API. While the transaction constraints are JavaScript objects, they should be treated as "internal", i.e. they should not be manipulated directly, since the structure can change.
  3. create and export the transaction with the final transaction constraints object.

The situation for script lookups is analogous.

In the initial version of this library, not all JavaScript types are very friendly to use, and some still expose too many Haskell internals to be considered stable. This can be remedied later by improving the `JSData` converters.

## Error Handling

A common way to represent actions that can fail in Haskell is with an `Either error success` result type, where `Left error` is for example used to indicate that a required lookup could not be found. However there is no standard way to represent `Either` in JavaScript, and no way to safely chain multiple "failable" actions. Morever, JavaScript being untyped, additional errors can occur, such as malformed JSON data for the inputs.

Therefore, we have chosen to not use `Either` return values, but instead raise a JavaScript exception on any error condition.

## C Code and Cryptography

Plutus transactions depend on cryptography from the `cryptonite` and `cardano-crypto` packages. The low-level crypto routines are written in C. For use in this library, the C code is compiled to JavaScript with emscripten.

Due to the difference in representation of memory and pointers between Haskell (GHCJS) and C (emscripten), a small "impedance matching" wrapper is used for each imported C function called from Haskell. These wrappers are typically a few lines of code that copy some data between the Haskell and C data structures.

The emscripten compiled code and the wrappers are included with patched packages in the nix build.

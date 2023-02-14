#!/usr/bin/env bash

# TODO(std) remove this after https://github.com/input-output-hk/plutus-apps/pull/951
nix-build default.nix -A build-and-push-devcontainer-script -o build-and-push-image.sh
./build-and-push-image.sh

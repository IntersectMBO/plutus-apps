#!/usr/bin/env bash
nix-shell ../default.nix --arg project ../servant-purescript --arg packages 'import ./packages.nix'

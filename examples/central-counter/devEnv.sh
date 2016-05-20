#!/usr/bin/env bash
nix-shell /home/robert/projects --arg project ../central-counter --arg packages 'import ./packages.nix {}'

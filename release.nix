{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, rootsOnly ? false
  # We explicitly pass true here in the GitHub action but don't want to slow down hydra
, checkMaterialization ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { system = builtins.currentSystem; } // sourcesOverride
, plutus-apps ? { outPath = ./.; rev = "abcdef"; }
}:
let
  traceNames = prefix: builtins.mapAttrs (n: v:
    if builtins.isAttrs v
    then if v ? type && v.type == "derivation"
    then __trace ("found job " + prefix + n) v
    else __trace ("looking in " + prefix + n) traceNames (prefix + n + ".") v
    else v);
  inherit (import (sources.plutus-core + "/nix/lib/ci.nix")) stripAttrsForHydra filterDerivations derivationAggregate;

  ci = import ./ci.nix {
    inherit supportedSystems rootsOnly checkMaterialization sourcesOverride sources;
    plutus-apps-commit = plutus-apps;
  };
  # ci.nix is a set of attributes that work fine as jobs (albeit in a slightly different structure, the platform comes
  # first), but we mainly just need to get rid of some extra attributes.
  ciJobsets = stripAttrsForHydra (filterDerivations ci);
  # Don't require darwin jobs to succeed for now, until the mac builders are fixed
  requiredJobsets = builtins.removeAttrs ciJobsets [ "darwin" ];
in
traceNames "" (ciJobsets // { required = derivationAggregate "required-plutus-apps" requiredJobsets; })

{ pkgs
, gitignore-nix
, fixStylishHaskell
, fixPurty
, fixPngOptimization
, src
, play-generated
, plutus-playground
, web-ghc
, docs
, vmCompileTests ? false
}:
let
  inherit (pkgs) lib;
  cleanSrc = gitignore-nix.gitignoreSource src;
in
pkgs.recurseIntoAttrs {
  shellcheck = pkgs.callPackage ./shellcheck.nix { src = cleanSrc; };

  stylishHaskell = pkgs.callPackage ./stylish-haskell.nix {
    src = cleanSrc;
    inherit fixStylishHaskell;
  };

  generated = pkgs.callPackage ./generated.nix {
    src = cleanSrc;
    inherit play-generated;
  };

  purty = pkgs.callPackage ./purty.nix {
    src = cleanSrc;
    inherit fixPurty;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    src = cleanSrc;
    inherit (pkgs) nixpkgs-fmt;
  };

  pngOptimization = pkgs.callPackage ./png-optimization.nix {
    src = cleanSrc;
    inherit fixPngOptimization;
  };

  vmTests = pkgs.callPackage ./vm.nix {
    inherit vmCompileTests plutus-playground web-ghc docs;
  };
}

{ pkgs
, gitignore-nix
, fixStylishHaskell
, fix-purs-tidy
, fixCabalFmt
, fixPngOptimization
, src
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

  purs-tidy = pkgs.callPackage ./purs-tidy.nix {
    src = cleanSrc;
    inherit fix-purs-tidy;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    src = cleanSrc;
    inherit (pkgs) nixpkgs-fmt;
  };

  cabalFmt = pkgs.callPackage ./cabal-fmt.nix {
    src = cleanSrc;
    inherit fixCabalFmt;
  };

  pngOptimization = pkgs.callPackage ./png-optimization.nix {
    src = cleanSrc;
    inherit fixPngOptimization;
  };

  vmTests = pkgs.callPackage ./vm.nix {
    inherit vmCompileTests plutus-playground web-ghc docs;
  };
}

{ pkgs
, gitignore-nix
, fixStylishHaskell
, fix-purs-tidy
, fixCabalFmt
, fixPngOptimization
, src
, play-generated
, nami-generated
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
    inherit play-generated nami-generated;
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

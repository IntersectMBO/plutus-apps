{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Cabal, containers, directory, filepath
      , generic-deriving, HUnit, stdenv, text
      }:
      mkDerivation {
        pname = "purescript-bridge";
        version = "0.2.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers directory filepath generic-deriving text
        ];
        testHaskellDepends = [ base Cabal HUnit ];
        description = "Generate PureScript data types from Haskell data types";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

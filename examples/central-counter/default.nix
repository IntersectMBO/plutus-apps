{ mkDerivation, aeson, base, containers, filepath, ghc-mod
, http-api-data, http-types, lens, mainland-pretty, mtl
, purescript-bridge, servant, servant-foreign, servant-purescript
, servant-server, servant-subscriber, stdenv, text, transformers
, wai, warp
}:
mkDerivation {
  pname = "central-counter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers filepath ghc-mod http-api-data http-types
    lens mainland-pretty mtl purescript-bridge servant servant-foreign
    servant-purescript servant-server servant-subscriber text
    transformers wai warp
  ];
  homepage = "https://github.com/eskimor/servant-purescript/tree/master/examples/distributed-counter";
  description = "Example project, putting servant-purescript, servant-subscriber and purescript-bridge to use";
  license = stdenv.lib.licenses.bsd3;
}

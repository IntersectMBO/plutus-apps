# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#39-nixcinix

{ system, l, ... }:

{

  excludedPaths =
    l.optionals (system != "x86_64-linux") [
      "packages.devcontainer-docker-image"
      "packages.devcontainer-push-docker-image"
    ];

}


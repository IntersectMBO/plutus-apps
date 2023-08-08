{ system, l, ... }:

{

  excludedPaths = 
    l.optionals (system != "x86_64-linux") [
      "packages.devcontainer-docker-image"
      "packages.devcontainer-push-docker-image"
    ];
    
}


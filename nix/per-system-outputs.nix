{ nix, projects, ... }:

rec {

  packages.generated-puc-scripts-output = 
    nix.plutus-apps.generated-puc-scripts-outputs projects.default;


  packages.plutus-playground-client-entrypoint = 
    nix.plutus-apps.plutus-playground-client-entrypoint;


  packages.devcontainer-docker-image = 
    nix.plutus-apps.devcontainer-docker-image projects.default;

  
  packages.devcontainer-push-docker-image = 
    nix.plutus-apps.devcontainer-push-docker-image 
      packages.devcontainer-docker-image;

}
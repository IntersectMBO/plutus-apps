{ repoRoot, inputs, pkgs, lib, system }:
let
  project = repoRoot.nix.project;
in
[
  (
    project.flake
  )
  {
    packages.generated-puc-scripts-output = repoRoot.nix.generated-puc-scripts-output;
    packages.plutus-playground-client-entrypoint = repoRoot.nix.plutus-playground-client-entrypoint;

    packages.devcontainer-docker-image = repoRoot.nix.devcontainer-docker-image;
    packages.devcontainer-push-docker-image = repoRoot.nix.devcontainer-push-docker-image;
  }
  {
    hydraJobs.generated-puc-scripts-output = repoRoot.nix.generated-puc-scripts-output;
    hydraJobs.plutus-playground-client-entrypoint = repoRoot.nix.plutus-playground-client-entrypoint;
  }
  (lib.optionalAttrs (system == "x86_64-linux")
    {
      hydraJobs.devcontainer-docker-image = repoRoot.nix.devcontainer-docker-image;
      hydraJobs.devcontainer-push-docker-image = repoRoot.nix.devcontainer-push-docker-image;
    })
]

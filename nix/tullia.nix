/*
  This file defines tullia tasks and cicero actions.
  Tullia is a sandboxed multi-runtime DAG task runner with Cicero support.
  Tasks can be written in different languages and are compiled for each runtime using Nix.
  It comes with essential building blocks for typical CI/CD scenarios.
  Learn more: https://github.com/input-output-hk/tullia
  Cicero is an if-this-then-that machine on HashiCorp Nomad.
  It can run any event-and-state-driven automation actions
  and hence CI/CD pipelines are a natural fit.
  In tandem with Tullia, an action could be described as
  the rule that describes when a Tullia task is to be invoked.
  Learn more: https://github.com/input-output-hk/cicero
*/

let
  ciInputName = "GitHub event";
  repository = "input-output-hk/plutus-apps";
  ciTaskTopAttr = "ciJobs";

in
rec {

  tasks.ci = { config, lib, ... }: {
    preset = {
      nix.enable = true;

      github.status = {
        enable = config.actionRun.facts != { };
        inherit repository;
        revision = config.preset.github.lib.readRevision ciInputName null;
      };
    };

    command.text =
      let
        flakeUrl = ''github:${repository}/"$(${lib.escapeShellArg config.preset.github.status.revision})"'';
      in
      config.preset.github.status.lib.reportBulk {
        bulk.text = ''
          nix eval .#ciJobs --apply __attrNames --json | # all systems the flake declares
          nix-systems -i | # figure out which the current machine is able to build
          jq 'with_entries(select(.value))' # only keep those we can build
        '';
        each.text = ''nix build -L ${flakeUrl}#${lib.escapeShellArg ciTaskTopAttr}."$1".required'';
        skippedDescription = lib.escapeShellArg "No nix builder available for this platform";
      };

    # some hydra jobs run NixOS tests
    env.NIX_CONFIG = ''
      extra-system-features = kvm
    '';

    memory = 1024 * 32;
    nomad.resources.cpu = 10000;
  };

  actions = {
    "plutus-apps/ci" = {
      task = "ci";
      io = ''
        // This is a CUE expression that defines what events trigger a new run of this action.
        // There is no documentation for this yet. Ask SRE if you have trouble changing this.
        let github = {
          #input: "${ciInputName}"
          #repo: "input-output-hk/plutus-apps"
        }
        #lib.merge
        #ios: [
          #lib.io.github_push & github,
          { #lib.io.github_pr, github, #target_default: false },
        ]
      '';
    };
  };
}

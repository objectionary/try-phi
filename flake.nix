{
  inputs = {
    nixpkgs_.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    flakes-tools.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=flakes-tools;
    my-codium.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=codium;
    my-devshell.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=devshell;
    drv-tools.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=drv-tools;
    purescript-tools.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=language-tools/purescript;
    workflows.url = "github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=workflows";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , flakes-tools
    , my-codium
    , drv-tools
    , purescript-tools
    , my-devshell
    , workflows
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkShellApps mkBin;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      pursTools = purescript-tools.toolSets.${system}.shellTools;
      inherit (my-devshell.functions.${system}) mkShell mkCommands;

      backDir = "back";
      frontDir = "front";
      scripts =
        (mkShellApps {
          back = {
            text = "cd ${backDir} && nix run";
            description = "Run backend";
          };
          front = {
            text = "cd ${frontDir} && nix run";
            description = "Run frontend";
          };
        }) // {
          writeSettings = writeSettingsJSON settingsNix;
          writeWorkflows = import ./nix-files/workflow.nix {
            name = "ci";
            inherit workflows backDir frontDir system;
          };
        };

      codiumTools = builtins.attrValues scripts;

      codium = mkCodium {
        extensions = { inherit (extensions) nix misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      tools = codiumTools ++ [ codium ];

      flakesTools = mkFlakesTools [ "front" "back" "." ];
    in
    {
      packages = {
        pushToCachix = flakesTools.pushToCachix;
        updateLocks = flakesTools.updateLocks;
      } // scripts;

      devShells.default = mkShell {
        packages = tools;
        commands = mkCommands "tools" tools;
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}

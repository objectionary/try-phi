{
  description = "Try-phi front end";

  inputs = {
    nixpkgs_.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    drv-tools.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=drv-tools;
    purescript-tools.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=language-tools/purescript;
    my-devshell.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=devshell;
    my-codium.url = github:deemp/flakes/8ee5d35e592860636adb57cee3e27c98de04202a?dir=codium;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , drv-tools
    , purescript-tools
    , my-devshell
    , my-codium
    , ...
    }:
      with flake-utils.lib;
      eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (builtins) attrValues;
        shellTools = purescript-tools.shellTools.${system};
        inherit (my-devshell.functions.${system}) mkShell mkCommands;
        inherit (drv-tools.functions.${system}) mkShellApps;
        inherit (my-codium.configs.${system}) extensions settingsNix;
        inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;

        scripts = mkShellApps {
          default = {
            text = "npm run quick-start";
            runtimeInputs = [ shellTools.nodejs-16_x ];
          };
          buildGHPages = {
            text = ''npm run build:gh-pages'';
            runtimeInputs = [ shellTools.nodejs-16_x ];
          };
        };

        codiumTools = attrValues { inherit (shellTools) purescript nodejs-16_x spago; };
        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github markdown purescript; };
          runtimeDependencies = codiumTools ++ (attrValues {
            inherit (shellTools) dhall-lsp-server purescript-language-server purs-tidy;
          });
        };

        tools = codiumTools ++ [ codium ];
      in
      {
        packages = scripts;
        devShells.default = mkShell {
          packages = tools;
          commands = mkCommands "tools" tools;
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
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

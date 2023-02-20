{
  description = "Try-phi front end";

  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    drv-tools.url = github:deemp/flakes?dir=drv-tools;
    purescript-tools.url = github:deemp/flakes?dir=language-tools/purescript;
    devshell.url = github:deemp/flakes?dir=devshell;
    codium.url = github:deemp/flakes?dir=codium;
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        shellTools = inputs.purescript-tools.shellTools.${system};
        inherit (inputs.devshell.functions.${system}) mkShell mkCommands mkRunCommands;
        inherit (inputs.drv-tools.functions.${system}) mkShellApps;
        inherit (inputs.codium.configs.${system}) extensions settingsNix;
        inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
        inherit (builtins) attrValues;
        
        scripts = mkShellApps {
          default = {
            text = "npm run quick-start";
            runtimeInputs = [ shellTools.nodejs-16_x ];
            description = "Run front";
          };
          buildGHPages = {
            text = ''npm run build:gh-pages'';
            runtimeInputs = [ shellTools.nodejs-16_x ];
            description = "Build GitHub Pages";
          };
        };

        tools = with shellTools; [ purescript nodejs-16_x spago ];

        packages = {
          codium = mkCodium {
            extensions = { inherit (extensions) nix misc github markdown purescript; };
            runtimeDependencies = tools ++ (attrValues {
              inherit (shellTools) dhall-lsp-server purescript-language-server purs-tidy;
            });
          };
          writeSettings = writeSettingsJSON settingsNix;
        } // scripts;

        devShells.default = mkShell {
          packages = tools;
          commands =
            mkCommands "tools" tools ++
            mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; } ++
            mkRunCommands "scripts" { inherit (packages) default buildGHPages; }
          ;
        };
      in
      {
        inherit packages devShells;
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

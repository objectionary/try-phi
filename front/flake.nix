{
  description = "Try-phi front end";

  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore_.url = github:deemp/flakes?dir=source-flake/gitignore;
    gitignore.follows = "gitignore_/gitignore";
    dream2nix_.url = github:deemp/flakes?dir=source-flake/dream2nix;
    dream2nix.follows = "dream2nix_/dream2nix";
    drv-tools.url = github:deemp/flakes?dir=drv-tools;
    purescript-tools.url = github:deemp/flakes?dir=language-tools/purescript;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , dream2nix
    , drv-tools
    , purescript-tools
    , ...
    }:
      with flake-utils.lib;
      eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nodeOutputs =
          dream2nix.lib.makeFlakeOutputs {
            systems = [ system ];
            config.projectRoot = ./.;
            source = gitignore.lib.gitignoreSource ./.;
            settings = [
              {
                subsystemInfo.nodejs = 16;
              }
            ];
          };
        shellTools = purescript-tools.toolSets.${system}.shellTools;
        inherit (drv-tools.functions.${system}) mkShellApps;
        scripts = mkShellApps {
          default = {
            text = "npm run quick-start";
            runtimeInputs = [ shellTools.nodejs-16_x ];
          };
        };
      in
      {
        packages = scripts;
        devShells.default = nodeOutputs.devShells.${system}.default.overrideAttrs (fin: prev: {
          buildInputs = prev.buildInputs ++ (builtins.attrValues shellTools);
        });
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

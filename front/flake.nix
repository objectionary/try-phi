{
  description = "Try-phi front end";

  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/gitignore;
    gitignore.follows = "gitignore_/gitignore";
    dream2nix_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/dream2nix;
    dream2nix.follows = "dream2nix_/dream2nix";
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    purescript-tools.url = github:br4ch1st0chr0n3/flakes?dir=language-tools/purescript;
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
        pursTools = purescript-tools.packages.${system};
        inherit (drv-tools.functions.${system}) mkShellApps;
        scripts = mkShellApps {
          default = {
            text = "npm run quick-start";
            runtimeInputs = [ pursTools.nodejs-16_x ];
          };
        };
      in
      {
        packages = scripts;
        devShells.default = nodeOutputs.devShells.${system}.default;
      });
}

{
  description = "Try-phi back end";
  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore_.url = github:deemp/flakes?dir=source-flake/gitignore;
    gitignore.follows = "gitignore_/gitignore";
    haskell-tools.url = github:deemp/flakes?dir=language-tools/haskell;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , haskell-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (haskell-tools.functions.${system}) haskellTools;
      inherit (haskellTools "902" { } (_: [ ]) [ ]) stack callCabal justStaticExecutable callCabal2nix;

      name = "back";
      
      back =
        let
          eo-utils = callCabal2nix "eo-utils" ./language-utils/eo-utils { };
          phi-utils = callCabal2nix "phi-utils" ./language-utils/phi-utils { };
          language-utils = callCabal2nix "language-utils" ./language-utils {
            inherit phi-utils eo-utils;
          };
          back_ = callCabal2nix "try-phi-back" ./. {
            inherit language-utils phi-utils eo-utils;
          };
        in
        justStaticExecutable name back_;

      backDocker = pkgs.dockerTools.buildLayeredImage {
        name = back.name;
        tag = "latest";
        contents = [ back ];
        config.Entrypoint = [ name ];
      };
    in
    {
      packages = {
        default = back;
      };

      devShells = {
        default = pkgs.mkShell {
          shellHook = ''
            export LANG="C.UTF-8";
          '';
          buildInputs = [ back ];
        };
      };

      images = {
        back = backDocker;
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

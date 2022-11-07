{
  description = "Try-phi back end";
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/gitignore;
    gitignore.follows = "gitignore_/gitignore";
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    haskell-tools.url = github:br4ch1st0chr0n3/flakes?dir=language-tools/haskell;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , drv-tools
    , haskell-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (haskell-tools.functions.${system})
        toolsGHC
        ;
      inherit (toolsGHC ghcVersion) stack callCabal justStaticExecutables;

      try-phi-back =
        let
          eo-utils = callCabal "eo-utils" ./language-utils/eo-utils { };
          phi-utils = callCabal "phi-utils" ./language-utils/phi-utils { };
          language-utils = callCabal "language-utils" ./language-utils {
            inherit phi-utils eo-utils;
          };
          back = callCabal "try-phi-back" ./. {
            inherit language-utils phi-utils eo-utils;
          };
        in
        justStaticExecutables back;

      back = pkgs.stdenv.mkDerivation {
        buildInputs = [ try-phi-back ];
        name = "back";
        src = self;
        installPhase = "
          mkdir -p $out/bin
          ln -s ${try-phi-back}/bin/try-phi-back-exe $out/bin/back
        ";
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
        };
      };
    });
}

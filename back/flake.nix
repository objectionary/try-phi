{
  description = "Try-phi back end";
  inputs = {
    inputs.url = "github:br4ch1st0chr0n3/flakes?dir=inputs";
    nixpkgs.follows = "inputs/nixpkgs";
    flake-utils.follows = "inputs/flake-utils";
    gitignore.follows = "inputs/gitignore";
    my-codium.follows = "inputs/my-codium";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , my-codium
    , inputs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (my-codium.tools.${system})
        allShellTools
        toolsGHC
        justStaticExecutables
        ;
      
      inherit (toolsGHC ghcVersion) stack callCabal;
      
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

      tools = builtins.attrValues {
        inherit (allShellTools)
          implicit-hie
          ghcid
          ;
        inherit stack;
      };
    in
    {
      packages = {
        default = back;
      };

      devShells = {
        default = pkgs.mkShell {
          buildInputs = tools ++ [back];

          # https://stackoverflow.com/a/63751678
          shellHook = ''
            export LANG="C.UTF-8";
          '';
        };
      };
    });
}

{
  description = "Try-phi back end";
  inputs = {
    # common-flake.url = "github:objectionary/try-phi?dir=common-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-codium = {
      url = "github:br4ch1st0chr0n3/flakes?dir=codium&rev=644841e48b858417353b85c97dfc06e234025145";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , my-codium
    # , common-flake
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
          buildInputs = tools;

          # https://stackoverflow.com/a/63751678
          shellHook = ''
            export LANG="C.UTF-8";
          '';
        };
      };
    });
}

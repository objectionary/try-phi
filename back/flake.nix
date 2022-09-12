{
  description = "Try-phi back end";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      # need to match Stackage LTS version from stack.yaml resolver
      hPkgs = pkgs.haskell.packages."ghc902";

      try-phi-back =
        let
          inherit (hPkgs) callCabal2nix;
          inherit (pkgs.haskell.lib) justStaticExecutables;
          inherit (gitignore.lib) gitignoreSource;

          eo-utils = callCabal2nix "eo-utils" (gitignoreSource ./language-utils/eo-utils) { };
          phi-utils = callCabal2nix "phi-utils" (gitignoreSource ./language-utils/phi-utils) { };
          language-utils = callCabal2nix "language-utils" (gitignoreSource ./language-utils) {
            inherit phi-utils eo-utils;
          };
          back = callCabal2nix "try-phi-back" (gitignoreSource ./.) {
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
          buildInputs = [ back ];

          # https://stackoverflow.com/a/63751678
          shellHook = ''
            export LANG="C.UTF-8";
          '';
        };
      };
    });
}

{
  description = "Try-phi back end";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils";

    # https://discourse.nixos.org/t/recommendations-for-use-of-flakes-input-follows/17413
    hls = {
      url = "github:haskell/haskell-language-server?rev=9a0684ec28022f644de4f8a0eb3610ea7a8dd2eb";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://cachix.cachix.org"
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };


  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , hls
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      # https://discourse.nixos.org/t/using-nixpkgs-legacypackages-system-vs-import/17462
      pkgs = nixpkgs.legacyPackages.${system};

      back =
        let
          inherit (hPkgs) callCabal2nix;
          inherit (pkgs.haskell.lib) justStaticExecutables;
          inherit (gitignore.lib) gitignoreSource;

          eo-utils = callCabal2nix "eo-utils" (gitignoreSource ./language-utils/eo-utils) {};
          phi-utils = callCabal2nix "phi-utils" (gitignoreSource ./language-utils/phi-utils) {};
          language-utils = callCabal2nix "language-utils" (gitignoreSource ./language-utils) {
            inherit phi-utils eo-utils;
          };
          back = callCabal2nix "back" (gitignoreSource ./.) {
            inherit language-utils phi-utils eo-utils;
          };
        in
        justStaticExecutables back;

      # need to match Stackage LTS version
      # from stack.yaml resolver
      ghcVersion = "902";
      hPkgs = pkgs.haskell.packages."ghc${ghcVersion}";
      haskell-language-server = hls.packages.${system}."haskell-language-server-${ghcVersion}";

      myDevTools = builtins.attrValues {
        inherit (hPkgs)
          ghc# GHC compiler in the desired version (will be available on PATH)
          ghcid# Continous terminal Haskell compile checker
          fourmolu# Haskell formatter
          ormolu# Haskell formatter
          hlint# Haskell codestyle checker
          hoogle# Lookup Haskell documentation
          implicit-hie# auto generate LSP hie.yaml file from cabal
          retrie# Haskell refactoring tool
          ;
        inherit stack-wrapped;
        inherit haskell-language-server;
      };

      # Wrap Stack to work with our Nix integration. 
      stack-wrapped = pkgs.symlinkJoin {
        # will be available as the usual `stack` in terminal
        name = "stack";
        paths = [ pkgs.stack ];
        buildInputs = [ pkgs.makeWrapper ];
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };
    in
    {
      packages = {
        default = back;
      };

      devShells = {
        default = pkgs.mkShell {
          buildInputs = myDevTools;

          # https://stackoverflow.com/a/63751678
          shellHook = ''
            export LANG="C.UTF-8";
          '';

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      };
    });
}

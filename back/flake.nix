{
  description = "A very basic flake";
  # inputs.haskellNix.url = "github:input-output-hk/haskell.nix/hkm/hls-ghc92";
  # inputs.haskellNix.url = "github:input-output-hk/haskell.nix/";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/?rev=9c697ee23a78af9326b5c8849e675a380e46a69a";

  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # index-state = "2022-08-15T00:00:00Z";
  nixConfig = {
    extra-substituters = "https://cache.nixos.org/ https://cache.iog.io";
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs

            helloProject =
              final.haskell-nix.project {
                src = ./.;
                compiler-nix-name = "ghc8107";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools =
                  let
                    index-state = "2022-08-15T00:00:00Z";
                  in
                  builtins.mapAttrs (n: v: { inherit index-state; })
                    {
                      cabal = { };
                      hlint = { };
                      haskell-language-server = { };
                    };
                # Non-Haskell shell tools go here
                # shell.buildInputs = with pkgs; [
                #   nixpkgs-fmt
                # ];
                # This adds `js-unknown-ghcjs-cabal` to the shell.
                # shell.crossPlatforms = p: [p.ghcjs];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.helloProject.flake {
          # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
      in
      flake // {
        # Built by `nix build .`
        packages = flake.packages // {
          default = flake.packages."try-phi-back:exe:try-phi-back-exe";
          try-phi-back = flake.packages."try-phi-back:exe:try-phi-back-exe";
          eo-utils = flake.packages."eo-utils:exe:eo-utils-exe";
          phi-utils = flake.packages."phi-utils:exe:phi-utils-exe";
          language-utils = flake.packages."language-utils:exe:language-utils-exe";
        };
      });
}

# {
#   description = "CLI for Hosted Nix binary caches";
#   inputs = {
#     nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
#     flake-utils.url = "github:numtide/flake-utils";
#     hls.url = "github:haskell/haskell-language-server/master";
#   };

#   nixConfig = {
#     # This sets the flake to use the IOG nix cache.
#     # Nix should ask for permission before using it,
#     # but remove it here if you do not want it to.
#     extra-substituters = [ "https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/" ];
#     extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
#     allow-import-from-derivation = "true";
#   };

#   outputs = { self, nixpkgs, flake-utils, hls }:
#     with flake-utils;
#     lib.eachDefaultSystem (system:
#       let
#         pkgs = nixpkgs.legacyPackages.${system};

#         # cachix-api = pkgs.haskellPackages.callCabal2nix "" (pkgs.gitignoreSource ./cachix-api) { };
#         # cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) {
#         #   inherit cachix-api;
#         #   # note: this has to be in sync with what hercules-ci-cnix-store was compiled with
#         #   nix = pkgs.nixVersions.nix_2_7;
#         # };
#         # packages = pkgs.haskell.lib.justStaticExecutables cachix;

#         hPkgs =
#           pkgs.haskell.packages."ghc902"; # need to match Stackage LTS version
#         # from stack.yaml resolver

#         myDevTools =
#           with hPkgs; [
#             ghc # GHC compiler in the desired version (will be available on PATH)
#             ghcid # Continous terminal Haskell compile checker
#             ormolu # Haskell formatter
#             hlint # Haskell codestyle checker
#             hoogle # Lookup Haskell documentation
#             hls.packages."x86_64-linux"."haskell-language-server-902"
#             implicit-hie # auto generate LSP hie.yaml file from cabal
#             retrie # Haskell refactoring tool
#             hspec-discover
#             releaser
#             hkgr
#             niv
#             stack-wrapped
#           ];

#         # Wrap Stack to work with our Nix integration. We don't want to modify
#         # stack.yaml so non-Nix users don't notice anything.
#         # - no-nix: We don't want Stack's way of integrating Nix.
#         # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
#         # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
#         stack-wrapped = pkgs.symlinkJoin {
#           name = "stack"; # will be available as the usual `stack` in terminal
#           paths = [ pkgs.stack ];
#           buildInputs = [ pkgs.makeWrapper ];
#           postBuild = ''
#             wrapProgram $out/bin/stack \
#               --add-flags "\
#                 --system-ghc \
#                 --no-install-ghc \
#               "
#           '';
#         };
#       in
#       {
#         packages.${system}.default = pkgs.try-phi;
#         devShells.default = pkgs.mkShell {
#           buildInputs = myDevTools;

#           # Make external Nix c libraries like zlib known to GHC, like
#           # pkgs.haskell.lib.buildStackProject does
#           # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
#           LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
#           shellHook = ''stack build'';
#         };
#       });
# }

{
  inputs =
    {
      flake-utils.url = "github:numtide/flake-utils";

      nix-filter.url = "https://github.com/numtide/nix-filter/archive/3c9e33ed627e009428197b07216613206f06ed80.tar.gz";
      # nix-filter.url = "github:numtide/nix-filter";
      # nix-filter.follows = "haskellNix/nixpkgs-unstable";

      haskellNix.url = "github:input-output-hk/haskell.nix/hkm/hls-ghc92";
      nixpkgs.follows = "haskellNix/nixpkgs-unstable";

      # hls.url = "github:haskell/haskell-language-server";
      # hls.follows = "haskellNix/nixpkgs-unstable";

      # hls.follows = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };

  # binary caches
  nixConfig = {
    extra-substituters = "https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/";
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , flake-utils
    , haskellNix
      # , hls
    , nix-filter
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      compiler-version = "902";
      compiler-nix-name = "ghc${compiler-version}";
      # index-state = "2022-08-08T00:00:00Z";
      # pkgs = import nixpkgs {
      #   system = "x86_64-linux";
      # };

      # defaultDerivation = pkgs.stdenv.mkDerivation { name = "default"; src = "https://github.com/numtide/flake-utils"; };

      # projectPackages = [
      #   { name' = "phi-utils"; src' = ./language-utils/phi-utils; }
      # ];

      # for = xs: f: builtins.map f xs;
      # toAttr = name: value: builtins.listToAttrs [{ inherit name value; }];
      # overlays = for projectPackages
      #   (
      #     { name', src' }:
      #     haskellNix.overlay
      #       (self: super: toAttr name' (
      #         self.haskell-nix.cabalProject' {
      #           src = self.haskell-nix.haskellLib.cleanSourceWith {
      #             name = name';
      #             src = src';
      #           };
      #           inherit compiler-nix-name index-state;
      #           # materialized = ./nix/materialized/phi-utils;
      #         }))
      #   );

      # hsPkgs = pkgs.haskell-nix.cabalProject {
      #   src = nix-filter {
      #     root = ./.;
      #     name = "try-phi";
      #     include = [
      # ./language-utils/eo-utils/eo-utils.cabal
      # ./language-utils/phi-utils/phi-utils.cabal
      # ./language-utils/language-utils.cabal
      # ./try-phi-back.cabal
      #     ];
      #   };
      # };

      # hsPkgs = pkgs.haskell-nix.cabalProject {
      #   src = nix-filter {
      #     root = ./.;
      #     name = "try-phi-back";
      #     include = [
      #       ./language-utils/eo-utils/eo-utils.cabal
      #       ./language-utils/phi-utils/phi-utils.cabal
      #       ./language-utils/language-utils.cabal
      #       ./try-phi-back.cabal
      #       ./cabal.project
      #     ];
      #   };

      #   modules = [
      #     {
      #       packages = {
      #         try-phi-back.src = ./.;
      #         language-utils.src = ./language-utils;
      #         eo-utils.src = ./language-utils/eo-utils;
      #         phi-utils.src = ./language-utils/phi-utils;
      #       };
      #     }
      #   ];
      # };


      overlays =
        [
          haskellNix.overlay
          (self: super: {
            # ({
            try-phi-back = self.haskell-nix.project' {
              src = (import nix-filter) {
                root = ./.;
                name = "try-phi-back";
                # include = [
                #   # ./language-utils/eo-utils/eo-utils.cabal
                #   # ./language-utils/phi-utils/phi-utils.cabal
                #   # ./language-utils/language-utils.cabal
                #   # ./try-phi-back.cabal
                #   # ./cabal.project
                #   ./language-utils/eo-utils
                #   ./language-utils/phi-utils
                #   ./language-utils
                # ];
              };

              modules = [
                {
                  packages = {
                    try-phi-back.src = ./.;
                    language-utils.src = ./language-utils;
                    eo-utils.src = ./language-utils/eo-utils;
                    phi-utils.src = ./language-utils/phi-utils;
                  };
                }
              ];
              # phi-utils = self.haskell-nix.cabalProject' {
              # phi-utils = {
              # src = ./.;
              name = "try-phi-back";
              # src = nix-filter
              #   {
              #     root = ./.;
              #     name = "try-phi-back";
              #     include = [
              #       ./language-utils/eo-utils/eo-utils.cabal
              #       ./language-utils/phi-utils/phi-utils.cabal
              #       ./language-utils/language-utils.cabal
              #       ./try-phi-back.cabal
              #     ];
              #   };
              # src = self.haskell-nix.haskellLib.cleanSourceWith {
              #   # src = {
              #   name = "phi-utils";
              #   src = ./language-utils/phi-utils;
              # };
              inherit compiler-nix-name;
              # index-state;
              # materialized = ./nix/materialized/phi-utils;
            };
          })
        ];
      #     haskellNix.overlay
      #     (self: super: {
      #       # ({

      #       eo-utils = self.haskell-nix.cabalProject' {
      #         # phi-utils = {
      #         src = self.haskell-nix.haskellLib.cleanSourceWith {
      #           # src = {
      #           name = "eo-utils";
      #           src = ./language-utils/eo-utils;
      #         };
      #         inherit compiler-nix-name index-state;
      #         # materialized = ./nix/materialized/phi-utils;
      #       };
      #     })
      #     haskellNix.overlay
      #     (self: super: {
      #       language-utils = self.haskell-nix.cabalProject' {
      #         src = self.haskell-nix.haskellLib.cleanSourceWith {
      #           name = "language-utils";
      #           src = ./language-utils;
      #         };
      #         inherit compiler-nix-name index-state;
      #         # materialized = ./nix/materialized/try-phi-back;
      #       };
      #       # shell.tools = [
      #       #   super.eo-utils
      #       #   super.phi-utils
      #       # ];
      #     })
      #     # haskellNix.overlay
      #     # (self: super: {
      #     #   try-phi-back = self.haskell-nix.cabalProject' {
      #     #     src = self.haskell-nix.haskellLib.cleanSourceWith {
      #     #       name = "try-phi-back";
      #     #       src = ./.;
      #     #     };
      #     #     inherit compiler-nix-name index-state;
      #     #     # materialized = ./nix/materialized/try-phi-back;
      #     #   };
      #     # })
      #   ];

      # pkgsSrc = import haskellNix.nixpkgs-unstable;

      # pkgs = (import nixpkgs) (haskellNix.nixpkgsArgs // { inherit system; });
      pkgs = import nixpkgs {
        # inherit system;
        inherit system overlays;
        inherit (haskellNix) config;
      };

      # flake = pkgs.language-utils.flake { };
      # flake = hsPkgs.try-phi-back.flake { };
      flake = pkgs.try-phi-back.flake { };
      # flake = pkgs.phi-utils.flake { };
      # flake = { };

    in
    # hsPkgs // {
    flake // {
      # packages.default = hsPkgs.try-phi-back.components.exes.try-phi-back-exe;
      # flake.packages."language-utils:exe:language-utils-exe";
      # packages.default = flake.packages."language-utils:exe:language-utils-exe";
      # packages.default = flake.packages.try-phi-back;
      # "try-phi-back:exe:try-phi-back-exe";
      packages.default = flake.packages."try-phi-back:exe:try-phi-back-exe";
      # packages.default = flake.packages."phi-utils:exe:phi-utils-exe";
      # defaultPackage = defaultDerivation;
      # packages.default = defaultDerivation;
      # devShells.default = defaultDerivation;
      # packages.default = flake.packages.;
      # ps = pkgs;
      # devShells.default = pkgs.language-utils.shellFor {
      #   # devShells.default = pkgs.try-phi-back.shellFor {
      #   # devShells.default = pkgs.phi-utils.shellFor {

      #   # tools.hoogle = {
      #   #   version = "5.0.18.3";
      #   #   index-state = "2022-08-04T00:00:00Z";
      #   #   materialized = ./nix/materialized/hoogle;
      #   # };
      #   nativeBuildInputs = with pkgs; [
      #     cabal-install
      #     hpack
      #     haskell-language-server
      #     # hls.packages."x86_64-linux"."haskell-language-server-${compiler-version}"
      #   ];
      #   exactDeps = true;
      # };
    });
}

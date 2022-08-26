{
  description = "Try-phi front end";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/481f9b246d200205d8bafab48f3bd1aeb62d775b";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/5926981701ac781f08b02e31e4705e46b799299d";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils/c0e246b9b83f637f4681389ecabcb2681b4f3af0";
    npmlock2nix_ = {
      # url = "github:nix-community/npmlock2nix/5c4f247688fc91d665df65f71c81e0726621aaa8";
      url = "github:tlxzilia/npmlock2nix/f63dc087b144fb608e99e6239ceb69c68449482b";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , easy-purescript-nix
    , flake-utils
    , npmlock2nix_
    }:
      with flake-utils.lib;
      eachSystem [ system.x86_64-linux ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myTools =
          let
            easy-ps = import easy-purescript-nix { inherit pkgs; };
          in
          builtins.attrValues {
            inherit (pkgs) nodejs-16_x dhall-lsp-server node2nix;
            inherit (easy-ps) purs-0_15_4 spago purescript-language-server;
          };
        myShellHook = "spago build";
        npmlock2nix = import npmlock2nix_ { inherit pkgs; };
      in
      {
        devShells =
          {
            default = pkgs.mkShell {
              buildInputs = myTools;
            };
            node =
              (npmlock2nix.shell {
                src = ./.;
              }).overrideAttrs
                (final: prev:
                  {
                    buildInputs = prev.buildInputs ++ myTools;
                    shellHooks = "${prev.shellHooks or ""}\n${myShellHook}";
                  }
                );
          };
      });
}

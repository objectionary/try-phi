{
  description = "Try-phi front end";

  inputs = {
    common-flake.url = "github:objectionary/try-phi/common-flake?dir=common-flake";
    nixpkgs.follows = "common-flake/nixpkgs";
    flake-utils.follows = "common-flake/flake-utils";
    gitignore.follows = "common-flake/gitignore";
    easy-purescript-nix.follows = "common-flake/easy-purescript-nix"; 
    dream2nix.follows = "common-flake/dream2nix";
  };

  outputs =
    { self
    , common-flake
    , nixpkgs
    , easy-purescript-nix
    , flake-utils
    , gitignore
    , dream2nix
    }:
      with flake-utils.lib;
      eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myTools =
          let
            easy-ps = import easy-purescript-nix { inherit pkgs; };
          in
          builtins.attrValues {
            inherit (easy-ps) purs-0_15_4 spago;
          };
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
      in
      {
        devShells.default = nodeOutputs.devShells.${system}.default.overrideAttrs (fin: prev: {
          buildInputs = prev.buildInputs ++ myTools;
        });
      });
}

{
  description = "Try-phi front end";

  inputs = {
    inputs.url = "github:br4ch1st0chr0n3/flakes?dir=inputs";
    nixpkgs.follows = "inputs/nixpkgs";
    flake-utils.follows = "inputs/flake-utils";
    gitignore.follows = "inputs/gitignore";
    easy-purescript-nix.follows = "inputs/easy-purescript-nix"; 
    dream2nix.follows = "inputs/dream2nix";
  };

  outputs =
    { self
    , inputs
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

{
  description = "RealWorld spec in the PureScript Halogen framework";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, easy-purescript-nix, flake-utils }@inputs:
    with flake-utils.lib;
    eachSystem [ system.x86_64-linux ] (system:
      let
        name = "halogen-realworld";
      in
      {
        devShells.default =
          let
            pkgs = import nixpkgs { inherit system; };
            easy-ps = import easy-purescript-nix { inherit pkgs; };
          in
          pkgs.mkShell {
            inherit name;
            buildInputs = (with pkgs; [
              nodejs-16_x
              nixpkgs-fmt
            ]) ++ (with easy-ps; [
              purs
              spago
              purescript-language-server
              purs-tidy
              psa
            ]);
          } // {shellHooks = "spago build";};
      });
}

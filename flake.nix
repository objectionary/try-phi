{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    back.url = "./back";
    front.url = "./front";
  };

  outputs = { self, nixpkgs, flake-utils, back, front }:
    # flake-utils.lib.eachDefaultSystem (system:
    with flake-utils.lib;
    eachSystem [ system.x86_64-linux ] (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        # packages.default = back.packages."${system}".default;

        devShells.default = pkgs.mkShell {

          buildInputs = [ ];
        };

      });
}

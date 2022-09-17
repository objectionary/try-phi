{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/c97e777ff06fcb8d37dcdf5e21e9eff1f34f0e90";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-codium = {
      url = "github:br4ch1st0chr0n3/flakes?dir=codium&rev=348e827ef643d205130d27a265865bd6e224b112";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/5926981701ac781f08b02e31e4705e46b799299d";
      flake = false;
    };
    dream2nix = {
      url = "github:nix-community/dream2nix/26083c3a2ea6f5cc0da6540b51b2affb8c38be49";
    };
    nix-vscode-marketplace = {
      url = "github:AmeerTaweel/nix-vscode-marketplace/e6b8eb76872a6d1401bae61f93f0f87f16301463";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = inputs: { };
}

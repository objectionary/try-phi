{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/c97e777ff06fcb8d37dcdf5e21e9eff1f34f0e90";
    # flake-utils.url = "github:numtide/flake-utils";
    # gitignore = {
    #   url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    # my-codium = {
    #   url = "github:br4ch1st0chr0n3/flakes?dir=codium&rev=644841e48b858417353b85c97dfc06e234025145";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.flake-utils.follows = "flake-utils";
    # };
  };
  outputs = inputs: {};
}

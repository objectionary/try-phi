{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/c97e777ff06fcb8d37dcdf5e21e9eff1f34f0e90";
    flake-utils.url = "github:numtide/flake-utils/c0e246b9b83f637f4681389ecabcb2681b4f3af0";
    backend = {
      url = "path:./back";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.my-codium.follows = "my-codium";
    };
    frontend = {
      url = "path:./front";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.my-codium.follows = "my-codium";
    };
    my-codium = {
      url = "github:br4ch1st0chr0n3/flakes?dir=codium&rev=0e18f2af853e9cd5094f689bc1d0eb94de30356d";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs =
    { self
    , backend
    , frontend
    , flake-utils
    , nixpkgs
    , my-codium
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.packages.${system})
        writeSettingsJson
        settingsNix
        extensions
        codium
        mergeValues
        allShellTools
        haskellTools
        shellTools
        toList
        ;
      l = (writeSettingsJson settingsNix);
      tools = pkgs.lib.lists.flatten [
        (toList shellTools)
        codium
        (writeSettingsJson settingsNix)
      ];
    in
    {
      devShells =
        {
          default = pkgs.mkShell {
            buildInputs = tools;
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath tools;
            shellHook = ''
              write-settings
            '';
          };
          front = pkgs.mkShell {
            shellHook = "(cd front && npm run dev)";
          };
          back = pkgs.mkShell {
            shellHook = "(cd back && nix run)";
          };
        };
      inherit l;
    });
}

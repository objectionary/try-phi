{
  inputs = {
    common-flake.url = "github:objectionary/try-phi?dir=common-flake";
    nixpkgs.follows = "common-flake/nixpkgs";
    flake-utils.follows = "common-flake/flake-utils";
    my-codium.follows = "common-flake/my-codium";
    backend = {
      url = path:./back;
    };
    frontend = {
      url = path:./front;
    };
  };
  outputs =
    { self
    , backend
    , frontend
    , flake-utils
    , nixpkgs
    , my-codium
    , common-flake
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.tools.${system})
        writeSettingsJson
        settingsNix
        codium
        shellTools
        toList
        toolsGHC
        ;
      tools902 = builtins.attrValues ({
        inherit (toolsGHC "902") hls stack;
      });
      tools = pkgs.lib.lists.flatten [
        (toList shellTools)
        codium
        tools902
      ];
    in
    {
      devShells =
        {
          default = pkgs.mkShell {
            buildInputs = tools;
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath tools;
          };
          front = pkgs.mkShell {
            shellHook = "(cd front && npm run dev)";
          };
          back = pkgs.mkShell {
            shellHook = "(cd back && nix run)";
          };

          # nix develop .#write-settings
          # will write the settings.json file
          write-settings = pkgs.mkShell {
            buildInputs = [ (writeSettingsJson settingsNix) ];
            shellHook = "write-settings";
          };
        };
    });
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      https://br4ch1st0chr0n3-nix-managed.cachix.org
      "https://br4ch1st0chr0n3-flakes.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "br4ch1st0chr0n3-nix-managed.cachix.org-1:sDKsfgu5fCCxNwVhZg+AWeGvbLlEtZoyzkSNKRM/KAo="
      "br4ch1st0chr0n3-flakes.cachix.org-1:Dyc2yLlRIkdbq8CtfOe24QQhQVduQaezkyV8J9RhuZ8="
    ];
  };
}

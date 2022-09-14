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
    });
}

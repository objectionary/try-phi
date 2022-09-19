{
  inputs = {
    inputs.url = "github:br4ch1st0chr0n3/flakes?dir=inputs";
    nixpkgs.follows = "inputs/nixpkgs";
    flake-utils.follows = "inputs/flake-utils";
    my-codium.follows = "inputs/my-codium";
    nix-vscode-marketplace.follows = "inputs/nix-vscode-marketplace";
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
    , inputs
    , nix-vscode-marketplace
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.tools.${system})
        writeSettingsJson
        settingsNix
        shellTools
        toList
        codium
        toolsGHC
        ;
      tools902 = builtins.attrValues ({
        inherit (toolsGHC "902") hls stack;
      });
      tools = pkgs.lib.lists.flatten [
        (toList shellTools)
        tools902
      ];
    in
    {
      devShells =
        {
          # TODO gitignore settings.json
          # add write tasks.json via json2nix
          # add TODO tree template for Nix
          # load shell tools
          default = pkgs.mkShell {
            buildInputs = tools;
          };

          # run front
          front = pkgs.mkShell {
            shellHook = "(cd front && npm run dev)";
          };

          # run server
          back = pkgs.mkShell {
            shellHook = "(cd back && nix run)";
          };

          # start codium and 
          # write settings.json
          codium = pkgs.mkShell {
            buildInputs = [ codium (writeSettingsJson settingsNix) ];
            shellHook = ''
              write-settings-json
              codium .
            '';
          };

          # update all flakes
          update-flakes = pkgs.mkShell {
            shellHook = ''
              (cd front && nix flake update)
              (cd back && nix flake update)
              nix flake update
            '';
          };
        };
    });
  nixConfig = {
    extra-substituters = [
      https://nix-community.cachix.org
      https://br4ch1st0chr0n3-nix-managed.cachix.org
      https://br4ch1st0chr0n3-flakes.cachix.org
    ];
    extra-trusted-public-keys = [
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      br4ch1st0chr0n3-nix-managed.cachix.org-1:sDKsfgu5fCCxNwVhZg+AWeGvbLlEtZoyzkSNKRM/KAo=
      br4ch1st0chr0n3-flakes.cachix.org-1:Dyc2yLlRIkdbq8CtfOe24QQhQVduQaezkyV8J9RhuZ8=
    ];
  };
}

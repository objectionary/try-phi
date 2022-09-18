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
        # mkCodium
        codium
        extensions
        toolsGHC
        ;
      tools902 = builtins.attrValues ({
        inherit (toolsGHC "902") hls stack;
      });
      # codium =
      #   let
      #     inherit (nix-vscode-marketplace.packages.${system}) vscode open-vsx;
      #   in
      #   mkCodium extensions;
      tools = pkgs.lib.lists.flatten [
        (toList shellTools)
        tools902
      ];

      settings = settingsNix // {
        yaml = {
          "yaml.schemas" = {
            "https://json.schemastore.org/github-workflow" = "/.githhub/workflows/**/*.yml";
            "https://json.schemastore.org/github-action" = "/.githhub/actions/**/action.yml";
          };
        };
      };
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
            buildInputs = [ (writeSettingsJson settings) ];
            shellHook = "write-settings";
          };
          codium = pkgs.mkShell {
            buildInputs = codium;
            shellHook = "codium .";
          };
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

{
  description = "Try-phi front end";

  inputs = { };

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = (import ../.).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools devshell codium;
          purescript-tools = flakes.language-tools.purescript;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in

        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          pursPkgs = inputs.purescript-tools.packages.${system};
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands;
          inherit (inputs.drv-tools.lib.${system}) mkShellApps;
          inherit (inputs.codium.lib.${system}) extensions settingsNix extensionsCommon;
          inherit (inputs.codium.lib.${system}) writeSettingsJSON mkCodium;

          scripts = mkShellApps {
            default = {
              text = "npm run quick-start";
              runtimeInputs = [ pursPkgs.nodejs_18 ];
              description = "Run front";
            };
            buildGHPages = {
              text = ''npm run build:gh-pages'';
              runtimeInputs = [ pursPkgs.nodejs_18 ];
              description = "Build GitHub Pages";
            };
          };

          tools = with pursPkgs; [
            purescript
            nodejs_18
            spago
            dhall-lsp-server
            purescript-language-server
            purs-tidy
          ];

          packages = {
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) purescript; }; };
            writeSettings = writeSettingsJSON settingsNix;
          } // scripts;

          devShells.default = mkShell {
            packages = tools;
            commands =
              mkCommands "tools" tools ++
              mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; } ++
              mkRunCommands "scripts" { inherit (packages) default buildGHPages; }
            ;
          };
        in
        {
          inherit packages devShells;
        });
    in
    outputs;

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}

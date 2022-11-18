{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    flake-tools.url = github:br4ch1st0chr0n3/flakes?dir=flake-tools;
    my-codium.url = github:br4ch1st0chr0n3/flakes?dir=codium;
    my-devshell.url = github:br4ch1st0chr0n3/flakes?dir=devshell;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    vscode-extensions_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/vscode-extensions;
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    haskell-tools.url = github:br4ch1st0chr0n3/flakes?dir=language-tools/haskell;
    purescript-tools.url = github:br4ch1st0chr0n3/flakes?dir=language-tools/purescript;
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , flake-tools
    , my-codium
    , vscode-extensions
    , drv-tools
    , haskell-tools
    , purescript-tools
    , my-devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (my-codium.functions.${system})
        writeSettingsJSON
        mkCodium
        ;
      inherit (drv-tools.functions.${system})
        toList
        mkBin
        mkShellApps
        mkShellApp
        ;
      inherit (flake-tools.functions.${system})
        mkFlakesTools
        ;
      inherit (my-codium.configs.${system})
        extensions
        settingsNix
        ;
      inherit (haskell-tools.functions.${system})
        toolsGHC
        ;
      inherit (toolsGHC ghcVersion)
        stack
        hls
        ;
      haskellTools = builtins.attrValues haskell-tools.toolSets.${system}.shellTools;
      pursTools = builtins.attrValues purescript-tools.toolSets.${system}.shellTools;

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown purescript; };
        runtimeDependencies = [ stack hls pursTools haskellTools ];
      };

      writeSettings = writeSettingsJSON settingsNix;

      devshell = my-devshell.devshell.${system};
      scripts = mkShellApps {
        back = {
          text = "cd back && nix run";
        };
        front = {
          text = "cd front && nix run";
        };
      };
      flakesTools = mkFlakesTools [ "." ];
    in
    {
      packages = {
        pushToCachix = flakesTools.pushToCachix;
        updateLocks = flakesTools.update;
      } // scripts;

      devShells.default = devshell.mkShell {
        packages = builtins.attrValues (scripts // { inherit codium writeSettings; });
        commands = [
          {
            name = "codium, ${writeSettings.name}";
          }
          {
            name = "back, front";
          }
        ];
      };
    });

  nixConfig = {
    extra-substituters = [
      https://haskell-language-server.cachix.org
      https://nix-community.cachix.org
      https://hydra.iohk.io
      https://br4ch1st0chr0n3.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU=
    ];
  };
}

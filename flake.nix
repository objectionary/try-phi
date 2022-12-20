{
  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    flakes-tools.url = github:deemp/flakes?dir=flakes-tools;
    my-codium.url = github:deemp/flakes?dir=codium;
    my-devshell.url = github:deemp/flakes?dir=devshell;
    drv-tools.url = github:deemp/flakes?dir=drv-tools;
    vscode-extensions_.url = github:deemp/flakes?dir=source-flake/vscode-extensions;
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    haskell-tools.url = github:deemp/flakes?dir=language-tools/haskell;
    purescript-tools.url = github:deemp/flakes?dir=language-tools/purescript;
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , flakes-tools
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
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkShellApps;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (haskell-tools.functions.${system}) toolsGHC;

      haskellTools = { inherit (toolsGHC ghcVersion) stack hls ghc; };
      pursTools = purescript-tools.toolSets.${system}.shellTools;
      devshell = my-devshell.devshell.${system};
      inherit (my-devshell.functions.${system}) mkCommands;

      writeSettings = writeSettingsJSON settingsNix;
      backDir = "back";
      frontDir = "front";
      scripts =
        let
          dockerHubImage = "try-phi-back";
          host = "127.0.0.1";
          name = "back";
          port = "8082";
          result = "result";
          tag = "latest";
          username = "deemp";
        in
        mkShellApps {
          back = {
            text = "cd ${backDir} && nix run";
            description = "run backend";
          };
          front = {
            text = "cd ${frontDir} && nix run";
            description = "run frontend";
          };
          backDockerBuild =
            {
              text = ''
                nix build -o ${result} ./${backDir}#images.${system}.${name}
                docker load < ${result}
              '';
              runtimeInputs = [ pkgs.docker ];
              description = "nix build an image and load it to docker";
            };
          backDockerRun =
            {
              text = "docker run -p ${host}:${port}:${port} ${name}:${tag} ${name}";
              runtimeInputs = [ pkgs.docker ];
              description = "run ${name} in a docker container";
            };
          backDockerPush =
            {
              text = ''
                docker tag ${name}:${tag} ${username}/${dockerHubImage}:${tag}
                docker push ${username}/${dockerHubImage}:${tag}
              '';
              runtimeInputs = [ pkgs.docker ];
              description = "Push ${name} to Docker Hub";
            };
        };
      codiumTools = builtins.attrValues (
        scripts // {
          inherit (pkgs) heroku;
          inherit (haskellTools) stack ghc;
          inherit (pursTools)
            nodejs-16_x
            purescript
            spago
            ;
          inherit writeSettings;
        }
      );
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown purescript; };
        runtimeDependencies =
          codiumTools ++
          (builtins.attrValues {
            inherit (pursTools) dhall-lsp-server purescript-language-server purs-tidy;
            inherit (haskellTools) hls;
          });
      };
      flakesTools = mkFlakesTools [ "front" "back" "." ];
      tools = codiumTools ++ [ codium ];
    in
    {
      packages = {
        pushToCachix = flakesTools.pushToCachix;
        updateLocks = flakesTools.updateLocks;
      } // scripts;

      devShells.default = devshell.mkShell {
        packages = tools;
        commands = mkCommands "tools" tools;
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
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

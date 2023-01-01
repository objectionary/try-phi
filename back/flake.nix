{
  description = "Try-phi back end";
  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = github:deemp/flakes?dir=language-tools/haskell;
    devshell.url = github:deemp/flakes?dir=devshell;
    drv-tools.url = github:deemp/flakes?dir=drv-tools;
    vscode-extensions_.url = github:deemp/flakes?dir=source-flake/vscode-extensions;
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    my-codium.url = github:deemp/flakes?dir=codium;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , haskell-tools
    , devshell
    , drv-tools
    , vscode-extensions
    , my-codium
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (devshell.functions.${system}) mkShell mkCommands;
      inherit (drv-tools.functions.${system}) mkShellApps mkBin;
      inherit (haskell-tools.functions.${system}) haskellTools;
      inherit (my-codium.configs.${system}) extensions settingsNix;

      packageName = "try-phi-back";
      override =
        {
          overrides = self: super: {
            eo-utils = super.callCabal2nix "eo-utils" ./language-utils/eo-utils { };
            phi-utils = super.callCabal2nix "phi-utils" ./language-utils/phi-utils { };
            language-utils = super.callCabal2nix "language-utils" ./language-utils {
              inherit (self) phi-utils eo-utils;
            };
            "${packageName}" =
              let
                back_ = super.callCabal2nix packageName ./. {
                  inherit (self) language-utils phi-utils eo-utils;
                };
              in
              pkgs.haskell.lib.overrideCabal back_
                (_: {
                  librarySystemDepends = [
                    pkgs.zlib
                  ];
                });
          };
        };

      inherit (haskellTools "902" override (ps: [ ps."${packageName}" ps.eo-utils ps.phi-utils ps.language-utils ]) [ ])
        cabal hpack callCabal justStaticExecutable
        callCabal2nix haskellPackages hls implicit-hie;

      exeName = "back";
      back = justStaticExecutable exeName haskellPackages."${packageName}";

      localImageName = "back";
      backImage = pkgs.dockerTools.buildLayeredImage {
        name = localImageName;
        tag = "latest";
        contents = [ back ];
        config.Entrypoint = [ exeName ];
      };

      scripts =
        let
          dockerHubImageName = "try-phi-back";
          herokuAppName = "try-phi-back";
          host = "127.0.0.1";
          port = "8082";
          result = "result";
          tag = "latest";
          username = "deemp";

          scripts1 = mkShellApps {
            dockerBuild = {
              text = ''docker load < ${backImage}'';
              runtimeInputs = [ pkgs.docker ];
              description = "Load an image into docker";
            };
          };

          scripts2 = mkShellApps {
            dockerRun =
              {
                text = ''
                  ${mkBin scripts1.dockerBuild}
                  docker run -p ${host}:${port}:${port} ${localImageName}:${tag}
                '';
                runtimeInputs = [ pkgs.docker ];
                description = "Run ${localImageName} in a docker container";
              };
            releaseOnHeroku =
              {
                text = ''
                  ${mkBin scripts1.dockerBuild}
                  docker login --username=_ --password=$(heroku auth:token) registry.heroku.com
                  docker tag ${localImageName}:${tag} registry.heroku.com/${herokuAppName}/web
                  docker push registry.heroku.com/${herokuAppName}/web
                  heroku container:release web -a ${herokuAppName}
                '';
                runtimeInputs = [ pkgs.docker pkgs.heroku ];
                description = "Release ${herokuAppName} on Heroku";
              };
          };
          scripts3 = {
            writeSettings = writeSettingsJSON {
              inherit (settingsNix) haskell todo-tree files editor gitlens
                git nix-ide workbench markdown-all-in-one markdown-language-features;
            };
          };
        in
        scripts1 // scripts2 // scripts3;
      codiumTools = [ hpack cabal pkgs.heroku implicit-hie ];
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };
      tools = codiumTools ++ [ codium ];
    in
    {
      packages = {
        default = back;
      } // scripts;

      devShells = {
        default = mkShell {
          packages = tools;
          bash.extra = ''export LANG="C.UTF-8";'';
          commands = mkCommands "tools" tools;
        };
      };

      image = backImage;
    });

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

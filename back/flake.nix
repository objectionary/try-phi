{
  description = "Try-phi back end";

  inputs = { };

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = (import ../.).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools devshell codium;
          haskell-tools = flakes.language-tools.haskell;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) writeSettingsJSON mkCodium;
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands;
          inherit (inputs.drv-tools.lib.${system}) mkShellApps getExe;
          inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
          inherit (inputs.codium.lib.${system}) extensions settingsNix extensionsCommon settingsCommonNix;

          packageName = "try-phi-back";
          override =
            {
              overrides = self: super: {
                eo-utils = self.callCabal2nix "eo-utils" ./eo-utils { };
                phi-utils = self.callCabal2nix "phi-utils" ./phi-utils { };
                language-utils = self.callCabal2nix "language-utils" ./language-utils { inherit (self) phi-utils eo-utils; };
                "${packageName}" =
                  pkgs.haskell.lib.overrideCabal
                    (self.callCabal2nix packageName ./${packageName} { inherit (self) language-utils phi-utils eo-utils; })
                    (x: { librarySystemDepends = [ pkgs.zlib ] ++ (x.librarySystemDepends or [ ]); });
              };
            };

          inherit (toolsGHC {
            version = "928";
            inherit override;
            packages = ps: [
              ps.${packageName}
              ps.eo-utils
              ps.phi-utils
              ps.language-utils
            ];
          })
            cabal ghc hpack callCabal justStaticExecutable
            callCabal2nix haskellPackages hls implicit-hie;

          binaryName = "back";

          back = justStaticExecutable {
            package = haskellPackages.${packageName};
            inherit binaryName;
          };

          localImageName = "back";
          backImage = pkgs.dockerTools.buildLayeredImage {
            name = localImageName;
            tag = "latest";
            contents = [ back ];
            config.Entrypoint = [ binaryName ];
          };

          scripts =
            let
              dockerHubImageName = packageName;
              herokuAppName = packageName;
              host = "127.0.0.1";
              port = "8082";
              result = "result";
              tag = "latest";

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
                      ${getExe scripts1.dockerBuild}
                      docker run -p ${host}:${port}:${port} ${localImageName}:${tag}
                    '';
                    runtimeInputs = [ pkgs.docker ];
                    description = "Run ${localImageName} in a docker container";
                  };
                herokuRelease =
                  {
                    text = ''
                      ${getExe scripts1.dockerBuild}
                      docker login --username=_ --password=$(heroku auth:token) registry.heroku.com
                      docker tag ${localImageName}:${tag} registry.heroku.com/${herokuAppName}/web
                      docker push registry.heroku.com/${herokuAppName}/web
                      heroku container:release web -a ${herokuAppName}
                    '';
                    runtimeInputs = [ pkgs.docker pkgs.heroku ];
                    description = "Release ${herokuAppName} on Heroku";
                  };
              };
            in
            scripts1 // scripts2;

          tools = [ hpack cabal pkgs.heroku implicit-hie hls ];

          packages = {
            default = back;

            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell; }; };
          } // scripts;

          devShells = {
            default = mkShell {
              packages = tools;
              bash.extra = ''export LANG="C.UTF-8";'';
              commands =
                mkCommands "tools" tools ++
                mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; } ++
                mkRunCommands "image" { inherit (packages) dockerBuild dockerRun herokuRelease; }
              ;
            };
          };
        in
        {
          inherit packages devShells;

          image = backImage;
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

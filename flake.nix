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
    workflows.url = "github:deemp/flakes?dir=workflows";
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
    , workflows
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkShellApps mkBin;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (haskell-tools.functions.${system}) haskellTools;
      inherit (workflows.functions.${system})
        writeWorkflow run nixCI_ stepsIf expr
        mkAccessors genAttrsId;
      inherit (workflows.configs.${system}) steps os oss nixCI;

      pursTools = purescript-tools.toolSets.${system}.shellTools;
      devshell = my-devshell.devshell.${system};
      inherit (my-devshell.functions.${system}) mkCommands;

      backDir = "back";
      frontDir = "front";
      scripts =
        (mkShellApps {
          back = {
            text = "cd ${backDir} && nix run";
            description = "Run backend";
          };
          front = {
            text = "cd ${frontDir} && nix run";
            description = "Run frontend";
          };
        }) // {
          writeSettings = writeSettingsJSON settingsNix;
          writeWorkflows = writeWorkflow "ci" workflow;
        };

      names = mkAccessors {
        matrix.os = "";
        secrets = genAttrsId [ "GITHUB_TOKEN" "HEROKU_API_KEY" "HEROKU_EMAIL" ];
      };

      workflow =
        let
          job1 = "_1_update_flake_locks";
          job2 = "_2_push_to_cachix";
          job3 = "_3_front";
          job4 = "_4_back";
          herokuAppName = "try-phi-back";
        in
        nixCI // {
          jobs = {
            "${job1}" = {
              name = "Update flake locks";
              runs-on = os.ubuntu-20;
              steps =
                [
                  steps.checkout
                  steps.installNix
                  steps.configGitAsGHActions
                  steps.updateLocksAndCommit
                ];
            };
            "${job2}" = {
              name = "Push to cachix";
              needs = job1;
              strategy.matrix.os = oss;
              runs-on = expr names.matrix.os;
              steps =
                [
                  steps.checkout
                  steps.installNix
                  steps.logInToCachix
                  steps.pushFlakesToCachix
                ];
            };
            "${job3}" =
              let
                dir = "front";
              in
              {
                name = "Publish front";
                needs = job1;
                runs-on = os.ubuntu-20;
                steps = [
                  steps.checkout
                  steps.installNix
                  {
                    name = "Build";
                    run = ''
                      cd ${dir}
                      nix develop -c bash -c '
                        npm run build:gh-pages
                      '
                    '';
                  }
                  {
                    name = "GitHub Pages action";
                    uses = "peaceiris/actions-gh-pages@v3.9.0";
                    "with" = {
                      github_token = expr names.secrets.GITHUB_TOKEN;
                      publish_dir = "./front/docs";
                      force_orphan = true;
                    };
                  }
                ];
              };
            "${job4}" = {
              name = "Release to Heroku";
              needs = job1;
              runs-on = os.ubuntu-20;
              steps =
                [
                  steps.checkout
                  steps.installNix
                  {
                    name = "Log in to Heroku";
                    # TODO switch to AkhileshNS/heroku-deploy
                    # https://github.com/AkhileshNS/heroku-deploy/pull/151
                    uses = "deemp/heroku-deploy@master";
                    "with" = {
                      heroku_api_key = expr names.secrets.HEROKU_API_KEY;
                      heroku_email = expr names.secrets.HEROKU_EMAIL;
                      heroku_app_name = herokuAppName;
                      justlogin = true;
                    };
                  }
                  {
                    name = "Release app on Heroku";
                    run = ''
                      cd ${backDir}
                      nix run .#herokuRelease
                    '';
                  }
                ];
            };
          };
        };

      codiumTools = builtins.attrValues scripts;

      codium = mkCodium {
        extensions = { inherit (extensions) nix misc github markdown; };
        runtimeDependencies =
          codiumTools ++
          (builtins.attrValues {
            inherit (pursTools) dhall-lsp-server purescript-language-server purs-tidy;
            inherit (pursTools) nodejs-16_x purescript spago;
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

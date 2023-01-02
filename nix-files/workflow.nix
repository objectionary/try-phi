{ workflows, backDir, frontDir, name, system }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId;
  inherit (workflows.configs.${system}) steps os oss nixCI;
  job1 = "_1_update_flake_locks";
  job2 = "_2_push_to_cachix";
  job3 = "_3_front";
  job4 = "_4_back";
  herokuAppName = "try-phi-back";
  names = mkAccessors {
    matrix.os = "";
    secrets = genAttrsId [ "GITHUB_TOKEN" "HEROKU_API_KEY" "HEROKU_EMAIL" ];
  };
  workflow =
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
in
writeWorkflow name workflow

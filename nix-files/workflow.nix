{ workflows, backDir, frontDir, name, system }:
let
  inherit (workflows.lib.${system}) writeWorkflow expr mkAccessors genAttrsId run steps os oss nixCI on;
  job1 = "_1_front";
  job2 = "_2_back";
  job3 = "_3_purge_caches";
  herokuAppName = "try-phi-back";
  names = mkAccessors {
    matrix.os = "";
    secrets = genAttrsId [ "GITHUB_TOKEN" "HEROKU_API_KEY" "HEROKU_EMAIL" ];
  };
  back = import ../back;
  front = import ../front;
  workflow =

    (nixCI { }) // {
      jobs = {
        "${job1}" =
          let dir = "front"; in
          {
            name = "Publish front";
            runs-on = os.ubuntu-22;
            steps = [
              steps.checkout
              (steps.installNix { })
              (steps.cacheNix {
                keyJob = "front";
                linuxGCEnabled = true;
                linuxMaxStoreSize = 0;
                macosGCEnabled = true;
                macosMaxStoreSize = 0;
              })
              {
                name = "Build";
                run = run.nixScript { inherit dir; inDir = true; name = front.packages.${system}.buildGHPages.pname; };
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
        "${job2}" = {
          name = "Publish back";
          runs-on = os.ubuntu-22;
          steps =
            [
              steps.checkout
              (steps.installNix { })
              (steps.cacheNix {
                keyJob = "back";
                linuxGCEnabled = true;
                linuxMaxStoreSize = 0;
                macosGCEnabled = true;
                macosMaxStoreSize = 0;
              })
              {
                name = "Log in to Heroku";
                uses = "AkhileshNS/heroku-deploy@master";
                "with" = {
                  heroku_api_key = expr names.secrets.HEROKU_API_KEY;
                  heroku_email = expr names.secrets.HEROKU_EMAIL;
                  heroku_app_name = herokuAppName;
                  justlogin = true;
                };
              }
              {
                name = "Release app on Heroku";
                run = run.nixScript { dir = backDir; inDir = true; name = back.packages.${system}.herokuRelease.pname; };
              }
            ];
        };
        "${job3}" = (nixCI {
          purgeCacheNeeds = [ job1 job2 ];
        }).jobs.purgeCache;
      };
    };
in
writeWorkflow name workflow

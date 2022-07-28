# Try-phi

This is an experimental interpreter for a variant of 𝜑-calculus.
Right now we implement it as a term rewriting system.

It is combined with [EO](https://github.com/objectionary/eo) editor. EO is based on 𝜑-calculus.

## Usage

- The online playground is available [here](https://www.objectionary.com/try-phi/?editor=phi&snippet=%5B%0A%20%20a%20-%3E%203%2C%0A%20%20b%20-%3E%20%5E0.a%0A%5D.b)

## Components

- [Front end](./front/)
- [Back end](./back/)

## Development

- Install dependencies

  - [Back end](./back/README.md#dependencies)
  - [Front end](./back/README.md#dependencies)

- Build the server and front end

  ```sh
  sh build.sh
  ```

- Run the server and front end in separate terminals to see separate logs:

  ```sh
  sh run_server.sh
  sh run_front.sh
  ```

## References

- [Add](https://git-scm.com/book/en/v2/Git-Tools-Submodules) a submodule
- Clone [nested submodules](https://stackoverflow.com/a/6562038)
- Convert a submodule to a folder while preserving its history: [src](https://medium.com/walkme-engineering/how-to-merge-a-git-submodule-into-its-main-repository-d83a215a319c)
- Get info about forks: [gitpop3](https://andremiras.github.io/gitpop3/)
- "[Automating](https://brandonchinn178.github.io/blog/2022/05/19/automating-fourmolu-releases-with-github-actions.html) Fourmolu releases" - use Python for CI
- Build a subdirectory: [buildpack](https://elements.heroku.com/buildpacks/timanovsky/subdir-heroku-buildpack)
- Deploy to Heroku: GH [action](https://github.com/marketplace/actions/deploy-to-heroku?version=v3.12.12)

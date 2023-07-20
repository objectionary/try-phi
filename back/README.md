# Try-phi backend

## Quick start

1. [Install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md)

1. Enter this directory.

    ```sh
    git clone https://github.com/objectionary/try-phi
    cd try-phi/back
    ```

1. Run `back end`.

    ```console
    nix run
    ```

## Development

1. Start a deshell.

    ```console
    nix develop
    ```

1. Build the package

    ```console
    cabal build
    ```

1. (Optionally) Start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. In a `.hs` file, hover over a function. `HLS` should start giving the type info soon.

1. [Troubleshoot](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md) if necessary

## References

- Heroku's [Container Registry & Runtime (Docker Deploys)](https://devcenter.heroku.com/articles/container-registry-and-runtime#dockerfile-commands-and-runtime)

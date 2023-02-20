# Try-phi

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Hits-of-Code](https://hitsofcode.com/github/objectionary/try-phi?branch=main)](https://hitsofcode.com/view/github/objectionary/try-phi?branch=main)
![Lines of code](https://img.shields.io/tokei/lines/github/objectionary/try-phi?style=flat-square)

This is an experimental interpreter for a variant of [𝜑-calculus](https://arxiv.org/abs/2204.07454), the base language of [EO](https://github.com/objectionary/eo).
We implement it as a term rewriting system.

## Usage

- The online playground is available [here](https://www.objectionary.com/try-phi/?editor=phi&snippet=%5B%0A%20%20a%20-%3E%203%2C%0A%20%20b%20-%3E%20%5E0.a%0A%5D.b)

## Components

- [Back end](./back/)
- [Front end](./front/)
- [EO editor](https://github.com/deemp/eo-editor)
- [Phi editor](https://github.com/deemp/phi-editor)

## Quick start

1. [Install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md)

1. Enter the repo.

  ```sh
  git clone https://github.com/objectionary/try-phi
  cd try-phi
  ```

1. Run `back end` and `front end` in separate terminals.

  ```console
  nix run .#back
  nix run .#front
  ```

## Development

1. Allow `direnv` in flake folders.

  ```sh
  direnv allow
  (cd front && direnv allow)
  (cd back && direnv allow && stack build)
  ```

1. Start a deshell.

  ```console
  nix develop
  ```

1. (Optionally) Start `VSCodium`.

  ```console
  nix run .#writeSettings
  nix run .#codium .
  ```

1. In a `.hs` file, hover over a function. `HLS` should start giving the type info soon.

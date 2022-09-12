# Try-phi

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Hits-of-Code](https://hitsofcode.com/github/objectionary/try-phi?branch=main)](https://hitsofcode.com/view/github/objectionary/try-phi?branch=main)
![Lines of code](https://img.shields.io/tokei/lines/github/objectionary/try-phi?style=flat-square)

This is an experimental interpreter for a variant of 𝜑-calculus.
Right now we implement it as a term rewriting system.

It is combined with [EO](https://github.com/objectionary/eo) editor. EO is based on 𝜑-calculus.

## Usage

- The online playground is available [here](https://www.objectionary.com/try-phi/?editor=phi&snippet=%5B%0A%20%20a%20-%3E%203%2C%0A%20%20b%20-%3E%20%5E0.a%0A%5D.b)

## Components

- [Back end](./back/)
- [Front end](./front/)
- [EO editor](https://github.com/br4ch1st0chr0n3/eo-editor)
- [Phi editor](https://github.com/br4ch1st0chr0n3/phi-editor)

## Prerequisits

- Install [Nix](https://nixos.org/download.html) (Single-user installation)
  ```sh
  sh <(curl -L https://nixos.org/nix/install) --no-daemon
  ```

- Enable [flakes](https://nixos.wiki/wiki/Flakes#Permanent)

- Enter the repo
  ```sh
  git clone https://github.com/objectionary/try-phi
  cd try-phi
  ```

## Quick start

- Run back and front in separate terminals
  ```console
  nix develop .#back
  nix develop .#front
  ```

## Development

- Install [direnv](https://nix.dev/tutorials/declarative-and-reproducible-developer-environments#direnv-automatically-activating-the-environment-on-directory-change) - steps 1, 2

- Allow direnv in flake folders
  ```sh
  direnv allow
  (cd front && direnv allow)
  (cd back && direnv allow)
  ```

- Open Codium
  ```console
  nix develop .#codium
  codium .
  ```

- In separate terminals:
  - backend dev
    ```sh
    cd back
    nix run
    ```
  - front
    ```sh
    cd front
    direnv allow
    npm run dev
    ```
    - Or run any other command from [package.json](package.json)

- In case of problems reload the window (`Ctrl`+`Shift`+`P` -> `Developer: Reload window`) and repeat previous commands to start the server and the client

<!-- TODO https://code.visualstudio.com/docs/editor/tasks#_compound-tasks

start server and client in different terminals -->
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

## Prerequisites

- Install [Nix](https://nixos.org/download.html) (Single-user installation)

  ```sh
  sh <(curl -L https://nixos.org/nix/install) --no-daemon
  ```

- Enable [flakes](https://nixos.wiki/wiki/flakes/8ee5d35e592860636adb57cee3e27c98de04202a#Permanent)

- Enter the repo

  ```sh
  git clone https://github.com/objectionary/try-phi
  cd try-phi
  ```

## Quick start

- Run back and front in separate terminals

  ```console
  nix run .#back
  nix run .#front
  ```

## Development

- Install [direnv](https://nix.dev/tutorials/declarative-and-reproducible-developer-environments#direnv-automatically-activating-the-environment-on-directory-change) - steps 1, 2

- [Enable flakes](https://nixos.wiki/wiki/flakes/8ee5d35e592860636adb57cee3e27c98de04202a#Enable_flakes)

- Allow direnv in flake folders

  ```sh
  direnv allow
  (cd front && direnv allow)
  (cd back && direnv allow && stack build)
  ```

- Open Codium

  ```console
  nix develop
  codium .
  ```

- Hotkey for `Command palette` - `Ctrl` (`Cmd`) + `Shift` + `P`

- Start app - a browser window should open
  - Command palette -> `Tasks: Run Task` -> `Run app`

- Terminate app
  - Command palette -> `Tasks: Terminate Task` -> `All Running Tasks`>

- In case of problems reload the window (`Ctrl` + `Shift` + `P` -> `Developer: Reload window`) and repeat previous commands to start the server and the client

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

## Development

- Install [Nix](https://nixos.org/download.html) (Single-user installation)
  ```sh
  sh <(curl -L https://nixos.org/nix/install) --no-daemon
  ```

- Install [direnv](https://nix.dev/tutorials/declarative-and-reproducible-developer-environments#direnv-automatically-activating-the-environment-on-directory-change) - steps 1, 2

- For [VS Code](https://code.visualstudio.com/), install extensions
  ```sh
  code --install-extension mkhl.direnv --install-extension haskell.haskell --install-extension nwolverson.ide-purescript
  ```

- In separate terminals:
  - backend dev
    ```sh
    cd back
    # for the first time
    direnv allow
    # start the server
    nix run
    ```
  - front
    ```sh
    cd front
    # for the first time
    direnv allow
    # open app in a browser
    npm run dev
    # or any other commands from package.json
    ```

- For Haskell, your shell will have [haskell-language-server](https://github.com/haskell/haskell-language-server)
- For Purescript - [purescript-language-server](https://github.com/nwolverson/purescript-language-server)
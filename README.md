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

- Install dependencies

  - [Back end](./back/README.md#dependencies)
  - [Front end](./front/README.md#dependencies)

- Build the server and front end

  ```sh
  sh build.sh
  ```

- Run the server and front end in separate terminals to see separate logs:

  ```sh
  sh run_back.sh
  sh run_front.sh
  ```